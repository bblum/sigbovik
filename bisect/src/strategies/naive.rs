use crate::sim::*;

pub struct NaiveBinarySearch {
    lo: usize,
    hi: usize,
    memory: Memory,
}

struct Retry {
    commit: usize,
    remaining_retries: usize,
}

enum Memory {
    PreviousLows(Vec<usize>),
    Retry {
        max_retries: usize,
        active_retry: Option<Retry>,
    },
    BlissfulIgnorance,
}

// mistrustful(3) == will run each passing commit 4 times before moving on
// mistrustful(0) is equivalent to forgeteverything
pub enum ConfusedHumanMode {
    ForgetEverything,
    UsePreviousLow,
    Mistrustful(usize)
}

impl NaiveBinarySearch {
    pub fn new(s: &SimulationState, how: ConfusedHumanMode) -> Self {
        Self {
            lo: 0,
            hi: s.pdf.len() - 1,
            memory: match how {
                ConfusedHumanMode::ForgetEverything => Memory::BlissfulIgnorance,
                ConfusedHumanMode::UsePreviousLow => Memory::PreviousLows(vec![]),
                ConfusedHumanMode::Mistrustful(max_retries) => {
                    assert_ne!(max_retries, 0, "just use ForgetEverything in this case");
                    if s.false_negative_rate == 0.0 {
                        // don't bother retrying commits in this mode, obviously
                        Memory::BlissfulIgnorance
                    } else {
                        Memory::Retry { max_retries, active_retry: None }
                    }
                },
            },
        }
    }
}

impl BisectStrategy for NaiveBinarySearch {
    fn name(&self) -> String {
        match self.memory {
            Memory::PreviousLows(_) => "naive_memory".to_string(),
            Memory::Retry { max_retries, .. } => format!("naive_mistrustful_{}", max_retries),
            Memory::BlissfulIgnorance => "naive_forgetful".to_string(),
        }
    }

    fn select_commit(&mut self, _s: &SimulationState) -> usize {
        if let Memory::Retry { active_retry: Some(ref retry), .. } = self.memory {
            retry.commit
        } else if self.lo == self.hi {
            assert_ne!(self.lo, 0, "ok, but the bug has to be at 0 now yo");
            // human thinks they found the bug. keep poking to increase confidence.
            self.lo - 1
        } else {
            (self.lo + self.hi) / 2
        }
    }

    fn notify_result(&mut self, result: BisectAttempt) {
        if result.bug_repros {
            self.hi = result.commit;
            if self.hi < self.lo {
                assert_eq!(self.hi, self.lo - 1, "whoa, got too excited there");
                // human realizes theyre in the twilight zone.
                // the current lower bound was a lie! discard it and rewind a lil.
                self.lo = match self.memory {
                    Memory::Retry { .. } | Memory::BlissfulIgnorance => 0,
                    Memory::PreviousLows(ref mut previous_lows) => {
                        previous_lows.pop().expect("impossible, bug before 0")
                    },
                };
                assert!(self.lo <= self.hi, "too many duplicate previous lows");
            }
            if let Memory::Retry { ref mut active_retry, .. } = self.memory {
                *active_retry = None;
            }
        } else {
            assert!(result.commit + 1 >= self.lo, "whoa, got too excited there");
            match self.memory {
                Memory::PreviousLows(ref mut previous_lows) => {
                    if result.commit + 1 > self.lo {
                        previous_lows.push(self.lo);
                        self.lo = result.commit + 1;
                    }
                },
                Memory::Retry { ref mut active_retry, max_retries } => {
                    if let Some(ref mut retry) = active_retry {
                        assert_eq!(retry.commit, result.commit, "deviated");
                        assert!(retry.remaining_retries > 0);
                        retry.remaining_retries -= 1;
                        if retry.remaining_retries == 0 {
                            drop(retry);
                            *active_retry = None;
                            self.lo = result.commit + 1;
                        }
                    } else {
                        *active_retry = Some(Retry {
                            commit: result.commit,
                            remaining_retries: max_retries,
                        });
                    }
                },
                Memory::BlissfulIgnorance => {
                    self.lo = result.commit + 1;
                },
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl NaiveBinarySearch {
        fn probe_assert(&mut self, s: &SimulationState, commit: usize,
                        bug_repros: bool, expected_lo: usize, expected_hi: usize)
        {
            assert_eq!(self.select_commit(s), commit);
            self.notify_result(BisectAttempt { commit, bug_repros });
            assert_eq!(self.lo, expected_lo);
            assert_eq!(self.hi, expected_hi);
        }

        fn probe_assert_retry(&mut self, s: &SimulationState, commit: usize,
                              retries: usize, expected_lo: usize, expected_hi: usize)
        {
            for _ in 0..retries {
                // self.lo doesn't update until retries runs out
                self.probe_assert(s, commit, false, self.lo, expected_hi);
            }
            self.probe_assert(s, commit, false, expected_lo, expected_hi);
        }

        fn unwrap_previous_lows(&self) -> &Vec<usize> {
            if let Memory::PreviousLows(ref memory) = self.memory {
                memory
            } else {
                panic!("expected previous lows mode here");
            }
        }

        fn unwrap_active_retry(&self) -> Option<&Retry> {
            if let Memory::Retry { ref active_retry, .. } = self.memory {
                active_retry.as_ref()
            } else {
                panic!("expected mistrustful mode here");
            }
        }
    }

    #[test]
    fn test_naive_basic() {
        // we'll say the bug is secretly at 5
        // we just need this to make `b` initialize its `hi`
        let s = SimulationState::new(8, 0.0);
        let mut b = NaiveBinarySearch::new(&s, ConfusedHumanMode::ForgetEverything);

        b.probe_assert(&s, 3, false, 4, 7);
        b.probe_assert(&s, 5, true,  4, 5);
        b.probe_assert(&s, 4, false, 5, 5);
    }

    fn setup_contradiction_test(how: ConfusedHumanMode)
        -> (SimulationState, NaiveBinarySearch)
    {
        let retries = if let ConfusedHumanMode::Mistrustful(n) = how { n } else { 0 };
        // need to have nonzero fnrate here for mistrustful to do its retries
        let s = SimulationState::new(8, 0.5);
        let mut b = NaiveBinarySearch::new(&s, how);

        // push the human to the end of the range with some negative flakes.
        b.probe_assert_retry(&s, 3, retries, 4, 7);
        b.probe_assert_retry(&s, 5, retries, 6, 7);
        b.probe_assert_retry(&s, 6, retries, 7, 7);

        (s, b)
    }

    #[test]
    fn test_naive_contradiction_forget() {
        let (s, mut b) = setup_contradiction_test(ConfusedHumanMode::ForgetEverything);
        // now, asked to confirm, the human probes 6 to increase confidence on 7
        // they realize something is up, and reset their lower bound to start over.
        b.probe_assert(&s, 6, true, 0, 6);

        // it isn't interesting to test further since there's no persistent state
    }

    #[test]
    fn test_naive_contradiction_rewind() {
        let (s, mut b) = setup_contradiction_test(ConfusedHumanMode::UsePreviousLow);
        b.probe_assert(&s, 6, true, 6, 6);

        // the human, who now has perfect memory, walks backwards in time
        b.probe_assert(&s, 5, true, 4, 5);
        b.probe_assert(&s, 4, true, 4, 4);
        b.probe_assert(&s, 3, true, 0, 3);

        // now they are truly in the twilight zone!
        assert!(b.unwrap_previous_lows().is_empty());
    }

    #[test]
    fn test_naive_contradiction_mistrustful() {
        for retries in 1..8 {
            let (s, mut b) = setup_contradiction_test(ConfusedHumanMode::Mistrustful(retries));
            // should behave the same way as forgetful mode when the bug does repro
            b.probe_assert(&s, 6, true, 0, 6);

            // let's say the bug is at 0 but we get some flakes
            // human should not update their low quite yet
            b.probe_assert(&s, 3, false, 0, 6);
            let retry = b.unwrap_active_retry().expect("retry should exist");
            assert!(retry.remaining_retries == retries);

            // now show them the bug
            b.probe_assert(&s, 3, true, 0, 3);
            // they should forget about their active retry
            assert!(b.unwrap_active_retry().is_none());
            // etc.
        }
    }
}
