use crate::sim::*;

#[derive(Debug)]
pub struct NaiveBinarySearch {
    lo: usize,
    hi: usize,
    memory: Option<Vec<usize>>,
}

pub enum ConfusedHumanMode { ForgetEverything, UsePreviousLow }

impl NaiveBinarySearch {
    pub fn new(s: &SimulationState, how: ConfusedHumanMode) -> Self {
        Self {
            lo: 0,
            hi: s.pdf.len() - 1,
            memory: match how {
                ConfusedHumanMode::ForgetEverything => None,
                ConfusedHumanMode::UsePreviousLow => Some(vec![]),
            },
        }
    }
}

impl BisectStrategy for NaiveBinarySearch {
    fn select_commit(&mut self, _s: &SimulationState) -> usize {
        let res = if self.lo == self.hi {
            assert_ne!(self.lo, 0, "ok, but the bug has to be at 0 now yo");
            // human thinks they found the bug. keep poking to increase confidence.
            self.lo - 1
        } else {
            (self.lo + self.hi) / 2
        };
        println!("asdf selected {}; self state {:?}", res, self);
        res
    }

    fn notify_result(&mut self, result: BisectAttempt) {
        if result.bug_repros {
            self.hi = result.commit;
            if self.hi < self.lo {
                assert_eq!(self.hi, self.lo - 1, "whoa, got too excited there");
                // human realizes theyre in the twilight zone.
                // the current lower bound was a lie! discard it and rewind a lil.
                self.lo = self.memory.as_mut().map_or(0, |previous_lows| {
                    previous_lows.pop().expect("impossible; bug before 0")
                });
                assert!(self.lo <= self.hi, "too many previous lows");
            }
        } else {
            assert!(result.commit + 1 >= self.lo, "whoa, got too excited there");
            if result.commit + 1 > self.lo {
                if let Some(ref mut previous_lows) = self.memory {
                    previous_lows.push(self.lo);
                }
            }
            self.lo = result.commit + 1;
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
        let s = SimulationState::new(8, 0.0);
        let mut b = NaiveBinarySearch::new(&s, how);

        // push the human to the end of the range with some negative flakes.
        b.probe_assert(&s, 3, false, 4, 7);
        b.probe_assert(&s, 5, false, 6, 7);
        b.probe_assert(&s, 6, false, 7, 7);

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
        assert!(b.memory.unwrap().is_empty());
    }
}
