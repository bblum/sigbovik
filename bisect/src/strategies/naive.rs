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

    #[test]
    fn test_naive_basic() {
        // we'll say the bug is secretly at 5
        // we just need this to make `b` initialize its `hi`
        let s = SimulationState::new(8, 0.0);
        let mut b = NaiveBinarySearch::new(&s, ConfusedHumanMode::ForgetEverything);

        assert_eq!(b.select_commit(&s), 3);
        b.notify_result(BisectAttempt { commit: 3, bug_repros: false });
        assert_eq!(b.lo, 4);
        assert_eq!(b.hi, 7);

        assert_eq!(b.select_commit(&s), 5);
        b.notify_result(BisectAttempt { commit: 5, bug_repros: true });
        assert_eq!(b.lo, 4);
        assert_eq!(b.hi, 5);

        assert_eq!(b.select_commit(&s), 4);
        b.notify_result(BisectAttempt { commit: 4, bug_repros: false });
        assert_eq!(b.lo, 5);
        assert_eq!(b.hi, 5);
    }

    fn setup_contradiction_test(how: ConfusedHumanMode) -> (SimulationState, NaiveBinarySearch) {
        let s = SimulationState::new(8, 0.0);
        let mut b = NaiveBinarySearch::new(&s, how);

        // push the human to the end of the range with some negative flakes.
        assert_eq!(b.select_commit(&s), 3);
        b.notify_result(BisectAttempt { commit: 3, bug_repros: false });
        assert_eq!(b.lo, 4);
        assert_eq!(b.hi, 7);

        assert_eq!(b.select_commit(&s), 5);
        b.notify_result(BisectAttempt { commit: 5, bug_repros: false });
        assert_eq!(b.lo, 6);
        assert_eq!(b.hi, 7);

        assert_eq!(b.select_commit(&s), 6);
        b.notify_result(BisectAttempt { commit: 6, bug_repros: false });
        assert_eq!(b.lo, 7);
        assert_eq!(b.hi, 7);

        // now, asked to confirm, the human just probes to increase confidence on 7
        assert_eq!(b.select_commit(&s), 6);
        b.notify_result(BisectAttempt { commit: 6, bug_repros: true });
        // at this point they realize something is up.
        // what they do now depends on the strategy.
        (s, b)
    }

    #[test]
    fn test_naive_contradiction_forget() {
        let (_, b) = setup_contradiction_test(ConfusedHumanMode::ForgetEverything);
        assert_eq!(b.lo, 0);
        assert_eq!(b.hi, 6);

        // it isn't interesting to test further since there's no persistent state
    }

    #[test]
    fn test_naive_contradiction_rewind() {
        let (s, mut b) = setup_contradiction_test(ConfusedHumanMode::UsePreviousLow);
        assert_eq!(b.lo, 6);
        assert_eq!(b.hi, 6);

        // the human, who now has perfect memory, walks backwards in time
        assert_eq!(b.select_commit(&s), 5);
        b.notify_result(BisectAttempt { commit: 5, bug_repros: true });
        assert_eq!(b.lo, 4);
        assert_eq!(b.hi, 5);

        assert_eq!(b.select_commit(&s), 4);
        b.notify_result(BisectAttempt { commit: 4, bug_repros: true });
        assert_eq!(b.lo, 4);
        assert_eq!(b.hi, 4);

        assert_eq!(b.select_commit(&s), 3);
        b.notify_result(BisectAttempt { commit: 3, bug_repros: true });
        assert_eq!(b.lo, 0);
        assert_eq!(b.hi, 3);

        // now they are truly in the twilight zone!
        assert_eq!(b.memory, Some(vec![]));
    }
}
