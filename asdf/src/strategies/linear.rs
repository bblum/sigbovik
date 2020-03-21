use crate::sim::*;

// it just finds where the cdf crosses 0.5 or whatever
pub struct LinearSearch {
    earliest_known_buggy_commit: usize,
}

impl LinearSearch {
    #[cfg_attr(not(test), allow(unused))]
    pub fn new(s: &SimulationState) -> Self {
        Self {
            earliest_known_buggy_commit: s.pdf.len() - 1,
        }
    }
}

impl BisectStrategy for LinearSearch {
    fn select_commit(&mut self, _: &SimulationState) -> usize {
        assert_ne!(self.earliest_known_buggy_commit, 0, "bug already found");
        self.earliest_known_buggy_commit - 1
    }

    fn notify_result(&mut self, result: BisectAttempt) {
        assert_eq!(result.commit, self.earliest_known_buggy_commit - 1);
        if result.bug_repros {
            self.earliest_known_buggy_commit = result.commit;
        }
    }
}
