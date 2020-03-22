use rand::Rng;

use crate::sim::*;

#[cfg_attr(not(test), allow(unused))]
pub enum How { Uniformly, WeightedByCDF }

pub struct ChooseRandomly {
    earliest_known_buggy_commit: usize,
    how: How,
}

impl ChooseRandomly {
    #[cfg_attr(not(test), allow(unused))]
    pub fn new(s: &SimulationState, how: How) -> Self {
        Self {
            earliest_known_buggy_commit: s.pdf.len() - 1,
            how,
        }
    }
}

impl BisectStrategy for ChooseRandomly {
    fn select_commit(&mut self, s: &SimulationState) -> usize {
        assert_ne!(self.earliest_known_buggy_commit, 0, "bug already found");
        match self.how {
            How::Uniformly => {
                rand::thread_rng().gen_range(0, self.earliest_known_buggy_commit)
            },
            How::WeightedByCDF => {
                // don't just say 1.0, and get got by floating point
                let max_cdf = s.cdf.last().expect("empty cdf");

                let p = rand::thread_rng().gen_range(0.0, max_cdf);
                let result = s.last_cdf_index_not_exceeding(p);
                assert!(result < self.earliest_known_buggy_commit);
                result
            },
        }
    }

    fn notify_result(&mut self, result: BisectAttempt) {
        if result.bug_repros {
            self.earliest_known_buggy_commit = result.commit;
        }
    }
}
