use rand::Rng;

use crate::sim::*;

pub enum RandomMode { Uniformly, WeightedByCDF }

pub struct ChooseRandomly(RandomMode);

impl ChooseRandomly {
    pub fn new(_: &SimulationState, how: RandomMode) -> Self { Self(how) }
}

impl BisectStrategy for ChooseRandomly {
    fn name(&self) -> String {
        match self.0 {
            RandomMode::Uniformly => "rand_uniform".to_string(),
            RandomMode::WeightedByCDF => "rand_cdf".to_string(),
        }
    }

    fn select_commit(&mut self, s: &SimulationState) -> usize {
        assert!(!s.bug_found());
        match self.0 {
            RandomMode::Uniformly => {
                rand::thread_rng().gen_range(s.lower_bound, s.upper_bound-1)
            },
            RandomMode::WeightedByCDF => {
                // don't just say 1.0, and get got by floating point
                let min_cdf = s.pdf[s.lower_bound];
                let max_cdf = s.cdf.last().expect("empty cdf");

                let p = rand::thread_rng().gen_range(min_cdf, max_cdf);
                let result = s.last_cdf_index_not_exceeding(p);
                assert!(s.in_range(result));
                assert!(result < s.upper_bound - 1);
                result
            },
        }
    }

    fn notify_result(&mut self, _: BisectAttempt) {}
}
