use rand::Rng;

use crate::sim::*;

pub trait SelectRandom {
    fn select_random(s: &SimulationState, up_to: usize) -> usize;
}

// aggressively avoiding code reuse
pub enum Uniformly {}
pub enum WeightedByCDF {}

pub struct ChooseRandomly<T> {
    earliest_known_buggy_commit: usize,
    _how: std::marker::PhantomData<T>,
}

impl<T> ChooseRandomly<T> {
    #[cfg_attr(not(test), allow(unused))]
    pub fn new(s: &SimulationState) -> Self {
        Self {
            earliest_known_buggy_commit: s.pdf.len() - 1,
            _how: std::marker::PhantomData,
        }
    }
}

impl SelectRandom for Uniformly {
    fn select_random(_: &SimulationState, up_to: usize) -> usize {
        rand::thread_rng().gen_range(0, up_to)
    }
}

impl SelectRandom for WeightedByCDF {
    fn select_random(s: &SimulationState, up_to: usize) -> usize {
        let p = rand::thread_rng().gen_range(0.0, s.cdf.last().expect("empty cdf"));
        let result = s.last_cdf_index_not_exceeding(p);
        assert!(result < up_to, "would test known buggy commit");
        result
    }
}

impl<T: SelectRandom> BisectStrategy for ChooseRandomly<T> {
    fn select_commit(&mut self, s: &SimulationState) -> usize {
        assert_ne!(self.earliest_known_buggy_commit, 0, "bug already found");
        <T as SelectRandom>::select_random(s, self.earliest_known_buggy_commit)
    }

    fn notify_result(&mut self, result: BisectAttempt) {
        if result.bug_repros {
            self.earliest_known_buggy_commit = result.commit;
        }
    }
}
