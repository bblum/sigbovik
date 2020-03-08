use crate::sim::{
    BisectStrategy,
    SimulationState,
};

pub struct NaiveBinarySearch {
}

impl NaiveBinarySearch {
    pub fn new() -> Self {
        Self { }
    }
}

impl BisectStrategy for NaiveBinarySearch {
    fn select_commit(&mut self, _state: &SimulationState) -> usize {
        unimplemented!()
    }

    fn notify_result(&mut self, _commit: usize, _bug_repros: bool) {
        unimplemented!()
    }
}
