use crate::sim::*;

// searches for the commit at which bisecting yields minimum expected entropy
pub struct MinExpectedEntropy;

impl MinExpectedEntropy {
    pub fn new(_: &SimulationState) -> Self { Self }
}

impl BisectStrategy for MinExpectedEntropy {
    fn select_commit(&mut self, s: &SimulationState) -> usize {
        s.min_expected_entropy_binary_search()
    }

    fn notify_result(&mut self, _: BisectAttempt) {}
}
