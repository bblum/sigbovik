use crate::sim::*;

// searches for the commit at which bisecting yields minimum expected entropy
pub struct MinExpectedEntropy {
    earliest_bug_seen: usize,
}

// how to search for the point of min expected entropy?
pub enum How { Linear, Binary }

impl MinExpectedEntropy {
    pub fn new(s: &SimulationState) -> Self {
        Self {
            earliest_bug_seen: s.pdf.len() - 1,
        }
    }
}

impl BisectStrategy for MinExpectedEntropy {
    fn select_commit(&mut self, s: &SimulationState) -> usize {
        // can't use min_by_key b/c f64 !: Ord >_<
        let mut best = None;
        for i in 0..self.earliest_bug_seen {
            let expected_entropy = s.hypothetical_expected_entropy(i);
            match best {
                Some((_, e2)) if e2 < expected_entropy => {},
                _ => {
                    best = Some((i, expected_entropy));
                },
            }
        }
        best.expect("no entropies hypothesized").0
    }

    fn notify_result(&mut self, result: BisectAttempt) {
        assert!(result.commit < self.earliest_bug_seen);
        if result.bug_repros {
            self.earliest_bug_seen = result.commit;
        }
    }
}
