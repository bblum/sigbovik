use crate::sim::{
    BisectAttempt,
    BisectStrategy,
    SimulationState,
};

// TODO handle contradiction -- keep a history of past "known good" his and los
pub struct NaiveBinarySearch {
    lo: usize,
    hi: Option<usize>,
}

impl NaiveBinarySearch {
    pub fn new() -> Self {
        Self { lo: 0, hi: None }
    }
}

impl BisectStrategy for NaiveBinarySearch {
    fn select_commit(&mut self, state: &SimulationState) -> usize {
        if self.hi == None {
            self.hi = Some(state.pdf.len() - 1);
        }
        (self.lo + self.hi.unwrap()) / 2
    }

    fn notify_result(&mut self, result: BisectAttempt) {
        if result.bug_repros {
            self.hi = Some(result.commit);
        } else {
            self.lo = result.commit + 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_naive_basic() {
        let mut b = NaiveBinarySearch::new();
        // we'll say the bug is secretly at 5
        // we just need this to make `b` initialize its `hi`
        let s = SimulationState::new(8, 0.0);

        assert_eq!(b.select_commit(&s), 3);
        b.notify_result(BisectAttempt { commit: 3, bug_repros: false });
        assert_eq!(b.lo, 4);
        assert_eq!(b.hi, Some(7));

        assert_eq!(b.select_commit(&s), 5);
        b.notify_result(BisectAttempt { commit: 5, bug_repros: true });
        assert_eq!(b.lo, 4);
        assert_eq!(b.hi, Some(5));

        assert_eq!(b.select_commit(&s), 4);
        b.notify_result(BisectAttempt { commit: 4, bug_repros: false });
        assert_eq!(b.lo, 5);
        assert_eq!(b.hi, Some(5));
    }

    // TODO: test contradiction
}
