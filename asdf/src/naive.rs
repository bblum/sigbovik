use crate::sim::{
    BisectAttempt,
    BisectStrategy,
    SimulationState,
};

// TODO handle contradiction -- keep a history of past "known good" his and los
pub struct NaiveBinarySearch {
    lo: usize,
    hi: usize,
}

impl NaiveBinarySearch {
    #[cfg_attr(not(test), allow(unused))]
    pub fn new(s: &SimulationState) -> Self {
        Self {
            lo: 0,
            hi: s.pdf.len() - 1,
        }
    }
}

impl BisectStrategy for NaiveBinarySearch {
    fn select_commit(&mut self, _s: &SimulationState) -> usize {
        (self.lo + self.hi) / 2
    }

    fn notify_result(&mut self, result: BisectAttempt) {
        if result.bug_repros {
            self.hi = result.commit;
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
        // we'll say the bug is secretly at 5
        // we just need this to make `b` initialize its `hi`
        let s = SimulationState::new(8, 0.0);
        let mut b = NaiveBinarySearch::new(&s);

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

    // TODO: test contradiction
}
