use crate::sim::*;

// it just finds where the cdf crosses 0.5 or whatever
pub struct CdfBisect {
    bisect_point: f64,
    earliest_bug_seen: usize,
}

impl CdfBisect {
    pub fn new(s: &SimulationState, bisect_point: f64) -> Self {
        Self {
            bisect_point,
            // never test the last commit
            earliest_bug_seen: s.pdf.len() - 1,
        }
    }
}

impl BisectStrategy for CdfBisect {
    fn name(&self) -> String { format!("cdf_{}", self.bisect_point) }

    fn select_commit(&mut self, s: &SimulationState) -> usize {
        let res = s.first_cdf_index_eq_or_greater(self.bisect_point);
        if res == self.earliest_bug_seen {
            assert!(res > 0, "saw bug on commit 0 -> should be 100% already");
            res - 1
        } else {
            res
        }
    }

    fn notify_result(&mut self, result: BisectAttempt) {
        if result.bug_repros {
            if result.commit < self.earliest_bug_seen {
                self.earliest_bug_seen = result.commit;
            } else if result.commit == self.earliest_bug_seen {
                panic!("tested known buggy commit twice");
            }
        }
    }
}
