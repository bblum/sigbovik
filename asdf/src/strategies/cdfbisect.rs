use crate::sim::{
    BisectAttempt,
    BisectStrategy,
    SimulationState,
};

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
    fn select_commit(&mut self, s: &SimulationState) -> usize {
        let res = find_first_eq_or_greater(&s.cdf, self.bisect_point);
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

#[cfg_attr(not(test), allow(unused))]
fn find_last_not_exceeding<T: PartialOrd>(v: &[T], target_value: T) -> usize {
    assert!(v.len() > 0);
    let mut lo = 0;
    let mut hi = v.len();
    loop {
        let mid = (lo + hi) / 2;
        if v[mid] > target_value {
            hi = mid;
        } else {
            lo = mid;
        }
        if lo == v.len() - 1 || lo == hi || lo == hi - 1 && v[hi] > target_value {
            return lo;
        }
    }
}

#[cfg_attr(not(test), allow(unused))]
fn find_first_eq_or_greater<T: PartialOrd>(v: &[T], target_value: T) -> usize {
    assert!(v.len() > 0);
    let mut lo = 0;
    let mut hi = v.len();
    loop {
        let mid = (lo + hi) / 2;
        if v[mid] < target_value {
            lo = mid;
        } else {
            hi = mid;
        }
        if hi == lo || hi == lo + 1 && v[lo] < target_value {
            return hi;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_reverse_lookup() {
        assert_eq!(find_last_not_exceeding(&[1, 3, 5], 0), 0);
        assert_eq!(find_last_not_exceeding(&[1, 3, 5], 1), 0);
        assert_eq!(find_last_not_exceeding(&[1, 3, 5], 2), 0);
        assert_eq!(find_last_not_exceeding(&[1, 3, 5], 3), 1);
        assert_eq!(find_last_not_exceeding(&[1, 3, 5], 4), 1);
        assert_eq!(find_last_not_exceeding(&[1, 3, 5], 5), 2);
        assert_eq!(find_last_not_exceeding(&[1, 3, 5], 6), 2);

        assert_eq!(find_last_not_exceeding(&[1], 0), 0);
        assert_eq!(find_last_not_exceeding(&[1], 1), 0);
        assert_eq!(find_last_not_exceeding(&[1], 2), 0);

        assert_eq!(find_first_eq_or_greater(&[1, 3, 5], 0), 0);
        assert_eq!(find_first_eq_or_greater(&[1, 3, 5], 1), 0);
        assert_eq!(find_first_eq_or_greater(&[1, 3, 5], 2), 1);
        assert_eq!(find_first_eq_or_greater(&[1, 3, 5], 3), 1);
        assert_eq!(find_first_eq_or_greater(&[1, 3, 5], 4), 2);
        assert_eq!(find_first_eq_or_greater(&[1, 3, 5], 5), 2);
        assert_eq!(find_first_eq_or_greater(&[1, 3, 5], 6), 3);

        assert_eq!(find_first_eq_or_greater(&[1], 0), 0);
        assert_eq!(find_first_eq_or_greater(&[1], 1), 0);
        assert_eq!(find_first_eq_or_greater(&[1], 2), 1);
    }
}
