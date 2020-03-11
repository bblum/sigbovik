use crate::sim::{
    BisectAttempt,
    BisectStrategy,
    SimulationState,
};

// it just finds where the cdf crosses 0.5 or whatever
pub struct CdfBisect {
    bisect_point: f64,
    earliest_bug_seen: Option<usize>,
}

impl CdfBisect {
    // TODO: make these `new()`s take a &SimulationState
    // so you can initialize using pdf.len and falsepos prob and so on
    pub fn new() -> Self {
        Self::new_with_bisect_point(0.5) // probably suboptimal
    }

    pub fn new_with_bisect_point(bisect_point: f64) -> Self {
        Self { bisect_point, earliest_bug_seen: None }
    }
}

impl BisectStrategy for CdfBisect {
    fn select_commit(&mut self, state: &SimulationState) -> usize {
        let res = find_first_eq_or_greater(&state.cdf, self.bisect_point);
        if Some(res) == self.earliest_bug_seen {
            assert!(res > 0, "saw bug on commit 0 -> should be 100% already");
            res - 1
        } else if res == state.cdf.len() - 1 {
            // never test the last commit
            // TODO: simplify this by just initializing earliest_bug_seen to len.
            // pending the refactor of the TODO above
            res - 1
        } else {
            res
        }
    }

    fn notify_result(&mut self, result: BisectAttempt) {
        if result.bug_repros {
            match self.earliest_bug_seen {
                Some(i) if i < result.commit => {}, // progress
                Some(i) if i == result.commit => panic!("tested known bug twice"),
                _ => {
                    self.earliest_bug_seen = Some(result.commit);
                }
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
