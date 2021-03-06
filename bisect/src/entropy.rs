use crate::sim::SimulationState;
use std::collections::HashMap;

fn entropy(pdf: &[f64]) -> f64 {
    let n = pdf.len() as f64;
    pdf.into_iter().filter(|&&p| p != 0.0).map(|&p| -p * p.log2() / n).sum()
}

impl SimulationState {
    pub fn hypothetical_expected_entropy(&self, commit: usize) -> f64 {
        assert!(self.in_range(commit), "commit out of range");
        // TODO refactor/restructure to allow this
        assert!(commit < self.pdf.len() - 1, "do not bisect at last commit");
        assert!(self.pdf[commit+1] > 0.0, "do not bisect at known buggy commit");

        let p = self.blind_a_priori_repro_prob(commit);
        let entropy_repro    = entropy(&self.hypothetical_pdf_bug_repros(commit));
        let entropy_no_repro = entropy(&self.hypothetical_pdf_no_repro(commit));

        (p * entropy_repro) + ((1.0 - p) * entropy_no_repro)
    }

    #[cfg_attr(not(test), allow(unused))]
    pub fn min_expected_entropy_linear_search(&self) -> usize {
        assert!(!self.bug_found(), "can't search when bug already found");
        let mut last_e = self.hypothetical_expected_entropy(self.lower_bound);
        for i in self.lower_bound+1..self.upper_bound-1 {
            let e = self.hypothetical_expected_entropy(i);
            if last_e < e - self.epsilon() {
                // entropy is lower at i-1 than at i
                // since it's guaranteed to have a unique local minimum, that was it
                return i-1;
            }
            last_e = e;
        }
        // if we fall out of the loop, it's because the min was at the front!
        // (or because there are only two commits left to test.)
        self.upper_bound-2
    }

    pub fn min_expected_entropy_binary_search(&self) -> usize {
        assert!(!self.bug_found(), "can't search when bug already found");
        let mut min = self.lower_bound;
        let mut max = self.upper_bound - 2;

        // because we measure diff in e vs neighbors, we might ask for an `i` twice
        let mut known_entropies = HashMap::new();
        let mut measure_expected_entropy = move |i: usize| {
            if let Some(&e) = known_entropies.get(&i) {
                e
            } else {
                let e = self.hypothetical_expected_entropy(i);
                known_entropies.insert(i, e);
                e
            }
        };

        while min < max {
            // TODO: try weighting by cdf? maybe that's faster?
            // (as the search
            let mid = (min + max) / 2;
            // div by two rounds towards min, so compare mid against mid+1.
            let e0 = measure_expected_entropy(mid);
            let e1 = measure_expected_entropy(mid + 1);
            // TODO: be more robust about flat entropy; it's inconclusive
            // so maybe check both sides. for now we'll bias towards the right
            // because empirically i think we should get plateaus only on the left
            // when p shrinks to very very tiny.
            if e1 > e0 + self.epsilon() {
                // positive derivative -- fall to the left
                max = mid;
            } else {
                // negative (or zero) derivative -- fall to the right
                min = mid + 1;
            }
        }
        min
    }
}

#[cfg(test)]
mod tests {
    use crate::sim::test_helpers::run_pdf_invariant_test;
    use super::*;
    use std::f64::EPSILON;

    #[test]
    fn test_entropy() {
        let v2  = vec![0.5, 0.5];
        let v4a = vec![0.25, 0.25, 0.25, 0.25];
        let v4b = vec![0.12, 0.12, 0.25, 0.51];
        let v4c = vec![0.10, 0.10, 0.20, 0.60];
        let v4d = vec![0.01, 0.02, 0.03, 0.94];

        for v in &[&v2, &v4a, &v4b, &v4c, &v4d] {
            assert_eq!(v.iter().sum::<f64>(), 1.0);
        }

        assert_eq!(entropy(&v2), entropy(&v4a));
        assert!(entropy(&v4b) < entropy(&v4a));
        assert!(entropy(&v4c) < entropy(&v4b));
        assert!(entropy(&v4d) < entropy(&v4c));
    }

    #[test]
    fn test_entropy_stability() {
        let v4e = vec![0.00000_00, 0.00000_00, 0.00001_00, 0.99999_00];
        let v4f = vec![0.00000_00, 0.00000_00, 0.00000_99, 0.99999_01];
        let v4g = vec![0.00000_33, 0.00000_33, 0.00000_33, 0.99999_01];
        assert_eq!(v4e.iter().sum::<f64>(), 1.0);
        assert_eq!(v4f.iter().sum::<f64>(), 1.0);
        assert_eq!(v4g.iter().sum::<f64>(), 1.0);

        // as you'd expect, v4f's max is higher
        assert!(entropy(&v4e) > entropy(&v4f));
        // but! v4e tells you more about the first 3 than v4g does, despite lower max.
        assert!(entropy(&v4e) < entropy(&v4g));
    }


    // this test validates `strategies::MinExpectedEntropy`'s reliance on binary search
    // to find the bisect point of minimum expected entropy, instead of linear
    #[test]
    fn test_expected_entropy_has_unique_local_minimum() {
        // 34 may seem like a weird `n`, but 32 was not sufficient to discover
        // the need for comparing `diff > EPSILON` instead of `diff > 0.0`.
        // 34 was the min such `n` that did it. after that, behaves the same up to 128.
        run_pdf_invariant_test(34, |s| {
            if s.bug_found() {
                return;
            }

            let mut any_diff = false;
            let mut diff_ever_negative = false;
            let mut diff_ever_positive = false;

            let mut last_e = s.hypothetical_expected_entropy(s.lower_bound);
            for i in s.lower_bound+1..s.upper_bound-1 {
                let e = s.hypothetical_expected_entropy(i);
                let diff = e - last_e;
                last_e = e;

                any_diff = true;
                if diff < -EPSILON {
                    diff_ever_negative = true;
                }
                if diff > EPSILON {
                    diff_ever_positive = true;
                } else {
                    assert!(!diff_ever_positive, "found a 2nd valley");
                }
            }
            if any_diff {
                if !(diff_ever_negative || diff_ever_positive) {
                    if s.false_negative_rate == 0.0 {
                        // flat entropy is uniquely possible in this case
                        // XXX: this can get tripped up by float imprecision
                        // ...but it seems like for 34;1 it doesn't v0v
                        assert_eq!(s.in_range_pdf(), &[1.0 / 3.0; 3]);
                    } else {
                        panic!("flat entropy");
                    }
                }
            }
        })
    }

    #[test]
    fn test_search_for_intelligent_life_um_i_mean_min_entropy() {
        run_pdf_invariant_test(32, |s| {
            if s.bug_found() {
                return;
            }
            let linear_result = s.min_expected_entropy_linear_search();
            let binary_result = s.min_expected_entropy_binary_search();
            assert_eq!(linear_result, binary_result);
        })
    }
}
