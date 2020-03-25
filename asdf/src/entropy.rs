use crate::sim::SimulationState;

fn entropy(pdf: &[f64]) -> f64 {
    let n = pdf.len() as f64;
    pdf.into_iter().filter(|&&p| p != 0.0).map(|&p| -p * p.log2() / n).sum()
}

impl SimulationState {
    pub fn hypothetical_expected_entropy(&self, commit: usize) -> f64 {
        // TODO refactor/restructure to allow this
        assert!(commit < self.pdf.len() - 1, "do not bisect at last commit");
        assert!(self.pdf[commit+1] > 0.0, "do not bisect at known buggy commit");

        let p = self.blind_a_priori_repro_prob(commit);
        let entropy_repro    = entropy(&self.hypothetical_pdf_bug_repros(commit));
        let entropy_no_repro = entropy(&self.hypothetical_pdf_no_repro(commit));

        (p * entropy_repro) + ((1.0 - p) * entropy_no_repro)
    }
}

#[cfg(test)]
mod tests {
    use crate::sim::test_helpers::run_pdf_invariant_test;
    use super::entropy;
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

    // this test validates `strategies::MinExpectedEntropy`'s reliance on binary search
    // to find the bisect point of minimum expected entropy, instead of linear
    #[test]
    fn test_expected_entropy_has_unique_local_minimum() {
        // 34 may seem like a weird `n`, but 32 was not sufficient to discover
        // the need for comparing `diff > EPSILON` instead of `diff > 0.0`.
        // 34 was the min such `n` that did it. after that, behaves the same up to 128.
        // also, don't include deterministic case in fnprobs; it's too fiddly.
        run_pdf_invariant_test(34, &[0.1, 0.5, 0.99], |s| {
            let mut any_diff = false;
            let mut diff_ever_negative = false;
            let mut diff_ever_positive = false;

            let mut last_e = s.hypothetical_expected_entropy(0);
            for i in 1..s.pdf.len()-1 {
                if s.pdf[i+1] == 0.0 {
                    // don't test the last buggy commit
                    break;
                }
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
                assert!(diff_ever_negative || diff_ever_positive, "flat entropy");
            }
        })
    }
}
