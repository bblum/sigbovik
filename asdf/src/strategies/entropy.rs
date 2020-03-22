use crate::sim::*;

// searches for the commit at which bisecting yields minimum expected entropy
pub struct MaxExpectedEntropy {
    earliest_bug_seen: usize,
}

impl MaxExpectedEntropy {
    pub fn new(s: &SimulationState) -> Self {
        Self {
            earliest_bug_seen: s.pdf.len() - 1,
        }
    }
}

impl BisectStrategy for MaxExpectedEntropy {
    fn select_commit(&mut self, s: &SimulationState) -> usize {
        // can't use min_by_key b/c f64 !: Ord >_<
        let mut best = None;
        for i in 0..self.earliest_bug_seen {
            let expected_entropy = hypothetical_expected_entropy(s, i);
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

fn hypothetical_expected_entropy(s: &SimulationState, commit: usize) -> f64 {
    let p = s.blind_a_priori_repro_prob(commit);
    let entropy_repro    = entropy(&s.hypothetical_pdf_bug_repros(commit));
    let entropy_no_repro = entropy(&s.hypothetical_pdf_no_repro(commit));
    (p * entropy_repro) + ((1.0 - p) * entropy_no_repro)
}

fn entropy(pdf: &[f64]) -> f64 {
    let n = pdf.len() as f64;
    pdf.iter().map(|&p| -p * p.ln() / n).sum()
}

#[cfg(test)]
mod tests {
    use super::entropy;

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
}
