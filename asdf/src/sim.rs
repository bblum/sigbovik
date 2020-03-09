use float_cmp::approx_eq;
use rand::Rng;
use std::f64::EPSILON;


pub trait BisectStrategy {
    fn select_commit(&mut self, state: &SimulationState) -> usize;
    fn notify_result(&mut self, result: BisectAttempt);
}

pub struct SimulationState {
    pub pdf: Vec<f64>,
    pub cdf: Vec<f64>,
    pub false_negative_rate: f64, // \elem [0,1)
    buggy_commit: usize, // shh, its a himitsu!!
    history: Vec<BisectAttempt>,
}

#[derive(Clone, Copy)]
pub struct BisectAttempt {
    pub commit: usize,
    pub bug_repros: bool,
}

#[derive(Debug)]
pub struct SimulateResult {
    pub steps: usize,
    pub confidence: f64,
    pub suspected_buggy_commit: usize,
}

impl SimulationState {
    pub fn new(num_commits: usize, false_negative_rate: f64) -> Self {
        let buggy_commit = rand::thread_rng().gen_range(0, num_commits);
        Self::new_with_bug_at(num_commits, false_negative_rate, buggy_commit)
    }

    fn new_with_bug_at(num_commits: usize, false_negative_rate: f64, buggy_commit: usize) -> Self {
        assert!(num_commits > 0, "excuse me?");
        assert!(buggy_commit < num_commits, "bug not in range");
        assert!(false_negative_rate >= 0.0, "you clown");
        assert!(false_negative_rate < 1.0, "you double clown"); // get it??
        let mut res = Self {
            pdf: vec![1.0 / num_commits as f64; num_commits],
            cdf: vec![0.0; num_commits], // computed below
            false_negative_rate,
            buggy_commit,
            history: vec![],
        };
        res.refresh_cdf();
        res.assert_consistent();
        res
    }

    fn refresh_cdf(&mut self) {
        self.cdf[0] = self.pdf[0];
        for i in 1..self.pdf.len() {
            self.cdf[i] = self.cdf[i-1] + self.pdf[i];
        }
    }

    fn assert_consistent(&self) {
        assert_eq!(self.pdf.len(), self.cdf.len());
        // i guess? i don't really understand this stuff
        let eps = self.pdf.len() as f64 * EPSILON;
        let mut sum = 0.0;
        for (i, &p) in self.pdf.iter().enumerate() {
            sum += p;
            assert!(p >= 0.0);
            assert!(p <= 1.0);
            assert!(self.cdf[i] <= 1.0);
            assert!(approx_eq!(f64, sum, self.cdf[i], epsilon = eps));
        }
        assert!(approx_eq!(f64, sum, 1.0, epsilon = eps));
    }

    fn simulate_step(&mut self, commit: usize) -> BisectAttempt {
        assert!(commit < self.pdf.len());
        let bug_present = commit >= self.buggy_commit;
        let bug_repros = if bug_present {
            rand::thread_rng().gen_range(0.0, 1.0) > self.false_negative_rate
        } else {
            false
        };
        if bug_repros {
            // Test failed! bug definitely present here.
            // TODO: rewrite this part using bayes' rule, prove it's equivalent
            // this is the amount of the probability space that will go to 1
            let bisected_cdf = self.cdf[commit];
            for i in 0..self.pdf.len() {
                if i <= commit {
                    // adjust commits before and including the buggy commit
                    // any of them might have introduced the bug
                    // all pdfs before the test point currently sum to bisected_cdf
                    // now we want it to sum to 1. so multiply by 1/bisected_cdf
                    self.pdf[i] /= bisected_cdf;
                } else {
                    // adjust commits after this buggy commit down to 0
                    // they won't have introduced it
                    self.pdf[i] = 0.0;
                }
            }
            self.refresh_cdf();
        } else {
            // Test passed... bug maybe absent, maybe not.

            // this is the amount of the probability space probably withOUT the bug now
            let bisected_cdf = self.cdf[commit];
            // apply bayes' rule.
            // P(A|B) = P(B|A) P(A) / P(B) where
            // P(bug <= commit | test passed) =
            // P(test passed | bug <= commit) --> false negative rate
            // P(bug <= commit) --> bisected_cdf
            // P(test passed) --> (FNR * bisected_cdf) + (1 * (1 - bisected_cdf))
            let p_b_given_a = self.false_negative_rate;
            let p_a = bisected_cdf;
            let p_b = (self.false_negative_rate * bisected_cdf) + (1.0 - bisected_cdf);
            let p_bug_before = p_b_given_a * p_a / p_b;
            // adjust commits before & including the probably-not-buggy commit down
            for i in 0..=commit {
                // everything here summed to `bisected_cdf` before.
                // now we want it to sum to `p_bug_before`.
                // so, multiply by p_bug_before / bisected_cdf.
                // since bisected_cdf is actually a factor within p_bug_before (p_a),
                // that's equivalent to just multiplying by p_b_given_a / p_b.
                self.pdf[i] *= p_b_given_a / p_b;
            }

            // adjust commits including & after to go down, i.e. be "probably not buggy"
            // now "A" is "bug introduced in or after this commit". "B" is the same.
            let p_b_given_a = 1.0;
            let p_a = 1.0 - bisected_cdf;
            let p_bug_in_or_after = p_b_given_a * p_a / p_b;
            for i in commit+1..self.pdf.len() {
                // everything here summed to `1 - bisected_cdf` before.
                // now we want it to sum to `p_bug_after`.
                // as above, `1 - bisected_cdf` is a factor of `p_bug_in_or_after`.
                self.pdf[i] *= p_b_given_a / p_b;
            }

            self.refresh_cdf();
            // check consistency of the first half
            assert!(approx_eq!(f64, self.cdf[commit], p_bug_before, epsilon = EPSILON));
            // also this
            let total_p = p_bug_before + p_bug_in_or_after;
            assert!(approx_eq!(f64, total_p, 1.0, epsilon = EPSILON));
        }
        let res = BisectAttempt { commit, bug_repros };
        self.history.push(res);
        self.assert_consistent();
        res
    }

    fn best_commit(&self) -> (usize, f64) {
        let mut best = None;
        for (i, &p) in self.pdf.iter().enumerate() {
            match best {
                Some((_, best_p)) if p < best_p => {},
                _ => {
                    best = Some((i, p));
                },
            }
        }
        best.expect("pdf shouldn't be empty")
    }

    fn simulate_while(
        mut self,
        mut strat: impl BisectStrategy,
        f: impl Fn(&Self) -> bool,
    ) -> SimulateResult {
        while f(&self) {
            let commit = strat.select_commit(&self);
            let result = self.simulate_step(commit);
            strat.notify_result(result);
        }

        // found_a_bug.c
        let (suspected_buggy_commit, confidence) = self.best_commit();
        SimulateResult { steps: self.history.len(), confidence, suspected_buggy_commit }
    }

    #[allow(unused)]
    pub fn simulate_n_steps(
        self,
        strat: impl BisectStrategy,
        max_steps: usize,
    ) -> SimulateResult {
        self.simulate_while(strat, |state| state.history.len() < max_steps)
    }

    pub fn simulate_til_confident(
        self,
        strat: impl BisectStrategy,
        confidence_threshold: f64,
    ) -> SimulateResult {
        self.simulate_while(strat, |state| state.best_commit().1 < confidence_threshold)
    }
}

#[cfg(test)]
mod tests {
    use super::SimulationState;
    use crate::naive::NaiveBinarySearch;

    #[test]
    fn test_determinism() {
        // power of 2 num_commits makes floats work well
        let mut s = SimulationState::new_with_bug_at(8, 0.0, 5);
        assert_eq!(s.pdf, vec![0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125]);
        assert_eq!(s.cdf, vec![0.125, 0.250, 0.375, 0.500, 0.625, 0.750, 0.875, 1.000]);

        // why would you test commit 7?? that gains no information. oh well
        assert_eq!(s.simulate_step(7).bug_repros, true);
        assert_eq!(s.pdf, vec![0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125]);
        assert_eq!(s.cdf, vec![0.125, 0.250, 0.375, 0.500, 0.625, 0.750, 0.875, 1.000]);

        // don't test commit 1-2 bc that'll introduce float imprecision with 0.33 lmao

        assert_eq!(s.simulate_step(3).bug_repros, false);
        assert_eq!(s.pdf, vec![0.0, 0.0, 0.0, 0.0, 0.25, 0.25, 0.25, 0.25]);
        assert_eq!(s.cdf, vec![0.0, 0.0, 0.0, 0.0, 0.25, 0.50, 0.75, 1.00]);
        //                                    ^^^

        assert_eq!(s.simulate_step(5).bug_repros, true);
        assert_eq!(s.pdf, vec![0.0, 0.0, 0.0, 0.0, 0.5, 0.5, 0.0, 0.0]);
        assert_eq!(s.cdf, vec![0.0, 0.0, 0.0, 0.0, 0.5, 1.0, 1.0, 1.0]);
        //                                              ^^^

        assert_eq!(s.simulate_step(4).bug_repros, false);
        assert_eq!(s.pdf, vec![0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0]);
        assert_eq!(s.cdf, vec![0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0]);
        //                                         ^^^
    }

    #[test]
    fn test_simulate_deterministic_naive() {
        let logn = 6;
        let n = 1 << logn;
        for buggy_commit in 0..n {
            let s = SimulationState::new_with_bug_at(n, 0.0, buggy_commit);
            let res = s.simulate_til_confident(NaiveBinarySearch::new(), 1.0);
            assert_eq!(res.steps, logn);
            assert_eq!(res.confidence, 1.0);
            assert_eq!(res.suspected_buggy_commit, buggy_commit);
        }
    }
}
