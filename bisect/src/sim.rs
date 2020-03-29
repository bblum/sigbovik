use rand::Rng;
use std::f64::EPSILON;

pub trait BisectStrategy {
    fn name(&self) -> String;
    fn select_commit(&mut self, state: &SimulationState) -> usize;
    fn notify_result(&mut self, result: BisectAttempt);
}

#[derive(Clone)]
pub struct SimulationState {
    pub pdf: Vec<f64>,
    pub cdf: Vec<f64>,
    pub false_negative_rate: f64, // \elem [0,1)
    pub lower_bound: usize, // index of the first commit where pdf != 0
    pub upper_bound: usize, // index of the first commit (> lower) where pdf = 0
    pub history: Vec<BisectAttempt>,
    buggy_commit: usize, // shh, its a himitsu!!
}

#[derive(Clone, Copy, Debug)]
pub struct BisectAttempt {
    pub commit: usize,
    pub bug_repros: bool,
}

#[derive(Debug)]
pub struct SimulateResult {
    pub steps: usize,
    pub confidence: f64,
    pub suspected_buggy_commit: usize,
    pub actual_buggy_commit: usize,
}

impl SimulationState {
    pub fn new(num_commits: usize, false_negative_rate: f64) -> Self {
        let buggy_commit = rand::thread_rng().gen_range(0, num_commits);
        Self::new_with_bug_at(num_commits, false_negative_rate, buggy_commit)
    }

    pub fn new_with_bug_at(num_commits: usize, false_negative_rate: f64, buggy_commit: usize) -> Self {
        assert!(num_commits > 0, "excuse me?");
        assert!(buggy_commit < num_commits, "bug not in range");
        assert!(false_negative_rate >= 0.0, "you clown");
        assert!(false_negative_rate < 1.0, "you double clown"); // get it??
        let mut res = Self {
            pdf: vec![1.0 / num_commits as f64; num_commits],
            cdf: vec![0.0; num_commits], // computed below
            false_negative_rate,
            lower_bound: 0,
            upper_bound: num_commits,
            buggy_commit,
            history: vec![],
        };
        // renormalize pdf not needed at this moment
        res.refresh_cdf();
        res.assert_consistent();
        res
    }

    // purely because floating point is terrible
    fn renormalize_pdf(&mut self) {
        let sum: f64 = self.in_range_pdf().iter().sum();
        for i in self.lower_bound..self.upper_bound {
            self.pdf[i] /= sum;
        }
    }

    fn refresh_cdf(&mut self) {
        if self.lower_bound > 0 {
            assert_eq!(self.cdf[self.lower_bound-1], 0.0);
        }
        self.cdf[self.lower_bound] = self.pdf[self.lower_bound];
        for i in self.lower_bound+1..self.upper_bound {
            self.cdf[i] = self.cdf[i-1] + self.pdf[i];
        }
        // ugh
        let one = self.cdf[self.upper_bound-1];
        self.assert_kinda_equals_one(one);
        for i in self.upper_bound..self.cdf.len() {
            // it was already one, but make it a better one
            // (renormalize because epsilon tightens as the pdf size tightens)
            self.cdf[i] = one;
        }
    }

    pub fn in_range(&self, i: usize) -> bool {
        i >= self.lower_bound && i < self.upper_bound
    }

    pub fn bug_found(&self) -> bool {
        let res = self.lower_bound + 1 == self.upper_bound;
        if res {
            assert!(self.false_negative_rate == 0.0 || self.upper_bound == 1);
        }
        res
    }

    pub fn in_range_pdf(&self) -> &[f64] {
        &self.pdf[self.lower_bound..self.upper_bound]
    }

    // [VII 2014]
    pub fn epsilon(&self) -> f64 {
        (self.upper_bound - self.lower_bound) as f64 * EPSILON
    }

    fn assert_kinda_equals_one(&self, x: f64) {
        assert!(self.epsilon() > 0.0);
        assert!(x > 1.0 - self.epsilon());
        assert!(x < 1.0 + self.epsilon());
    }

    fn assert_consistent(&self) {
        assert_eq!(self.pdf.len(), self.cdf.len());
        assert!(self.lower_bound < self.upper_bound, "whered the bug go?");

        if self.lower_bound + 1 == self.upper_bound {
            // termination condition; pdf should be 1 here
            self.assert_kinda_equals_one(self.pdf[self.lower_bound]);
        }

        let mut sum = 0.0;
        for (i, &p) in self.pdf.iter().enumerate() {
            sum += p;
            assert!(p >= 0.0);
            if self.in_range(i) {
                // it's possible for a p to get driven all the way down to 0
                // by a very inefficient strategy..! (within rounding error, at least)
                // better strategies will not "steal from the poor" so much.
                // it took random-uniform w/ N>=44 to expose this, and when it did,
                // i was 0 and pdf[1] was 4.0474 x 10^-320. wow!
                // before that, of course, this assertion was just self.pdf[i] > 0.0
                assert!(self.pdf[i] >= 0.0);
            } else {
                assert_eq!(self.pdf[i], 0.0);
            }
            assert!(p < 1.0 + self.epsilon());
            // i had to renormalize >=upper bound, in refresh_cdf, to make this work!!
            assert!(self.cdf[i] < 1.0 + self.epsilon());
        }
        self.assert_kinda_equals_one(sum);
    }

    // TODO use this in the bayes rule app somehow
    pub fn blind_a_priori_repro_prob(&self, commit: usize) -> f64 {
        self.cdf[commit] * (1.0 - self.false_negative_rate)
    }

    fn adjust_pdf_bug_repros(&mut self, commit: usize) {
        assert!(self.in_range(commit), "tested known buggy commit");
        assert_ne!(commit, self.upper_bound - 1, "tested known buggy commit");
        // TODO: rewrite this part using bayes' rule, prove it's equivalent
        // this is the amount of the probability space that will go to 1
        let bisected_cdf = self.cdf[commit];
        for i in self.lower_bound..self.upper_bound {
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
        self.renormalize_pdf();
        self.refresh_cdf();
        self.upper_bound = commit + 1;
    }

    fn adjust_pdf_no_repro(&mut self, commit: usize) {
        assert!(self.in_range(commit), "tested known buggy commit");
        assert_ne!(commit, self.upper_bound - 1, "tested known buggy commit");
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
        for i in self.lower_bound..=commit {
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
        for i in commit+1..self.upper_bound {
            // everything here summed to `1 - bisected_cdf` before.
            // now we want it to sum to `p_bug_after`.
            // as above, `1 - bisected_cdf` is a factor of `p_bug_in_or_after`.
            self.pdf[i] *= p_b_given_a / p_b;
        }

        // TODO: figure out why this assertion still doesn't work even w/fancy epsilons
        //self.assert_kinda_equals(self.cdf[commit], p_bug_before);
        self.assert_kinda_equals_one(p_bug_before + p_bug_in_or_after);

        self.renormalize_pdf();
        self.refresh_cdf();
        if self.false_negative_rate == 0.0 {
            self.lower_bound = commit + 1;
        }
    }

    pub fn hypothetical_pdf_bug_repros(&self, commit: usize) -> Vec<f64> {
        let mut self2 = self.clone();
        self2.adjust_pdf_bug_repros(commit);
        self2.pdf
    }

    pub fn hypothetical_pdf_no_repro(&self, commit: usize) -> Vec<f64> {
        let mut self2 = self.clone();
        self2.adjust_pdf_no_repro(commit);
        self2.pdf
    }

    fn simulate_step(&mut self, commit: usize) -> BisectAttempt {
        let bug_present = commit >= self.buggy_commit;
        let bug_repros = if bug_present {
            rand::thread_rng().gen_range(0.0, 1.0) > self.false_negative_rate
        } else {
            false
        };
        if bug_repros {
            // Test failed! bug definitely present here.
            self.adjust_pdf_bug_repros(commit);
        } else {
            // Test passed... bug maybe absent, maybe not.
            self.adjust_pdf_no_repro(commit);
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
                _ => best = Some((i, p)),
            }
        }
        best.expect("pdf shouldn't be empty")
    }

    fn simulate_while(
        mut self,
        mut strat: impl BisectStrategy,
        f: impl Fn(&Self) -> bool,
    ) -> SimulateResult {
        // println!("simulating; himitsu @ {}", self.buggy_commit);
        while f(&self) {
            let prev_pdf = self.pdf.clone();

            let commit = strat.select_commit(&self);
            let result = self.simulate_step(commit);
            strat.notify_result(result);

            if self.pdf[commit] < EPSILON {
                // special case for random-uniform to dodge the no progress assert.
                // see huge comment about it, above in assert_consistent.
                // in this case, if you get p all the way down to 0.00...025, the
                // minimum nonzero float, it turns into a fixed point to bisect on it.
                continue;
            }

            // println!("simulating; {:?} -> pdf {:?}", result, self.pdf);
            assert!(self.pdf != prev_pdf, "no progress");
        }

        // found_a_bug.c
        let (suspected_buggy_commit, confidence) = self.best_commit();
        SimulateResult {
            steps: self.history.len(),
            confidence,
            suspected_buggy_commit,
            actual_buggy_commit: self.buggy_commit,
        }
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

    #[allow(unused)]
    pub fn last_cdf_index_not_exceeding(&self, target_value: f64) -> usize {
        find_last_not_exceeding(&self.cdf, target_value)
    }

    pub fn first_cdf_index_eq_or_greater(&self, target_value: f64) -> usize {
        find_first_eq_or_greater(&self.cdf, target_value)
    }
}

pub fn find_last_not_exceeding<T: PartialOrd>(v: &[T], target_value: T) -> usize {
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

pub fn find_first_eq_or_greater<T: PartialOrd>(v: &[T], target_value: T) -> usize {
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
pub mod test_helpers {
    use super::*;

    // runs a function on a bunch of different possible legal pdfs
    pub fn run_pdf_invariant_test(n: usize, f: impl Fn(&SimulationState)) {
        for buggy_commit in 0..n {
            for &fn_prob in &[0.0, 0.1, 0.5, 0.99] {
                let mut s = SimulationState::new_with_bug_at(n, fn_prob, buggy_commit);
                println!("pdf test: fnprob {}; initial pdf: {:?}", fn_prob, s.pdf);
                f(&s);
                // TODO: clean up this "never test pdf[n-1]" thing.
                // allow 64 to be the answer & testing 63 to make sense.
                for i in (0..n-1).rev() {
                    if i < s.lower_bound {
                        break;
                    }
                    s.simulate_step(i);
                    println!("pdf test: fnprob {}; pdf after testing {}: {:?}",
                             fn_prob, i, s.pdf);
                    f(&s);
                }
            }
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

    #[test]
    fn test_determinism() {
        // power of 2 num_commits makes floats work well
        let mut s = SimulationState::new_with_bug_at(8, 0.0, 5);
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
    #[should_panic]
    fn test_known_buggy_commit_above() {
        let mut s = SimulationState::new_with_bug_at(8, 0.0, 5);
        s.simulate_step(5);
        s.simulate_step(6);
    }

    #[test]
    #[should_panic]
    fn test_known_buggy_commit_below() {
        let mut s = SimulationState::new_with_bug_at(8, 0.0, 5);
        s.simulate_step(4);
        s.simulate_step(3);
    }

    #[test]
    fn test_pdf_slice() {
        let mut s = SimulationState::new_with_bug_at(8, 0.0, 5);
        s.simulate_step(2);
        s.simulate_step(5);
        assert_eq!(s.in_range_pdf(), &[1.0 / 3.0; 3]);
    }

    #[test]
    fn test_blind() {
        let s = SimulationState::new(8, 0.0);
        for i in 0..8 {
            assert_eq!(s.blind_a_priori_repro_prob(i), 0.125 * (i + 1) as f64);
        }

        let s = SimulationState::new(8, 0.5);
        for i in 0..8 {
            assert_eq!(s.blind_a_priori_repro_prob(i), 0.0625 * (i + 1) as f64);
        }
    }

    #[test]
    fn test_pdf_monotonicity() {
        test_helpers::run_pdf_invariant_test(64, |s| {
            let mut last_p = 0.0;
            let mut k_seen = false;
            for &p in &s.pdf {
                if p < last_p {
                    assert!(!k_seen);
                    k_seen = true;
                }
                if k_seen {
                    assert_eq!(p, 0.0);
                }
                last_p = p;
            }
        })
    }

    #[test]
    fn test_figure_1() {
        let mut s = SimulationState::new_with_bug_at(16, 0.5, 11);

        s.adjust_pdf_no_repro(7);
        s.adjust_pdf_bug_repros(11);
        assert_eq!(s.pdf[0], 0.0625);
        assert_eq!(s.pdf[11], 0.125);

        s.adjust_pdf_no_repro(9);
        assert_eq!(s.pdf[0], 0.05);
        assert_eq!(s.pdf[9], 0.1);
        assert_eq!(s.pdf[10], 0.2);

        s.adjust_pdf_no_repro(10);
        assert!(s.pdf[11] < 1.0 / 3.0 + s.epsilon());
        assert!(s.pdf[11] > 1.0 / 3.0 - s.epsilon());

        for _ in 0..17 {
            s.adjust_pdf_no_repro(10);
        }
        assert!(s.pdf[11] < 0.99999 - s.epsilon());
        s.adjust_pdf_no_repro(10);
        assert!(s.pdf[11] > 0.99999 + s.epsilon());
    }
}
