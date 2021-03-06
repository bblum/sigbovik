#![cfg(test)]

use crate::sim::{BisectStrategy, SimulationState};
use crate::strategies::*;

// there's a progress assertion in the simulator we hope won't trip here
// basically, we're testing a strategy is capable of driving the confidence
// to some arbitrary threshold (e.g. five nines), no matter how slowly
fn run_progress_test<S: BisectStrategy>(n: usize, f: impl Fn(&SimulationState) -> S) {
    for buggy_commit in 0..n {
        for &fp_prob in &[0.0, 0.5, 0.9] {
            let s = SimulationState::new_with_bug_at(n, fp_prob, buggy_commit);
            let strat = f(&s);
            let res = s.simulate_til_confident(strat, 0.99999);
            if fp_prob == 0.0 {
                assert_eq!(res.suspected_buggy_commit, buggy_commit);
            }
        }
    }
}

#[test]
fn test_simulate_deterministic_naive() {
    let logn = 6;
    let n = 1 << logn;
    for buggy_commit in 0..n {
        let s = SimulationState::new_with_bug_at(n, 0.0, buggy_commit);
        let nb = NaiveBinarySearch::new(&s, ConfusedHumanMode::ForgetEverything);
        let res = s.simulate_til_confident(nb, 1.0);
        assert_eq!(res.steps, logn);
        assert_eq!(res.confidence, 1.0);
        assert_eq!(res.suspected_buggy_commit, buggy_commit);
    }
}

#[test]
fn test_linear_dumbness() {
    let n = 16;
    for buggy_commit in 0..n {
        println!("asdf: {}", buggy_commit);
        let s = SimulationState::new_with_bug_at(n, 0.0, buggy_commit);
        let c = LinearSearch::new(&s);
        let res = s.simulate_til_confident(c, 0.99);
        println!("asdf: steps was {}", res.steps);
        assert_eq!(res.suspected_buggy_commit, buggy_commit);
        // it searches backwards
        // bug @ 0 takes same steps to confirm as bug @ 1. otherwise normal
        let expected_steps = n - std::cmp::max(buggy_commit, 1);
        assert_eq!(res.steps, expected_steps);
    }
}

#[test]
fn test_cdfbisect_progress() {
    for &bisect_point in &[0.01, 0.1, 0.5, 0.9, 0.99] {
        run_progress_test(16, |s| CdfBisect::new(s, bisect_point));
    }
}

#[test]
fn test_naive_progress() {
    run_progress_test(64, |s| NaiveBinarySearch::new(s, ConfusedHumanMode::ForgetEverything));
    run_progress_test(64, |s| NaiveBinarySearch::new(s, ConfusedHumanMode::UsePreviousLow));
    for &num_retries in &[1, 8, 64] {
        run_progress_test(64, |s| NaiveBinarySearch::new(s, ConfusedHumanMode::Mistrustful(num_retries)));
    }
}

#[test]
fn test_linear_progress() {
    run_progress_test(64, LinearSearch::new);
}

#[test]
fn test_entropy_progress() {
    run_progress_test(47, MinExpectedEntropy::new);
}

#[test]
fn test_random_progress() {
    // 48 was min number to expose the 0 bug documented in assert_consistent.
    run_progress_test(48, |s| ChooseRandomly::new(s, RandomMode::Uniformly));
    run_progress_test(32, |s| ChooseRandomly::new(s, RandomMode::WeightedByCDF));
}
