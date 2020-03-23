use crate::sim::SimulationState;
use crate::strategies::*;

#[test]
fn test_simulate_deterministic_naive() {
    let logn = 6;
    let n = 1 << logn;
    for buggy_commit in 0..n {
        let s = SimulationState::new_with_bug_at(n, 0.0, buggy_commit);
        let nb = NaiveBinarySearch::new(&s);
        let res = s.simulate_til_confident(nb, 1.0);
        assert_eq!(res.steps, logn);
        assert_eq!(res.confidence, 1.0);
        assert_eq!(res.suspected_buggy_commit, buggy_commit);
    }
}

#[test]
fn test_cdfbisect_progress() {
    let n = 16;
    for buggy_commit in 0..n {
        for &bisect_point in &[0.01, 0.1, 0.5, 0.9, 0.99] {
            for &fp_prob in &[0.0, 0.5, 0.9] {
                let s = SimulationState::new_with_bug_at(n, fp_prob, buggy_commit);
                let c = CdfBisect::new(&s, bisect_point);
                let _res = s.simulate_til_confident(c, 0.99);
                // lol no! the algorithm could be wrong :))
                // assert_eq!(res.suspected_buggy_commit, buggy_commit);
            }
        }
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
