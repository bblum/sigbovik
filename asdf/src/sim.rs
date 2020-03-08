use rand::Rng;

pub trait BisectStrategy {
    fn select_commit(&mut self, state: &SimulationState) -> usize;
    fn notify_result(&mut self, commit: usize, bug_repros: bool);
}

pub struct SimulationState {
    pub probabilities: Vec<f64>,
    pub false_positive_rate: f64, // \elem [0,1)
    buggy_commit: usize, // shh, its a himitsu!!
    history: Vec<Attempt>,
}

struct Attempt {
    commit: usize,
    bug_present: bool,
    bug_repros: bool,
}

#[derive(Debug)]
pub struct SimulateResult {
    pub steps: usize,
    pub confidence: f64,
    pub suspected_buggy_commit: usize,
}

impl SimulationState {
    pub fn new(num_commits: usize, false_positive_rate: f64) -> Self {
        assert!(num_commits > 0, "excuse me?");
        assert!(false_positive_rate >= 0.0, "you clown");
        assert!(false_positive_rate < 1.0, "you double clown"); // get it??
        let initial_p = 1.0 / num_commits as f64;
        let res = Self {
            probabilities: std::iter::repeat(initial_p).take(num_commits).collect(),
            false_positive_rate,
            buggy_commit: rand::thread_rng().gen_range(0, num_commits),
            history: vec![],
        };
        res.assert_consistent();
        res
    }

    fn assert_consistent(&self) {
        let sum = self.probabilities.iter().sum::<f64>();
        // i guess? i don't really understand this stuff
        let eps = self.probabilities.len() as f64 * std::f64::EPSILON;
        assert!(float_cmp::approx_eq!(f64, sum, 1.0, epsilon = eps));
    }

    fn simulate_step(&mut self, commit: usize) -> bool {
        let bug_present = commit <= self.buggy_commit;
        let bug_repros = if bug_present {
            rand::thread_rng().gen_range(0.0, 1.0) > self.false_positive_rate
        } else {
            false
        };
        if bug_repros {
            // Test failed! bug definitely present here.
            // TODO: adjust probabilities here
        } else {
            // Test passed... bug maybe absent, maybe not.
            // TODO: do bayes rule stuff here
        }
        self.history.push(Attempt { commit, bug_present, bug_repros });
        self.assert_consistent();
        bug_repros
    }

    fn best_commit(&self) -> (usize, f64) {
        let mut best = None;
        for (i, &p) in self.probabilities.iter().enumerate() {
            match best {
                Some((_, best_p)) if p < best_p => {},
                _ => {
                    best = Some((i, p));
                },
            }
        }
        best.expect("probabilities shouldn't be empty")
    }

    fn simulate_while(mut self, mut strategy: impl BisectStrategy, f: impl Fn(&Self) -> bool) -> SimulateResult {
        while f(&self) {
            let commit = strategy.select_commit(&self);
            let bug_repros = self.simulate_step(commit);
            strategy.notify_result(commit, bug_repros);
        }

        // found_a_bug.c
        let (suspected_buggy_commit, confidence) = self.best_commit();
        SimulateResult {
            steps: self.history.len(),
            confidence,
            suspected_buggy_commit,
        }
    }

    pub fn simulate_n_steps(self, strategy: impl BisectStrategy, max_steps: usize) -> SimulateResult {
        self.simulate_while(strategy, |state| state.history.len() < max_steps)
    }

    pub fn simulate_til_confident(self, strategy: impl BisectStrategy, confidence_threshold: f64) -> SimulateResult {
        self.simulate_while(strategy, |state| state.best_commit().1 < confidence_threshold)
    }
}
