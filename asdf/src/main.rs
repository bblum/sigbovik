use std::io::Write;
use std::fs::File;
use rand::Rng;

mod entropy;
mod strategies;
mod sim;
mod test;

use strategies::CdfBisect;
use sim::SimulationState;

fn output(f: &mut File, s: String) {
    println!("{}", s);
    f.write_all(s.as_bytes()).expect("couldnt write to file");
}

fn main() {
    let filename = format!("simulate-{}.txt", rand::thread_rng().gen::<usize>());
    println!("(emitting output to {})", filename);
    let mut f = File::create(filename).expect("couldn't make ouptput file");

    let n = 1024;
    let conf_thresh = 0.99999;
    let trials = 65536;

    output(&mut f, format!("searching for best bisect_poince~~"));
    output(&mut f, format!("n = {}, confidence goal = {}, trials = {}",
                           n, conf_thresh, trials));
    for &false_negative_prob in &[
        0.0,
        0.00001, 0.0001, 0.001, 0.01,
        0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,
        0.99, 0.999, 0.9999, 0.99999, // these will take FOREVER
    ] {
        output(&mut f, format!("==== trial for P(false negative) = {} ====",
                               false_negative_prob));
        for &bisect_point in &[
            0.30, 0.31, 0.32, 0.33, 0.34, 0.35, 0.36, 0.37, 0.38, 0.39,
            0.40, 0.41, 0.42, 0.43, 0.44, 0.45, 0.46, 0.47, 0.48, 0.49,
            0.50, 0.51, 0.52, 0.53, 0.54, 0.55,
        ] {
            let mut result_steps = vec![];
            let mut wrongs = 0;
            for _ in 0..trials {
                'inner: loop {
                    let s = SimulationState::new(n, false_negative_prob);
                    let c = CdfBisect::new(&s, bisect_point);
                    let res = s.simulate_til_confident(c, conf_thresh);
                    if res.suspected_buggy_commit == res.actual_buggy_commit {
                        result_steps.push(res.steps);
                        break 'inner;
                    } else {
                        wrongs += 1;
                    }
                }
            }
            let stddev = stats::stddev(result_steps.iter().cloned());
            let total_steps: usize = result_steps.iter().sum();
            let avg_steps = total_steps as f64 / trials as f64;
            output(&mut f, format!(
                "bisect @ {} -> {} steps ({} stddev; {} times wrong)",
                bisect_point, avg_steps, stddev, wrongs,
            ));
        }
    }
}
