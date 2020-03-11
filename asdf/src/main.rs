mod cdfbisect;
mod sim;
mod naive;

use cdfbisect::CdfBisect;
use naive::NaiveBinarySearch;
use sim::SimulationState;

fn main() {
    // let s = SimulationState::new(65536, 0.0);
    // let res = s.simulate_n_steps(NaiveBinarySearch::new(), 1_000_000);
    // println!("{:?}", res);

    let s = SimulationState::new(1024, 0.0);
    let nb = NaiveBinarySearch::new(&s);
    let res = s.simulate_til_confident(nb, 0.99);
    println!("{:?}", res);

    let s = SimulationState::new(8, 0.5);
    let c = CdfBisect::new(&s);
    let res = s.simulate_til_confident(c, 0.99);
    println!("{:?}", res);
}
