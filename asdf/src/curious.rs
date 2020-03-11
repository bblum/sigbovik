// "Curious" strategies, which maximize information gain by simulating every option.

extern crate ordered_float;
use ordered_float::OrderedFloat;
use std::f64::NEG_INFINITY;

use crate::sim::{
    BisectAttempt,
    BisectStrategy,
    SimulationState,
};

pub struct MaximizePdf {}

impl BisectStrategy for MaximizePdf {
    fn select_commit(&mut self, state: &SimulationState) -> usize {
        let n = state.pdf.len();
        (0..n).max_by_key(|&commit| {
            // We get hypothetical pdfs we might get from probing this commit:
            let repro_pdf = state.hypothetical_repro_pdf(commit);
            let no_repro_pdf = state.hypothetical_no_repro_pdf(commit);

            // We guess the probability p of reproing the bug:
            let p_present = state.cdf[commit];
            let p_repro_if_present = 1.0 - state.false_negative_rate;
            let p = p_present * p_repro_if_present;

            // Compute the p-weighted average of our pdfs:
            let average_pdf = (0..n).map(|i|
                p * repro_pdf[i] + (1.0 - p) * no_repro_pdf[i]
            ).collect::<Vec<f64>>();

            // Our strategy: select the commit with largest max(average_pdf).
            OrderedFloat(average_pdf.iter().cloned().fold(NEG_INFINITY, f64::max))
        }).unwrap()
    }

    fn notify_result(&mut self, _result: BisectAttempt) {
        // Do nothing
    }
}
