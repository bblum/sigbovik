mod cdfbisect;
mod entropy;
mod linear;
mod naive;
mod random;

pub use cdfbisect::CdfBisect;
pub use entropy::MinExpectedEntropy;
pub use linear::LinearSearch;
pub use naive::NaiveBinarySearch;
pub use random::{ChooseRandomly, How};
