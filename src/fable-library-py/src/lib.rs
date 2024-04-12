use pyo3::prelude::*;

mod types;

use crate::types::*;

/// Formats the sum of two numbers as string.
#[pyfunction]
fn sum_as_string(a: usize, b: usize) -> PyResult<String> {
    Ok((a + b).to_string())
}

/// A Python module implemented in Rust.
#[pymodule]
fn _core(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(sum_as_string, m)?)?;
    m.add_class::<Int8>()?;
    m.add_class::<UInt8>()?;
    m.add_class::<Int16>()?;
    m.add_class::<UInt16>()?;
    m.add_class::<Int32>()?;
    m.add_class::<UInt32>()?;
    //m.add_class::<Int64>()?;
    //m.add_class::<UInt64>()?;
    Ok(())
}
