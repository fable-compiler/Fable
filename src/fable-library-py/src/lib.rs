use pyo3::prelude::*;

mod types;
mod array;

use crate::types::*;
use crate::array::register_array_module;

/// A Python module implemented in Rust.
#[pymodule]
fn _core(_py: Python, m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<Int8>()?;
    m.add_class::<UInt8>()?;
    m.add_class::<Int16>()?;
    m.add_class::<UInt16>()?;
    m.add_class::<Int32>()?;
    m.add_class::<UInt32>()?;
    m.add_class::<Int64>()?;
    m.add_class::<UInt64>()?;

    register_array_module(m)?;
    Ok(())
}
