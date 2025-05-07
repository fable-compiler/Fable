use pyo3::prelude::*;

mod ints;
mod array;
mod datetime_offset;
mod floats;
mod options;

use crate::ints::*;
use crate::array::register_array_module;
use crate::datetime_offset::register_datetime_offset_module;
use crate::floats::*;
use crate::options::register_option_module;

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
    m.add_class::<Float32>()?;
    m.add_class::<Float64>()?;

    register_array_module(m)?;
    register_datetime_offset_module(m)?;
    register_option_module(m)?;

    Ok(())
}
