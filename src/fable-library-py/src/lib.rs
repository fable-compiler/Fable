use pyo3::prelude::*;

mod array;
mod datetime_offset;
mod floats;
mod ints;
mod native_array;
mod options;
mod strings;
mod types;
mod util;

use crate::array::register_array_module;
use crate::datetime_offset::register_datetime_offset_module;
use crate::floats::*;
use crate::ints::*;
use crate::options::register_option_module;
use crate::strings::register_string_module;
use crate::types::*;

/// A Python module implemented in Rust.
#[pymodule]
fn _core(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<Int8>()?;
    m.add_class::<UInt8>()?;
    m.add_class::<Int16>()?;
    m.add_class::<UInt16>()?;
    m.add_class::<Int32>()?;
    m.add_class::<UInt32>()?;
    m.add_class::<Int64>()?;
    m.add_class::<UInt64>()?;

    register_array_module(m)?;
    register_datetime_offset_module(m)?;
    register_option_module(m)?;
    register_float_module(m)?;
    register_string_module(m)?;
    register_types_module(m)?;
    register_int_module(m)?;

    Ok(())
}
