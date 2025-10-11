use core::convert::Into;
use pyo3::class::basic::CompareOp;
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::types::{PyAny, PyList, PyType};

use pyo3::IntoPyObjectExt;

pub fn register_option_module(parent_module: &Bound<'_, PyModule>) -> PyResult<()> {
    let py = parent_module.py();
    let m = PyModule::new(py, "option")?;

    m.add_class::<SomeWrapper>()?;

    // Add functions
    m.add_function(wrap_pyfunction!(default_arg, &m)?)?;
    m.add_function(wrap_pyfunction!(default_arg_with, &m)?)?;
    m.add_function(wrap_pyfunction!(filter, &m)?)?;
    m.add_function(wrap_pyfunction!(map, &m)?)?;
    m.add_function(wrap_pyfunction!(map2, &m)?)?;
    m.add_function(wrap_pyfunction!(map3, &m)?)?;
    m.add_function(wrap_pyfunction!(some, &m)?)?;
    m.add_function(wrap_pyfunction!(value, &m)?)?;
    m.add_function(wrap_pyfunction!(of_nullable, &m)?)?;
    m.add_function(wrap_pyfunction!(to_nullable, &m)?)?;
    m.add_function(wrap_pyfunction!(flatten, &m)?)?;
    m.add_function(wrap_pyfunction!(to_array, &m)?)?;
    m.add_function(wrap_pyfunction!(bind, &m)?)?;
    m.add_function(wrap_pyfunction!(or_else, &m)?)?;
    m.add_function(wrap_pyfunction!(or_else_with, &m)?)?;
    m.add_function(wrap_pyfunction!(non_null, &m)?)?;

    parent_module.add_submodule(&m)?;
    Ok(())
}

#[pyclass(module = "fable", frozen)]
#[derive(Debug)]
pub struct SomeWrapper {
    #[pyo3(get)]
    value: Py<PyAny>,
}

impl PartialEq for SomeWrapper {
    fn eq(&self, other: &Self) -> bool {
        Python::attach(|py| {
            self.value
                .bind(py)
                .eq(other.value.bind(py))
                .unwrap_or(false)
        })
    }
}

#[pymethods]
impl SomeWrapper {
    #[new]
    pub fn new(value: Py<PyAny>) -> Self {
        SomeWrapper { value }
    }

    fn __richcmp__<'py>(
        &self,
        other: &Bound<'_, PyAny>,
        op: CompareOp,
        py: Python<'py>,
    ) -> PyResult<bool> {
        let other_value = extract_value(py, other)?;
        let result = self.value.bind(py).rich_compare(other_value, op)?;
        result.is_truthy()
    }

    pub fn __repr__(&self, py: Python<'_>) -> PyResult<String> {
        let value_repr = self.value.bind(py).repr()?;
        let value_str = value_repr.to_string();
        Ok(format!("Some {}", value_str))
    }

    pub fn __str__(&self, py: Python<'_>) -> PyResult<String> {
        let value_str = self.value.bind(py).str()?.to_string();
        Ok(format!("Some {}", value_str))
    }

    #[classmethod]
    fn __class_getitem__(cls: &Bound<'_, PyType>, _item: &Bound<'_, PyAny>) -> Py<PyAny> {
        cls.clone().unbind().into()
    }
}

// Helper function to check if an object is a Some wrapper
fn is_some_wrapper(py: Python<'_>, obj: &Bound<'_, PyAny>) -> bool {
    obj.is_instance(&py.get_type::<SomeWrapper>())
        .unwrap_or(false)
}

// Helper function to extract value from an option (None or SomeWrapper)
fn extract_value(py: Python<'_>, opt: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    if opt.is_none() {
        Err(PyValueError::new_err("Option has no value"))
    } else if is_some_wrapper(py, opt) {
        let wrapper = opt.extract::<Py<SomeWrapper>>()?;
        let wrapper_ref = wrapper.borrow(py);
        Ok(wrapper_ref.value.clone_ref(py))
    } else {
        opt.into_py_any(py)
    }
}

// Helper function to wrap a value in a SomeWrapper or return as is
fn wrap_value(py: Python<'_>, x: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    // Check if x is None or an instance of Some
    if x.is_none() || is_some_wrapper(py, x) {
        // Create a new SomeWrapper instance
        let some_type = py.get_type::<SomeWrapper>();
        let some_instance = some_type.call1((x,))?;
        some_instance.into_py_any(py)
    } else {
        // If it's neither None nor a Some wrapper, just return x as is
        x.into_py_any(py)
    }
}

#[pyfunction]
fn default_arg(
    py: Python<'_>,
    opt: &Bound<'_, PyAny>,
    default_value: Py<PyAny>,
) -> PyResult<Py<PyAny>> {
    if opt.is_none() {
        Ok(default_value)
    } else {
        extract_value(py, opt)
    }
}

#[pyfunction]
fn default_arg_with(
    py: Python<'_>,
    opt: &Bound<'_, PyAny>,
    def_thunk: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    if opt.is_none() {
        // Call the thunk function to get the default value
        Ok(def_thunk.call0()?.into())
    } else {
        extract_value(py, opt)
    }
}

#[pyfunction]
fn filter(
    py: Python<'_>,
    predicate: &Bound<'_, PyAny>,
    opt: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    if opt.is_none() {
        return Ok(py.None());
    }

    let val = extract_value(py, opt)?;
    let result = predicate.call1((val.clone_ref(py),))?;

    if result.is_truthy()? {
        opt.into_py_any(py)
    } else {
        Ok(py.None())
    }
}

#[pyfunction]
fn map(py: Python<'_>, mapping: &Bound<'_, PyAny>, opt: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    if opt.is_none() {
        return Ok(py.None());
    }

    let val = extract_value(py, opt)?;
    let result = mapping.call1((val,))?;

    wrap_value(py, &result)
}

#[pyfunction]
fn map2(
    py: Python<'_>,
    mapping: &Bound<'_, PyAny>,
    opt1: &Bound<'_, PyAny>,
    opt2: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    if opt1.is_none() || opt2.is_none() {
        return Ok(py.None());
    }

    let val1 = extract_value(py, opt1)?;
    let val2 = extract_value(py, opt2)?;

    Ok(mapping.call1((val1, val2))?.into())
}

#[pyfunction]
fn map3(
    py: Python<'_>,
    mapping: &Bound<'_, PyAny>,
    opt1: &Bound<'_, PyAny>,
    opt2: &Bound<'_, PyAny>,
    opt3: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    if opt1.is_none() || opt2.is_none() || opt3.is_none() {
        return Ok(py.None());
    }

    let val1 = extract_value(py, opt1)?;
    let val2 = extract_value(py, opt2)?;
    let val3 = extract_value(py, opt3)?;

    Ok(mapping.call1((val1, val2, val3))?.into())
}

#[pyfunction]
fn some(py: Python<'_>, x: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    wrap_value(py, x)
}

#[pyfunction]
fn value(py: Python<'_>, x: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    extract_value(py, x)
}

#[pyfunction]
fn of_nullable(py: Python<'_>, x: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    // Just return the object as is
    x.into_py_any(py)
}

#[pyfunction]
fn to_nullable(py: Python<'_>, x: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    if x.is_none() {
        Ok(py.None())
    } else {
        extract_value(py, x)
    }
}

#[pyfunction]
fn flatten(py: Python<'_>, x: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    if x.is_none() {
        Ok(py.None())
    } else {
        extract_value(py, x)
    }
}

#[pyfunction]
fn to_array(py: Python<'_>, opt: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    if opt.is_none() {
        // Return empty list
        let empty_list = PyList::empty(py);
        empty_list.into_py_any(py)
    } else {
        let val = extract_value(py, opt)?;
        let list = PyList::new(py, &[val])?;
        list.into_py_any(py)
    }
}

#[pyfunction]
fn bind(py: Python<'_>, binder: &Bound<'_, PyAny>, opt: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    if opt.is_none() {
        Ok(py.None())
    } else {
        let val = extract_value(py, opt)?;
        binder.call1((val,))?.into_py_any(py)
    }
}

#[pyfunction]
fn or_else(
    py: Python<'_>,
    opt: &Bound<'_, PyAny>,
    if_none: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    if opt.is_none() {
        if_none.into_pyobject(py)?.into_py_any(py)
    } else {
        opt.into_py_any(py)
    }
}

#[pyfunction]
fn or_else_with(
    py: Python<'_>,
    opt: &Bound<'_, PyAny>,
    if_none_thunk: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    if opt.is_none() {
        if_none_thunk.call0()?.into_py_any(py)
    } else {
        opt.into_py_any(py)
    }
}

#[pyfunction]
fn non_null(py: Python<'_>, x: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    if x.is_none() {
        Err(PyValueError::new_err("Value cannot be null"))
    } else {
        x.into_py_any(py)
    }
}
