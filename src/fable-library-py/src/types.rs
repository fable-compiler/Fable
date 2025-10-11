use core::convert::Into;
use pyo3::prelude::*;
use pyo3::types::{PyAny, PyType};
use std::sync::{Arc, RwLock};

pub fn register_types_module(parent_module: &Bound<'_, PyModule>) -> PyResult<()> {
    let py = parent_module.py();
    let m = PyModule::new(py, "types")?;

    m.add_class::<FSharpRef>()?;

    parent_module.add_submodule(&m)?;
    Ok(())
}

#[pyclass(module = "fable")]
#[derive(Debug)]
pub struct FSharpRef {
    getter: Py<PyAny>,
    setter: Py<PyAny>,
}

#[pymethods]
impl FSharpRef {
    #[new]
    #[pyo3(signature = (contents_or_getter, setter=None))]
    fn new(
        py: Python<'_>,
        contents_or_getter: Bound<'_, PyAny>,
        setter: Option<Bound<'_, PyAny>>,
    ) -> PyResult<Self> {
        // Test first if setter is callable, then create Self with either
        // the supplied callbacks or the ones we create
        if let Some(setter) = setter {
            if setter.is_callable() {
                // If setter is callable, use contents_or_getter as getter and provided setter
                let getter = contents_or_getter.into_pyobject(py)?.into();
                let setter = setter.into_pyobject(py)?.into();
                return Ok(Self { getter, setter });
            }
        }

        // If no setter is provided or setter is not callable, create default getter/setter
        let contents: Py<PyAny> = contents_or_getter.into_pyobject(py)?.into();
        let shared_contents = Arc::new(RwLock::new(contents));
        let getter = Getter {
            value: shared_contents.clone(),
        }
        .into_pyobject(py)?
        .into();
        let setter = Setter {
            value: shared_contents,
        }
        .into_pyobject(py)?
        .into();
        Ok(Self { getter, setter })
    }

    #[classmethod]
    fn __class_getitem__(cls: &Bound<'_, PyType>, _item: &Bound<'_, PyAny>) -> Py<PyAny> {
        cls.clone().unbind().into()
    }

    #[getter(contents)]
    pub fn get_contents(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        self.getter.call0(py)
    }

    #[setter(contents)]
    pub fn set_contents(&self, py: Python<'_>, value: Py<PyAny>) -> PyResult<()> {
        self.setter.call1(py, (value,))?;
        Ok(())
    }
}

#[pyclass]
struct Getter {
    value: Arc<RwLock<Py<PyAny>>>,
}

#[pymethods]
impl Getter {
    #[pyo3(signature = (*_args, **_kwargs))]
    fn __call__(
        &self,
        py: Python<'_>,
        _args: &Bound<'_, PyAny>,
        _kwargs: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<Py<PyAny>> {
        Ok(self.value.read().unwrap().clone_ref(py))
    }
}

#[pyclass]
struct Setter {
    value: Arc<RwLock<Py<PyAny>>>,
}

#[pymethods]
impl Setter {
    #[pyo3(signature = (*args, **_kwargs))]
    fn __call__(
        &self,
        args: &Bound<'_, PyAny>,
        _kwargs: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<()> {
        let new_value = args.get_item(0)?;
        *self.value.write().unwrap() = new_value.into();
        Ok(())
    }
}
