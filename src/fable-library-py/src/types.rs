#![allow(dead_code)]
use byteorder::{BigEndian, ByteOrder, LittleEndian};
use pyo3::class::basic::CompareOp;
use pyo3::exceptions;
use pyo3::prelude::*;
use pyo3::types::PyBytes;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

macro_rules! integer_variant {
    ($name:ident, $type:ty) => {
        #[pyclass]
        #[derive(Clone)]
        pub struct $name($type);

        #[pymethods]
        impl $name {
            #[new]
            pub fn new(value: &PyAny) -> PyResult<Self> {
                match value.extract::<$name>() {
                    Ok(value) => Ok(Self(value.0)),
                    Err(_) => match value.extract::<$type>() {
                        Ok(value) => Ok(Self(value)),
                        Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>(
                            "Cannot convert argument to integer",
                        )),
                    },
                }
            }

            pub fn to_bytes(
                &self,
                py: Python,
                length: usize,
                byteorder: &str,
            ) -> PyResult<PyObject> {
                let mut bytes = vec![0; length];
                match byteorder {
                    "little" => LittleEndian::write_uint(&mut bytes, self.0 as u64, length),
                    "big" => BigEndian::write_uint(&mut bytes, self.0 as u64, length),
                    _ => {
                        return Err(PyErr::new::<exceptions::PyValueError, _>(
                            "Invalid byteorder",
                        ))
                    }
                }
                Ok(PyBytes::new(py, &bytes).into())
            }

            pub fn __add__(&self, other: &PyAny) -> PyResult<$name> {
                let other = match other.extract::<$name>() {
                    Ok(other) => other.0,
                    Err(_) => match other.extract::<$type>() {
                        Ok(other) => other,
                        Err(_) => {
                            return Err(PyErr::new::<exceptions::PyTypeError, _>(
                                "Cannot convert argument to integer",
                            ))
                        }
                    },
                };
                Ok($name(self.0 + other))
            }

            pub fn __sub__(&self, other: &PyAny) -> PyResult<$name> {
                let other = match other.extract::<$name>() {
                    Ok(other) => other.0,
                    Err(_) => match other.extract::<$type>() {
                        Ok(other) => other,
                        Err(_) => {
                            return Err(PyErr::new::<exceptions::PyTypeError, _>(
                                "Cannot convert argument to integer",
                            ))
                        }
                    },
                };
                Ok($name(self.0 - other))
            }

            pub fn __mul__(&self, other: &PyAny) -> PyResult<$name> {
                let other = match other.extract::<$name>() {
                    Ok(other) => other.0,
                    Err(_) => match other.extract::<$type>() {
                        Ok(other) => other,
                        Err(_) => {
                            return Err(PyErr::new::<exceptions::PyTypeError, _>(
                                "Cannot convert argument to integer",
                            ))
                        }
                    },
                };
                Ok($name(self.0 * other))
            }

            pub fn __truediv__(&self, other: &PyAny) -> PyResult<$name> {
                let other = other.extract::<$type>()?;
                Ok($name(self.0 / other))
            }

            pub fn __richcmp__(&self, other: &PyAny, op: CompareOp) -> PyResult<bool> {
                let other = match other.extract::<$name>() {
                    Ok(other) => Ok(other.0),
                    Err(_) => match other.extract::<$type>() {
                        Ok(other) => Ok(other),
                        Err(_) => match other.extract::<f64>() {
                            Ok(other) => Ok(other as $type),
                            Err(_) => {
                                Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot compare"))
                            }
                        },
                    },
                };

                match other {
                    Ok(other) => match op {
                        CompareOp::Eq => Ok(self.0 == other),
                        CompareOp::Ne => Ok(self.0 != other),
                        CompareOp::Lt => Ok(self.0 < other),
                        CompareOp::Le => Ok(self.0 <= other),
                        CompareOp::Gt => Ok(self.0 > other),
                        CompareOp::Ge => Ok(self.0 >= other),
                    },
                    Err(_) => Ok(false),
                }
            }

            fn __bool__(&self) -> bool {
                self.0 != 0
            }

            pub fn __hash__(&self) -> u64 {
                let mut hasher = DefaultHasher::new();
                self.0.hash(&mut hasher);
                hasher.finish()
            }

            pub fn __int__(&self) -> PyResult<$type> {
                Ok(self.0)
            }

            pub fn __repr__(&self) -> PyResult<String> {
                Ok(self.0.to_string())
            }

            pub fn __str__(&self) -> PyResult<String> {
                Ok(self.0.to_string())
            }
        }
    };
}

integer_variant!(Int16, i16);
integer_variant!(UInt16, u16);
