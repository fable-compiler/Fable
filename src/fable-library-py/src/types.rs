#![allow(dead_code)]
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use pyo3::class::basic::CompareOp;
use pyo3::exceptions;
use pyo3::prelude::*;
use pyo3::types::PyBytes;

use byteorder::{BigEndian, ByteOrder, LittleEndian};

macro_rules! integer_variant {
    ($name:ident, $type:ty, $extract_type:ty, $mask:expr) => {
        #[pyclass(module = "fable", frozen)]
        #[derive(Clone)]
        pub struct $name($type);

        #[pymethods]
        impl $name {
            #[new]
            pub fn new(value: &Bound<'_, PyAny>) -> PyResult<Self> {
                let value = value.call_method1("__and__", ($mask,))?;
                let value: PyResult<u32> = value.extract();
                match value {
                    Ok(value) => Ok(Self(value as $type)),
                    Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>(format!(
                        "Cannot convert argument to {}",
                        stringify!($name)
                    ))),
                }
            }

            pub fn __neg__(&self) -> PyResult<$name> {
                Ok($name(self.0.wrapping_neg()))
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
                Ok(PyBytes::new_bound(py, &bytes).into())
            }

            pub fn __add__(&self, other: &Bound<'_, PyAny>) -> PyResult<$name> {
                let other = match other.extract::<$type>() {
                    Ok(other) => other,
                    Err(_) => {
                        return Err(PyErr::new::<exceptions::PyTypeError, _>(
                            "Cannot convert argument to integer",
                        ))
                    }
                };
                Ok($name(self.0.wrapping_add(other)))
            }

            // For the case where we are on the right side of the operator we let the other
            // object handle the addition and turn ourselves into an integer.
            pub fn __radd__<'py>(&self, other: &Bound<'py, PyAny>) -> PyResult<Bound<'py, PyAny>> {
                other.add(self.0)
            }

            pub fn __sub__(&self, other: &Bound<'_, PyAny>) -> PyResult<$name> {
                let other = match other.extract::<$type>() {
                    Ok(other) => other,
                    Err(_) => {
                        return Err(PyErr::new::<exceptions::PyTypeError, _>(
                            "Cannot convert argument to integer",
                        ))
                    }
                };
                Ok($name(self.0 - other))
            }

            pub fn __rsub__<'py>(&self, other: &Bound<'py, PyAny>) -> PyResult<Bound<'py, PyAny>> {
                other.sub(self.0)
            }

            pub fn __mul__(&self, other: &Bound<'_, PyAny>) -> PyResult<$name> {
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

            pub fn __rmul__<'py>(&self, other: &Bound<'py, PyAny>) -> PyResult<Bound<'py, PyAny>> {
                other.mul(self.0)
            }

            pub fn __truediv__(&self, other: &Bound<'_, PyAny>) -> PyResult<$name> {
                let other = other.extract::<$type>()?;
                Ok($name(self.0 / other))
            }

            pub fn __floordiv__(&self, other: &Bound<'_, PyAny>) -> PyResult<$name> {
                let other = other.extract::<$type>()?;
                Ok($name(self.0 / other))
            }

            pub fn __mod__(&self, other: &Bound<'_, PyAny>) -> PyResult<$name> {
                let other = other.extract::<$type>()?;
                Ok($name(self.0 % other))
            }

            pub fn __invert__(&self) -> Self {
                Self(!self.0)
            }

            pub fn __inv__(&self) -> Self {
                Self(!self.0)
            }

            #[cfg(any(target_type = "i8", target_type = "i16", target_type = "i32"))]
            pub fn __abs__(&self) -> Self {
                Self(self.0.abs())
            }

            #[cfg(any(target_type = "u8", target_type = "u16", target_type = "u32"))]
            pub fn __abs__(&self) -> Self {
                Self(self.0)
            }

            pub fn __richcmp__(&self, other: &Bound<'_, PyAny>, op: CompareOp) -> PyResult<bool> {
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

            pub fn __rshift__(&self, other: u32) -> PyResult<$name> {
                Ok($name(self.0.wrapping_shr(other)))
            }

            pub fn __rrshift__<'py>(
                &self,
                other: &'py Bound<'py, PyAny>,
            ) -> PyResult<Bound<'py, PyAny>> {
                other.rshift(self.0)
            }

            pub fn __lshift__(&self, other: u32) -> PyResult<$name> {
                Ok($name(self.0.rotate_left(other)))
            }

            pub fn __rlshift__<'py>(
                &self,
                other: &'py Bound<'py, PyAny>,
            ) -> PyResult<Bound<'py, PyAny>> {
                other.lshift(self.0)
            }

            pub fn __and__(&self, other: &Bound<'_, PyAny>) -> PyResult<$name> {
                match other.extract::<u32>() {
                    Ok(other) => Ok($name((self.0 as u32 & other) as $type)),
                    Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot compare")),
                }
            }

            pub fn __rand__<'py>(
                &self,
                other: &'py Bound<'py, PyAny>,
            ) -> PyResult<Bound<'py, PyAny>> {
                other.bitand(self.0)
            }

            pub fn __or__(&self, other: &Bound<'_, PyAny>) -> PyResult<$name> {
                let other = match other.extract::<$type>() {
                    Ok(other) => Ok(other),
                    Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot compare")),
                };

                match other {
                    Ok(other) => Ok($name(self.0 | other)),
                    Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot or")),
                }
            }

            pub fn __ror__<'py>(
                &self,
                other: &'py Bound<'py, PyAny>,
            ) -> PyResult<Bound<'py, PyAny>> {
                other.bitor(self.0)
            }

            pub fn __xor__(&self, other: &Bound<'_, PyAny>) -> PyResult<$name> {
                let other = match other.extract::<$name>() {
                    Ok(other) => Ok(other.0),
                    Err(_) => match other.extract::<$type>() {
                        Ok(other) => Ok(other),
                        Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot compare")),
                    },
                };

                match other {
                    Ok(other) => Ok($name(self.0 ^ other)),
                    Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot xor")),
                }
            }

            pub fn __rxor__<'py>(
                &self,
                other: &'py Bound<'py, PyAny>,
            ) -> PyResult<Bound<'py, PyAny>> {
                other.bitxor(self.0)
            }

            fn __bool__(&self) -> bool {
                self.0 != 0
            }

            pub fn __hash__(&self) -> $type {
                let mut hasher = DefaultHasher::new();
                self.0.hash(&mut hasher);
                hasher.finish().try_into().unwrap()
            }

            pub fn __int__(&self) -> PyResult<$type> {
                Ok(self.0)
            }

            // Special method so that arbitrary objects can be used whenever integers
            // are explicitly needed in Python
            pub fn __index__(&self) -> PyResult<$type> {
                Ok(self.0)
            }

            pub fn __repr__(&self) -> PyResult<String> {
                Ok(self.0.to_string())
            }

            pub fn __str__(&self) -> PyResult<String> {
                Ok(self.0.to_string())
            }

            pub fn __format__<'py>(&self, py: Python<'py>, format: &str) -> PyResult<String> {
                // This is hard to implement so we just convert to a Python integer and let Python handle it
                let int = self.__int__()?;
                let int = int.into_py(py);
                let result = int.call_method1(py, "__format__", (format,))?;
                result.extract::<String>(py)
            }
        }
    };
}

//integer_variant!(UInt8, u8, u32, 0xff);
integer_variant!(Int8, i8, i32, 0xff_u32);
integer_variant!(UInt16, u16, u32, 0xffff_u32);
integer_variant!(Int16, i16, i32, 0xffff_u32);
integer_variant!(UInt32, u32, u32, 0xffffffff_u32);
integer_variant!(Int32, i32, i32, 0xffffffff_u32);
//integer_variant!(Int64, i64);
//integer_variant!(UInt64, u64);

// TODO: Remove code below. We only keep one non-macro integer type for
// TODO: now to make the IDE type inference work properly.
#[pyclass(module = "fable", frozen)]
#[derive(Clone)]
pub struct UInt8(u8);

#[pymethods]
impl UInt8 {
    #[new]
    pub fn new(value: &Bound<'_, PyAny>) -> PyResult<Self> {
        match value.extract::<u8>() {
            Ok(value) => Ok(Self(value)),
            Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>(
                "Cannot convert argument to integer",
            )),
        }
    }

    pub fn __neg__(&self) -> PyResult<Self> {
        Ok(UInt8(self.0.wrapping_neg()))
    }

    pub fn to_bytes(&self, py: Python, length: usize, byteorder: &str) -> PyResult<PyObject> {
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
        Ok(PyBytes::new_bound(py, &bytes).into())
    }

    pub fn __add__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
        let other = match other.extract::<u8>() {
            Ok(other) => other,
            Err(_) => {
                return Err(PyErr::new::<exceptions::PyTypeError, _>(
                    "Cannot convert argument to integer",
                ))
            }
        };
        Ok(UInt8(self.0.wrapping_add(other)))
    }

    pub fn __radd__<'py>(&self, other: &Bound<'py, PyAny>) -> PyResult<Bound<'py, PyAny>> {
        other.add(self.0)
    }

    pub fn __sub__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
        let other = match other.extract::<u8>() {
            Ok(other) => other,
            Err(_) => {
                return Err(PyErr::new::<exceptions::PyTypeError, _>(
                    "Cannot convert argument to integer",
                ))
            }
        };
        Ok(UInt8(self.0.wrapping_sub(other)))
    }

    pub fn __rsub__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
        self.__sub__(other)
    }

    pub fn __mul__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
        let other = match other.extract::<UInt8>() {
            Ok(other) => other.0,
            Err(_) => match other.extract::<u8>() {
                Ok(other) => other,
                Err(_) => {
                    return Err(PyErr::new::<exceptions::PyTypeError, _>(
                        "Cannot convert argument to integer",
                    ))
                }
            },
        };
        Ok(UInt8(self.0.wrapping_mul(other)))
    }

    pub fn __rmul__<'py>(&self, other: &Bound<'py, PyAny>) -> PyResult<Bound<'py, PyAny>> {
        other.mul(self.0)
    }

    pub fn __truediv__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
        let other = other.extract::<u8>()?;
        Ok(UInt8(self.0 / other))
    }

    pub fn __floordiv__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
        let other = other.extract::<u8>()?;
        Ok(UInt8(self.0 / other))
    }

    pub fn __mod__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
        let other = other.extract::<u8>()?;
        Ok(UInt8(self.0 % other))
    }

    pub fn __invert__(&self) -> Self {
        UInt8(!self.0)
    }

    pub fn __richcmp__(&self, other: &Bound<'_, PyAny>, op: CompareOp) -> PyResult<bool> {
        let other = match other.extract::<u8>() {
            Ok(other) => Ok(other),
            Err(_) => match other.extract::<f64>() {
                Ok(other) => Ok(other as u8),
                Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot compare")),
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

    pub fn __rshift__(&self, other: u32) -> PyResult<Self> {
        Ok(UInt8(self.0.wrapping_shr(other)))
    }

    // Return a python object not u8. First try to extract for a Python integer, then for a Rust u8
    pub fn __rrshift__<'py>(
        &self,
        py: Python<'py>,
        other: &'py Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        let result = match other.extract::<UInt8>() {
            Ok(other) => {
                let shifted = other.0.wrapping_shr(self.0.into());
                Ok(UInt8(shifted).into_py(py).into_bound(py))
            }
            Err(_) => match other.extract::<i32>() {
                Ok(other) => {
                    let shifted = other.wrapping_shr(self.0.into());
                    Ok(Int32(shifted).into_py(py).into_bound(py))
                }
                Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot shift")),
            },
        };
        result
    }

    pub fn __lshift__(&self, other: u32) -> PyResult<Self> {
        Ok(UInt8(self.0.rotate_left(other)))
    }

    pub fn __rlshift__<'py>(&self, other: &'py Bound<'py, PyAny>) -> PyResult<Bound<'py, PyAny>> {
        other.lshift(self.0)
    }

    pub fn __and__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
        let other = match other.extract::<u8>() {
            Ok(other) => Ok(other),
            Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot compare")),
        };

        match other {
            Ok(other) => Ok(UInt8(self.0 & other)),
            Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot and")),
        }
    }

    pub fn __rand__<'py>(&self, other: &'py Bound<'py, PyAny>) -> PyResult<Bound<'py, PyAny>> {
        other.bitand(self.0)
    }

    pub fn __or__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
        let other = match other.extract::<u8>() {
            Ok(other) => Ok(other),
            Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot compare")),
        };

        match other {
            Ok(other) => Ok(UInt8(self.0 | other)),
            Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot or")),
        }
    }

    pub fn __ror__<'py>(&self, other: &'py Bound<'py, PyAny>) -> PyResult<Bound<'py, PyAny>> {
        other.bitor(self.0)
    }

    pub fn __xor__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
        let other = match other.extract::<u8>() {
            Ok(other) => Ok(other),
            Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot compare")),
        };

        match other {
            Ok(other) => Ok(UInt8(self.0 ^ other)),
            Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot xor")),
        }
    }

    pub fn __rxor__<'py>(&self, other: &'py Bound<'py, PyAny>) -> PyResult<Bound<'py, PyAny>> {
        other.bitxor(self.0)
    }

    fn __bool__(&self) -> bool {
        self.0 != 0
    }

    pub fn __hash__(&self) -> u8 {
        let mut hasher = DefaultHasher::new();
        self.0.hash(&mut hasher);
        hasher.finish().try_into().unwrap()
    }

    pub fn __int__(&self) -> PyResult<u8> {
        Ok(self.0)
    }

    // Special method so that arbitrary objects can be used whenever integers
    // are explicitly needed in Python
    pub fn __index__(&self) -> PyResult<u8> {
        Ok(self.0)
    }

    pub fn __repr__(&self) -> PyResult<String> {
        Ok(self.0.to_string())
    }

    pub fn __str__(&self) -> PyResult<String> {
        Ok(self.0.to_string())
    }

    pub fn __format__<'py>(&self, py: Python<'py>, format: &str) -> PyResult<String> {
        // This is hard to implement so we just convert to a Python integer and let Python handle it
        let int = self.__int__()?;
        let int = int.into_py(py);
        let result = int.call_method1(py, "__format__", (format,))?;
        result.extract::<String>(py)
    }
}
