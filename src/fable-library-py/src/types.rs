#![allow(dead_code)]
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use pyo3::class::basic::CompareOp;
use pyo3::exceptions;
use pyo3::prelude::*;
use pyo3::types::PyBytes;

use byteorder::{BigEndian, ByteOrder, LittleEndian};

macro_rules! integer_variant {
    ($name:ident, $type:ty) => {
        #[pyclass(module = "fable", frozen)]
        #[derive(Clone)]
        pub struct $name($type);

        #[pymethods]
        impl $name {
            #[new]
            pub fn new(value: &PyAny) -> PyResult<Self> {
                match value.extract::<$type>() {
                    Ok(value) => Ok(Self(value)),
                    Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>(
                        "Cannot convert argument to integer",
                    )),
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
                let other = match other.extract::<$type>() {
                    Ok(other) => other,
                    Err(_) => {
                        return Err(PyErr::new::<exceptions::PyTypeError, _>(
                            "Cannot convert argument to integer",
                        ))
                    }
                };
                Ok($name(self.0 + other))
            }

            pub fn __radd__(&self, other: &PyAny) -> PyResult<$name> {
                self.__add__(other)
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

            pub fn __rsub__(&self, other: &PyAny) -> PyResult<$name> {
                self.__sub__(other)
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

            pub fn __rmul__(&self, other: &PyAny) -> PyResult<$name> {
                self.__mul__(other)
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

            pub fn __rshift__(&self, other: &PyAny) -> PyResult<$name> {
                let other = other.extract::<$type>()?;
                Ok($name(self.0 >> other))
            }

            pub fn __lshift__(&self, other: &PyAny) -> PyResult<$name> {
                let other = other.extract::<$type>()?;
                Ok($name(self.0 << other))
            }

            pub fn __and__(&self, other: &PyAny) -> PyResult<$name> {
                let other = match other.extract::<$name>() {
                    Ok(other) => Ok(other.0),
                    Err(_) => match other.extract::<$type>() {
                        Ok(other) => Ok(other),
                        Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot compare")),
                    },
                };

                match other {
                    Ok(other) => Ok($name(self.0 & other)),
                    Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot and")),
                }
            }

            pub fn __rand__(&self, other: &PyAny) -> PyResult<$name> {
                self.__and__(other)
            }

            pub fn __or__(&self, other: &PyAny) -> PyResult<$name> {
                let other = match other.extract::<$name>() {
                    Ok(other) => Ok(other.0),
                    Err(_) => match other.extract::<$type>() {
                        Ok(other) => Ok(other),
                        Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot compare")),
                    },
                };

                match other {
                    Ok(other) => Ok($name(self.0 | other)),
                    Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot or")),
                }
            }

            pub fn __ror__(&self, other: &PyAny) -> PyResult<$name> {
                self.__or__(other)
            }

            pub fn __xor__(&self, other: &PyAny) -> PyResult<$name> {
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

            pub fn __rxor__(&self, other: &PyAny) -> PyResult<$name> {
                self.__xor__(other)
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
        }
    };
}

integer_variant!(Int8, i8);
//integer_variant!(UInt8, u8);
integer_variant!(Int16, i16);
integer_variant!(UInt16, u16);
integer_variant!(Int32, i32);
integer_variant!(UInt32, u32);
//integer_variant!(Int64, i64);
//integer_variant!(UInt64, u64);

#[pyclass(module = "fable", frozen)]
#[derive(Clone)]
pub struct UInt8(u8);

#[pymethods]
impl UInt8 {
    #[new]
    pub fn new(value: &PyAny) -> PyResult<Self> {
        match value.extract::<u8>() {
            Ok(value) => Ok(Self(value)),
            Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>(
                "Cannot convert argument to integer",
            )),
        }
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
        Ok(PyBytes::new(py, &bytes).into())
    }

    pub fn __add__(&self, other: &PyAny) -> PyResult<UInt8> {
        let other = match other.extract::<u8>() {
            Ok(other) => other,
            Err(_) => match other.extract::<f64>() {
                Ok(_other) => {
                    //return Ok(other + (f64.from(self.0)));
                    return Err(PyErr::new::<exceptions::PyTypeError, _>(
                        "Cannot convert argument to float",
                    ));
                }
                Err(_) => {
                    return Err(PyErr::new::<exceptions::PyTypeError, _>(
                        "Cannot convert argument to integer",
                    ))
                }
            },
        };
        Ok(UInt8(self.0.wrapping_add(other)))
    }

    pub fn __radd__(&self, other: &PyAny) -> PyResult<UInt8> {
        self.__add__(other)
    }

    pub fn __sub__(&self, other: &PyAny) -> PyResult<UInt8> {
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

    pub fn __rsub__(&self, other: &PyAny) -> PyResult<UInt8> {
        self.__sub__(other)
    }

    pub fn __mul__(&self, other: &PyAny) -> PyResult<UInt8> {
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

    pub fn __rmul__(&self, other: &PyAny) -> PyResult<UInt8> {
        self.__mul__(other)
    }

    pub fn __truediv__(&self, other: &PyAny) -> PyResult<UInt8> {
        let other = other.extract::<u8>()?;
        Ok(UInt8(self.0 / other))
    }

    pub fn __richcmp__(&self, other: &PyAny, op: CompareOp) -> PyResult<bool> {
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

    pub fn __rshift__(&self, other: &PyAny) -> PyResult<UInt8> {
        let other = other.extract::<u8>()?;
        Ok(UInt8(self.0 >> other))
    }

    pub fn __lshift__(&self, other: &PyAny) -> PyResult<UInt8> {
        let other = other.extract::<u8>()?;
        Ok(UInt8(self.0 << other))
    }

    pub fn __and__(&self, other: &PyAny) -> PyResult<UInt8> {
        let other = match other.extract::<u8>() {
            Ok(other) => Ok(other),
            Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot compare")),
        };

        match other {
            Ok(other) => Ok(UInt8(self.0 & other)),
            Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot and")),
        }
    }

    pub fn __rand__(&self, other: &PyAny) -> PyResult<UInt8> {
        self.__and__(other)
    }

    pub fn __or__(&self, other: &PyAny) -> PyResult<UInt8> {
        let other = match other.extract::<u8>() {
            Ok(other) => Ok(other),
            Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot compare")),
        };

        match other {
            Ok(other) => Ok(UInt8(self.0 | other)),
            Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot or")),
        }
    }

    pub fn __ror__(&self, other: &PyAny) -> PyResult<UInt8> {
        self.__or__(other)
    }

    pub fn __xor__(&self, other: &PyAny) -> PyResult<UInt8> {
        let other = match other.extract::<u8>() {
            Ok(other) => Ok(other),
            Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot compare")),
        };

        match other {
            Ok(other) => Ok(UInt8(self.0 ^ other)),
            Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot xor")),
        }
    }

    pub fn __rxor__(&self, other: &PyAny) -> PyResult<UInt8> {
        self.__xor__(other)
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
}
