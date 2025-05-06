#![allow(dead_code)]
use pyo3::class::basic::CompareOp;
use pyo3::exceptions;
use pyo3::prelude::*;
use std::ops::Deref;
use std::hash::{Hash, Hasher};
use std::collections::hash_map::DefaultHasher; // Using the same hasher as integers for consistency, though float hashing needs care.
use pyo3::types::PyNotImplemented;
use pyo3::types::PyBool;
use pyo3::BoundObject;

// Macro to generate float wrapper types (Float32, Float64)
macro_rules! float_variant {
    ($name:ident, $type:ty) => {
        #[pyclass(module = "fable", frozen)]
        #[derive(Clone, Copy)] // Floats are typically Copy
        pub struct $name(pub $type);

        // Implement Deref trait for easy access to the inner value
        impl Deref for $name {
            type Target = $type;
            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        #[pymethods]
        impl $name {
            #[new]
            pub fn new(value: $type) -> PyResult<Self> {
                Ok(Self(value))
            }

            // --- Arithmetic Methods ---
            pub fn __add__(&self, other: $type) -> PyResult<Self> {
                Ok(Self(self.0 + other))
            }

            pub fn __radd__(&self, other: $type) -> PyResult<Self> {
                // Addition is commutative for floats
                self.__add__(other)
            }

            pub fn __sub__(&self, other: $type) -> PyResult<Self> {
                Ok(Self(self.0 - other))
            }

            pub fn __rsub__(&self, other: $type) -> PyResult<Self> {
                 Ok(Self(other - self.0))
            }

            pub fn __mul__(&self, other: $type) -> PyResult<Self> {
                Ok(Self(self.0 * other))
            }

             pub fn __rmul__(&self, other: $type) -> PyResult<Self> {
                // Multiplication is commutative
                self.__mul__(other)
            }

            // In .NET, division by zero with floating point numbers returns infinity
            // instead of raising an exception
            pub fn __truediv__(&self, other: $type) -> PyResult<Self> {
                return Ok(Self(self.0 / other));
            }

            pub fn __rtruediv__(&self, other: $type) -> PyResult<Self> {
                // In .NET, division by zero returns infinity
                Ok(Self(other / self.0))
            }

            // Note: __floordiv__ for floats is often less intuitive.
            // In .NET, division by zero with floating point numbers returns infinity
            pub fn __floordiv__(&self, other: $type) -> PyResult<Self> {
                Ok(Self((self.0 / other).floor()))
            }

            pub fn __rfloordiv__(&self, other: $type) -> PyResult<Self> {
                // In .NET, division by zero returns infinity
                Ok(Self((other / self.0).floor()))
            }

            // Modulo for floats: a % b == a - floor(a / b) * b
            // In .NET, modulo by zero returns NaN
            pub fn __mod__(&self, other: $type) -> PyResult<Self> {
                Ok(Self(self.0 % other))
            }

            pub fn __rmod__(&self, other: $type) -> PyResult<Self> {
                // In .NET, modulo by zero returns NaN
                Ok(Self(other % self.0))
            }

            pub fn __pow__(&self, other: $type, modulo: Option<$type>) -> PyResult<Self> {
                if modulo.is_some() {
                    return Err(PyErr::new::<exceptions::PyTypeError, _>(
                        "pow() with modulo not supported for floats"
                    ));
                }

                Ok(Self(self.0.powf(other)))
            }

            // __rpow__ is tricky as order matters. Python typically handles this via reflection.
            // Let Python handle a ** self for now if the left operand isn't our type.

            pub fn __neg__(&self) -> Self {
                Self(-self.0)
            }

            pub fn __pos__(&self) -> Self {
                *self // Positive of a float is itself
            }

            pub fn __abs__(&self) -> Self {
                Self(self.0.abs())
            }

            pub fn __floor__(&self) -> Self {
                Self(self.0.floor())
            }

            pub fn __ceil__(&self) -> Self {
                Self(self.0.ceil())
            }

            pub fn __round__(&self) -> Self {
                Self(self.0.round())
            }

            pub fn __trunc__(&self) -> Self {
                Self(self.0.trunc())
            }

            // --- Comparison ---
            fn __richcmp__<'py>(&self, other: &Bound<'_, PyAny>, op: CompareOp, py: Python<'py>) -> PyResult<Borrowed<'py, 'py, PyAny>> {
                // Try to convert other to our type first
                if let Ok(other_float) = other.extract::<$type>() {
                    let result = match op {
                        CompareOp::Eq => self.0 == other_float,
                        CompareOp::Ne => self.0 != other_float,
                        CompareOp::Lt => self.0 < other_float,
                        CompareOp::Le => self.0 <= other_float,
                        CompareOp::Gt => self.0 > other_float,
                        CompareOp::Ge => self.0 >= other_float,
                    };
                    return Ok(PyBool::new(py, result).into_any());
                }
                // Return NotImplemented to let Python try the other object's comparison
                Ok(PyNotImplemented::get(py).into_any())
            }

            // --- Hashing ---
            // Be careful with float hashing due to precision issues and NaN/Infinity.
            // Python's float hash handles NaN, +/-Inf specially.
            // A simple approach is to convert to bits, but this differs from Python's hash.
            // For consistency with Python, converting to Python float and hashing might be best,
            // but let's use a bit-based hash for now, acknowledging the difference.
             pub fn __hash__(&self) -> PyResult<isize> {
                 // Handle NaN and Infinity specially if needed, similar to Python.
                 // Python hash(float('nan')) == 0
                 // Python hash(float('inf')) == some large int (sys.hash_info.inf)
                 // Python hash(float('-inf')) == some large negative int (sys.hash_info.inf)
                 if self.0.is_nan() {
                     Ok(0) // Python's hash for NaN
                 } else if self.0.is_infinite() {
                     // Use Python's sys.hash_info.inf constants if possible, otherwise approximate
                     // For simplicity here, use a fixed large value. A better way involves PySys_GetHashInfo
                     if self.0.is_sign_positive() {
                         Ok(314159) // Placeholder for sys.hash_info.inf
                     } else {
                         Ok(-271828) // Placeholder for -sys.hash_info.inf
                     }
                 } else {
                     // Use DefaultHasher on the bit representation for non-special floats
                     let mut hasher = DefaultHasher::new();
                     self.0.to_bits().hash(&mut hasher);
                     // Convert u64 hash to isize. Might truncate on 32-bit systems.
                     Ok(hasher.finish() as isize)
                 }
            }


            // --- Conversions ---
            pub fn __bool__(&self) -> bool {
                self.0 != 0.0
            }

            // Coercions

            // Convert to Python's built-in float
            pub fn __float__(&self) -> PyResult<f64> {
                Ok(self.0 as f64) // Convert f32 to f64 if necessary
            }

            pub fn __index__(&self) -> PyResult<isize> {
                if self.0.is_nan() || self.0.is_infinite() {
                    return Err(PyErr::new::<exceptions::PyValueError, _>(
                        "cannot convert float NaN or infinity to integer"
                    ));
                }
                Ok(self.0.trunc() as isize)
            }

            // Convert to Python's built-in int (truncates)
            pub fn __int__(&self) -> PyResult<isize> {
                 return self.__index__();
            }

            pub fn __repr__(&self) -> String {
                // Use Rust's default float formatting, which is generally good.
                // {:?} might sometimes add type suffix, .to_string() is usually preferred.
                self.0.to_string()
            }

            pub fn __str__(&self) -> String {
                self.0.to_string()
            }

            pub fn __format__(&self, format_spec: &str) -> PyResult<String> {
                // Handle format specifiers like .2f
                if format_spec.is_empty() {
                    return Ok(self.__str__());
                }

                // Parse the format specifier
                let mut precision = None;
                let mut format_type = None;

                // Simple parsing of format specifier
                let mut parts = format_spec.chars().peekable();

                // Check for precision specifier (e.g., .2)
                if let Some(&'.') = parts.peek() {
                    parts.next(); // consume the '.'
                    let mut precision_str = String::new();
                    while let Some(&c) = parts.peek() {
                        if c.is_ascii_digit() {
                            precision_str.push(c);
                            parts.next();
                        } else {
                            break;
                        }
                    }
                    if !precision_str.is_empty() {
                        precision = Some(precision_str.parse::<usize>().map_err(|_| {
                            PyErr::new::<exceptions::PyValueError, _>("Invalid precision in format specifier")
                        })?);
                    }
                }

                // Check for format type (e.g., f, e, g)
                if let Some(&c) = parts.peek() {
                    format_type = Some(c);
                    parts.next();
                }

                // Format based on the parsed specifiers
                match format_type {
                    Some('f') | None => {
                        if let Some(prec) = precision {
                            Ok(format!("{:.1$}", self.0, prec))
                        } else {
                            Ok(format!("{}", self.0))
                        }
                    },
                    Some('e') => {
                        if let Some(prec) = precision {
                            Ok(format!("{:.1$e}", self.0, prec))
                        } else {
                            Ok(format!("{:e}", self.0))
                        }
                    },
                    Some('g') => {
                        if let Some(prec) = precision {
                            Ok(format!("{:.1$?}", self.0, prec))
                        } else {
                            Ok(format!("{:?}", self.0))
                        }
                    },
                    Some(c) => Err(PyErr::new::<exceptions::PyValueError, _>(
                        format!("Unknown format code '{}' for {}", c, stringify!($name))
                    )),
                }
            }

            // --- Math operations ---
            pub fn floor(&self) -> Self {
                Self(self.0.floor())
            }

            pub fn ceil(&self) -> Self {
                Self(self.0.ceil())
            }

            // public static double Round(double value, int digits);
            #[pyo3(signature = (digits = None))]
            pub fn round(&self, digits: Option<usize>) -> Self {
                match digits {
                    Some(d) => {
                        let factor = 10.0_f64.powi(d as i32) as $type;
                        Self((self.0 * factor).round_ties_even() / factor)
                    }
                    None => Self(self.0.round_ties_even()),
                }
            }

            pub fn sqrt(&self) -> PyResult<Self> {
                if self.0 < 0.0 {
                    return Err(PyErr::new::<exceptions::PyValueError, _>(
                        "cannot compute square root of negative number"
                    ));
                }
                Ok(Self(self.0.sqrt()))
            }

            pub fn cos(&self) -> Self {
                Self(self.0.cos())
            }
            pub fn sin(&self) -> Self {
                Self(self.0.sin())
            }
            pub fn tan(&self) -> Self {
                Self(self.0.tan())
            }
            pub fn acos(&self) -> PyResult<Self> {
                if self.0 < -1.0 || self.0 > 1.0 {
                    return Err(PyErr::new::<exceptions::PyValueError, _>(
                        "acos() domain error"
                    ));
                }
                Ok(Self(self.0.acos()))
            }
            pub fn asin(&self) -> PyResult<Self> {
                if self.0 < -1.0 || self.0 > 1.0 {
                    return Err(PyErr::new::<exceptions::PyValueError, _>(
                        "asin() domain error"
                    ));
                }
                Ok(Self(self.0.asin()))
            }
            pub fn atan(&self) -> Self {
                Self(self.0.atan())
            }
            pub fn atan2(&self, other: $type) -> PyResult<Self> {
                Ok(Self(self.0.atan2(other)))
            }
            pub fn exp(&self) -> Self {
                Self(self.0.exp())
            }
            #[pyo3(signature = (base = None))]
            pub fn log(&self, base: Option<$type>) -> PyResult<Self> {
                if self.0 <= 0.0 {
                    return Err(PyErr::new::<exceptions::PyValueError, _>(
                        "log() domain error"
                    ));
                }
                let base_val = match base {
                    Some(b) => b,
                    None => std::f64::consts::E as $type,
                };
                if base_val <= 1.0 {
                    return Err(PyErr::new::<exceptions::PyValueError, _>(
                        "log() base must be greater than 1"
                    ));
                }
                Ok(Self(self.0.log(base_val)))
            }
            pub fn log10(&self) -> PyResult<Self> {
                if self.0 <= 0.0 {
                    return Err(PyErr::new::<exceptions::PyValueError, _>(
                        "log10() domain error"
                    ));
                }
                Ok(Self(self.0.log(10.0)))
            }
            pub fn log2(&self) -> PyResult<Self> {
                if self.0 <= 0.0 {
                    return Err(PyErr::new::<exceptions::PyValueError, _>(
                        "log2() domain error"
                    ));
                }
                Ok(Self(self.0.log(2.0)))
            }
            pub fn degrees(&self) -> Self {
                Self(self.0.to_degrees())
            }

            pub fn radians(&self) -> Self {
                Self(self.0.to_radians())
            }


            // --- Helper ---
            pub fn value(&self) -> $type {
                self.0
            }

            // Check if the value is NaN (Not a Number)
            pub fn is_nan(&self) -> bool {
                self.0.is_nan()
            }

            // Check if the value is infinity (positive or negative)
            pub fn is_infinity(&self) -> bool {
                self.0.is_infinite()
            }

            // Check if the value is positive infinity
            pub fn is_positive_infinity(&self) -> bool {
                self.0.is_infinite() && self.0.is_sign_positive()
            }

            // Check if the value is negative infinity
            pub fn is_negative_infinity(&self) -> bool {
                self.0.is_infinite() && self.0.is_sign_negative()
            }
        }
    };
}

// Instantiate the float types using the macro
float_variant!(Float32, f32);
float_variant!(Float64, f64);
