#![allow(dead_code)]
use pyo3::class::basic::CompareOp;
use pyo3::exceptions;
use pyo3::prelude::*;
use std::ops::Deref;
use std::hash::{Hash, Hasher};
use std::collections::hash_map::DefaultHasher; // Using the same hasher as integers for consistency, though float hashing needs care.

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
            pub fn new(value: &Bound<'_, PyAny>) -> PyResult<Self> {
                // Attempt to extract directly as float first
                if let Ok(f_val) = value.extract::<$type>() {
                    return Ok(Self(f_val));
                }
                // Try extracting as an integer and converting
                if let Ok(i_val) = value.extract::<i64>() {
                    return Ok(Self(i_val as $type));
                }
                 // Try extracting as an unsigned integer and converting
                 if let Ok(u_val) = value.extract::<u64>() {
                    return Ok(Self(u_val as $type));
                }
                // Fallback for other numeric types if necessary (e.g., Decimal) - might need refinement
                 if let Ok(f64_val) = value.extract::<f64>() {
                    return Ok(Self(f64_val as $type));
                }

                Err(PyErr::new::<exceptions::PyTypeError, _>(format!(
                    "Cannot convert argument to {}", stringify!($name)
                )))
            }

            // --- Arithmetic Methods ---
            pub fn __add__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
                let other_val = Self::new(other)?.0;
                Ok(Self(self.0 + other_val))
            }

            pub fn __radd__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
                // Addition is commutative for floats
                self.__add__(other)
            }

             pub fn __sub__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
                let other_val = Self::new(other)?.0;
                Ok(Self(self.0 - other_val))
            }

            pub fn __rsub__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
                 let other_val = Self::new(other)?.0;
                 Ok(Self(other_val - self.0))
            }

            pub fn __mul__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
                let other_val = Self::new(other)?.0;
                Ok(Self(self.0 * other_val))
            }

             pub fn __rmul__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
                // Multiplication is commutative
                self.__mul__(other)
            }

            pub fn __truediv__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
                let other_val = Self::new(other)?.0;
                if other_val == 0.0 {
                     return Err(PyErr::new::<exceptions::PyZeroDivisionError, _>(
                        "division by zero"
                    ));
                }
                Ok(Self(self.0 / other_val))
            }

             pub fn __rtruediv__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
                 let other_val = Self::new(other)?.0;
                 if self.0 == 0.0 {
                     return Err(PyErr::new::<exceptions::PyZeroDivisionError, _>(
                        "division by zero"
                    ));
                 }
                 Ok(Self(other_val / self.0))
            }

            // Note: __floordiv__ for floats is often less intuitive.
            // Python's math.floor(a / b) behavior.
            pub fn __floordiv__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
                let other_val = Self::new(other)?.0;
                 if other_val == 0.0 {
                     return Err(PyErr::new::<exceptions::PyZeroDivisionError, _>(
                        "division by zero"
                    ));
                }
                Ok(Self((self.0 / other_val).floor()))
            }

             pub fn __rfloordiv__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
                 let other_val = Self::new(other)?.0;
                 if self.0 == 0.0 {
                     return Err(PyErr::new::<exceptions::PyZeroDivisionError, _>(
                        "division by zero"
                    ));
                 }
                 Ok(Self((other_val / self.0).floor()))
            }

            // Modulo for floats: a % b == a - floor(a / b) * b
            pub fn __mod__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
                let other_val = Self::new(other)?.0;
                 if other_val == 0.0 {
                     return Err(PyErr::new::<exceptions::PyZeroDivisionError, _>(
                        "division by zero"
                    ));
                }
                Ok(Self(self.0 % other_val)) // Rust's % behaves like Python's for floats
            }

             pub fn __rmod__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
                 let other_val = Self::new(other)?.0;
                 if self.0 == 0.0 {
                     return Err(PyErr::new::<exceptions::PyZeroDivisionError, _>(
                        "division by zero"
                    ));
                 }
                 Ok(Self(other_val % self.0))
            }

            pub fn __pow__(&self, other: &Bound<'_, PyAny>, modulo: Option<&Bound<'_, PyAny>>) -> PyResult<Self> {
                if modulo.is_some() {
                    return Err(PyErr::new::<exceptions::PyTypeError, _>(
                        "pow() with modulo not supported for floats"
                    ));
                }
                let other_val = Self::new(other)?.0;
                Ok(Self(self.0.powf(other_val)))
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

            // --- Comparison ---
            pub fn __richcmp__(&self, other: &Bound<'_, PyAny>, op: CompareOp) -> PyResult<bool> {
                // Attempt to convert 'other' to our float type.
                // If conversion fails, Python's default behavior is often False for Eq/Ne
                // and TypeError for ordered comparisons. We'll try to mimic this.
                match Self::new(other) {
                    Ok(other_float) => {
                        let other_val = other_float.0;
                        match op {
                            CompareOp::Eq => Ok(self.0 == other_val),
                            CompareOp::Ne => Ok(self.0 != other_val),
                            CompareOp::Lt => Ok(self.0 < other_val),
                            CompareOp::Le => Ok(self.0 <= other_val),
                            CompareOp::Gt => Ok(self.0 > other_val),
                            CompareOp::Ge => Ok(self.0 >= other_val),
                        }
                    },
                    Err(_) => match op {
                        // If comparison isn't possible, Eq is false, Ne is true.
                        CompareOp::Eq => Ok(false),
                        CompareOp::Ne => Ok(true),
                        // Ordered comparisons raise TypeError if types are incompatible.
                        _ => Err(PyErr::new::<exceptions::PyTypeError, _>(format!(
                            "'>', '<', '>=', '<=' not supported between instances of '{}' and '{}'",
                            stringify!($name),
                            other.get_type().qualname()?
                        )))
                    }
                }
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

            // Convert to Python's built-in float
            pub fn __float__(&self) -> PyResult<f64> {
                Ok(self.0 as f64) // Convert f32 to f64 if necessary
            }

            // Convert to Python's built-in int (truncates)
            pub fn __int__(&self) -> PyResult<isize> {
                 if self.0.is_nan() || self.0.is_infinite() {
                    Err(PyErr::new::<exceptions::PyValueError, _>(
                        "cannot convert float NaN or infinity to integer"
                    ))
                } else {
                    // Be mindful of potential overflow when converting large floats to isize
                    Ok(self.0.trunc() as isize)
                }
            }

            // --- Representation ---
            pub fn __repr__(&self) -> String {
                // Use Rust's default float formatting, which is generally good.
                // {:?} might sometimes add type suffix, .to_string() is usually preferred.
                self.0.to_string()
            }

            pub fn __str__(&self) -> String {
                self.0.to_string()
            }

            // --- Helper ---
            pub fn value(&self) -> $type {
                self.0
            }
        }
    };
}

// Instantiate the float types using the macro
float_variant!(Float32, f32);
float_variant!(Float64, f64);
