#![allow(dead_code)]
use pyo3::class::basic::CompareOp;
use pyo3::exceptions;
use pyo3::prelude::*;
use pyo3::types::PyBool;
use pyo3::types::PyNotImplemented;
use pyo3::BoundObject;
use std::ops::Deref;

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
            #[classattr]
            pub fn nan() -> Self {
                Self(<$type>::NAN)
            }

            #[classattr]
            pub fn infinity() -> Self {
                Self(<$type>::INFINITY)
            }
            #[classattr]
            pub fn negative_infinity() -> Self {
                Self(<$type>::NEG_INFINITY)
            }

            #[new]
            pub fn new(value: $type) -> Self {
                Self(value)
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
            // In .NET we don't have a floor division operator so if
            // we divide a float by integer we get a float.
            pub fn __floordiv__(&self, other: $type) -> PyResult<Self> {
                Ok(Self(self.0 / other).floor())
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
                        "pow() with modulo not supported for floats",
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
            fn __richcmp__<'py>(
                &self,
                other: &Bound<'_, PyAny>,
                op: CompareOp,
                py: Python<'py>,
            ) -> PyResult<Borrowed<'py, 'py, PyAny>> {
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
            // .NET-compatible float hashing with integer consistency
            pub fn __hash__(&self) -> i64 {
                if self.0.is_nan() {
                    // .NET semantic: All NaN values hash to the same consistent value
                    0
                } else if self.0.is_infinite() {
                    // .NET semantic: +Inf and -Inf have distinct, consistent hashes
                    if self.0.is_sign_positive() {
                        i64::MAX
                    } else {
                        i64::MIN
                    }
                } else if self.0.fract() == 0.0 {
                    // Critical: Integer-like floats must hash same as integers
                    // This ensures hash(2) == hash(2.0) for HashSet consistency
                    self.0 as i64
                } else {
                    // Non-integer floats: use bit-based hashing for consistency
                    // This ensures the same float always produces the same hash
                    self.0.to_bits() as i64
                }
            }

            /// .NET compatible GetHashCode method.
            ///
            /// Returns a 32-bit hash code by calling __hash__ and truncating to i32.
            /// This ensures consistency between the Python hash and .NET hash while
            /// maintaining .NET's expected return type.
            #[allow(non_snake_case)]
            pub fn GetHashCode(&self) -> i32 {
                // Call the Python hash method and truncate to i32 for .NET compatibility
                self.__hash__() as i32
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
                        "cannot convert float NaN or infinity to integer",
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
                            PyErr::new::<exceptions::PyValueError, _>(
                                "Invalid precision in format specifier",
                            )
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
                    }
                    Some('e') => {
                        if let Some(prec) = precision {
                            Ok(format!("{:.1$e}", self.0, prec))
                        } else {
                            Ok(format!("{:e}", self.0))
                        }
                    }
                    Some('g') => {
                        if let Some(prec) = precision {
                            Ok(format!("{:.1$?}", self.0, prec))
                        } else {
                            Ok(format!("{:?}", self.0))
                        }
                    }
                    Some(c) => Err(PyErr::new::<exceptions::PyValueError, _>(format!(
                        "Unknown format code '{}' for {}",
                        c,
                        stringify!($name)
                    ))),
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

            pub fn cosh(&self) -> Self {
                Self(self.0.cosh())
            }

            pub fn sinh(&self) -> Self {
                Self(self.0.sinh())
            }

            pub fn tanh(&self) -> Self {
                Self(self.0.tanh())
            }

            pub fn acos(&self) -> PyResult<Self> {
                if self.0 < -1.0 || self.0 > 1.0 {
                    return Err(PyErr::new::<exceptions::PyValueError, _>(
                        "acos() domain error",
                    ));
                }
                Ok(Self(self.0.acos()))
            }

            pub fn asin(&self) -> PyResult<Self> {
                if self.0 < -1.0 || self.0 > 1.0 {
                    return Err(PyErr::new::<exceptions::PyValueError, _>(
                        "asin() domain error",
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
                // Special case for 0.0 to match .NET semantics
                if self.0 == 0.0 {
                    return Ok(Self(<$type>::NEG_INFINITY));
                }
                // Return NaN for negative values to match .NET semantics
                if self.0 < 0.0 {
                    return Ok(Self(<$type>::NAN));
                }
                let base_val = match base {
                    Some(b) => b,
                    None => std::f64::consts::E as $type,
                };
                if base_val <= 1.0 {
                    return Err(PyErr::new::<exceptions::PyValueError, _>(
                        "log() base must be greater than 1",
                    ));
                }
                Ok(Self(self.0.log(base_val)))
            }

            pub fn log10(&self) -> PyResult<Self> {
                if self.0 <= 0.0 {
                    return Err(PyErr::new::<exceptions::PyValueError, _>(
                        "log10() domain error",
                    ));
                }
                Ok(Self(self.0.log(10.0)))
            }

            pub fn log2(&self) -> PyResult<Self> {
                if self.0 <= 0.0 {
                    return Err(PyErr::new::<exceptions::PyValueError, _>(
                        "log2() domain error",
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

// Free functions for mathematical operations
#[pyfunction]
pub fn abs(x: &Float64) -> Float64 {
    Float64(x.0.abs())
}

#[pyfunction]
pub fn sqrt(x: &Float64) -> PyResult<Float64> {
    x.sqrt()
}

#[pyfunction]
pub fn cos(x: &Float64) -> Float64 {
    x.cos()
}

#[pyfunction]
pub fn sin(x: &Float64) -> Float64 {
    x.sin()
}

#[pyfunction]
pub fn tan(x: &Float64) -> Float64 {
    x.tan()
}

#[pyfunction]
pub fn cosh(x: &Float64) -> Float64 {
    x.cosh()
}

#[pyfunction]
pub fn sinh(x: &Float64) -> Float64 {
    x.sinh()
}

#[pyfunction]
pub fn tanh(x: &Float64) -> Float64 {
    x.tanh()
}

#[pyfunction]
pub fn acos(x: &Float64) -> PyResult<Float64> {
    x.acos()
}

#[pyfunction]
pub fn asin(x: &Float64) -> PyResult<Float64> {
    x.asin()
}

#[pyfunction]
pub fn atan(x: &Float64) -> Float64 {
    x.atan()
}

#[pyfunction]
pub fn atan2(py: Python<'_>, y: &Bound<'_, PyAny>, x: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let y_val = y
        .extract::<Float64>()
        .map_err(|_| PyErr::new::<exceptions::PyTypeError, _>("Expected Float64 for y"))?;
    let x_val = x
        .extract::<Float64>()
        .map_err(|_| PyErr::new::<exceptions::PyTypeError, _>("Expected Float64 for x"))?;
    let result = y_val.atan2(*x_val)?;
    Ok(result.into_pyobject(py)?.into())
}

#[pyfunction]
pub fn exp(x: &Float64) -> Float64 {
    x.exp()
}

#[pyfunction]
#[pyo3(signature = (x, base = None))]
pub fn log(
    py: Python<'_>,
    x: &Bound<'_, PyAny>,
    base: Option<&Bound<'_, PyAny>>,
) -> PyResult<Py<PyAny>> {
    let f64_val = x
        .extract::<Float64>()
        .map_err(|_| PyErr::new::<exceptions::PyTypeError, _>("Expected Float64"))?;
    let base = match base {
        Some(b) => Some(b.extract::<f64>()?),
        None => None,
    };
    let result = f64_val.log(base)?;
    Ok(result.into_pyobject(py)?.into())
}

#[pyfunction]
pub fn log10(x: &Float64) -> PyResult<Float64> {
    x.log10()
}

#[pyfunction]
pub fn log2(py: Python<'_>, x: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let f64_val = x
        .extract::<Float64>()
        .map_err(|_| PyErr::new::<exceptions::PyTypeError, _>("Expected Float64"))?;
    let result = f64_val.log2()?;
    Ok(result.into_pyobject(py)?.into())
}

#[pyfunction]
pub fn degrees(py: Python<'_>, x: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let f64_val = x
        .extract::<Float64>()
        .map_err(|_| PyErr::new::<exceptions::PyTypeError, _>("Expected Float64"))?;
    let result = f64_val.degrees();
    Ok(result.into_pyobject(py)?.into())
}

#[pyfunction]
pub fn radians(py: Python<'_>, x: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let f64_val = x
        .extract::<Float64>()
        .map_err(|_| PyErr::new::<exceptions::PyTypeError, _>("Expected Float64"))?;
    let result = f64_val.radians();
    Ok(result.into_pyobject(py)?.into())
}

#[pyfunction]
pub fn is_nan(_py: Python<'_>, x: &Bound<'_, PyAny>) -> PyResult<bool> {
    let f64_val = x
        .extract::<Float64>()
        .map_err(|_| PyErr::new::<exceptions::PyTypeError, _>("Expected Float64"))?;
    Ok(f64_val.is_nan())
}

#[pyfunction]
pub fn is_infinity(_py: Python<'_>, x: &Bound<'_, PyAny>) -> PyResult<bool> {
    let f64_val = x
        .extract::<Float64>()
        .map_err(|_| PyErr::new::<exceptions::PyTypeError, _>("Expected Float64"))?;
    Ok(f64_val.is_infinity())
}

#[pyfunction]
pub fn is_positive_infinity(_py: Python<'_>, x: &Bound<'_, PyAny>) -> PyResult<bool> {
    let f64_val = x
        .extract::<Float64>()
        .map_err(|_| PyErr::new::<exceptions::PyTypeError, _>("Expected Float64"))?;
    Ok(f64_val.is_positive_infinity())
}

#[pyfunction]
pub fn is_negative_infinity(_py: Python<'_>, x: &Bound<'_, PyAny>) -> PyResult<bool> {
    let f64_val = x
        .extract::<Float64>()
        .map_err(|_| PyErr::new::<exceptions::PyTypeError, _>("Expected Float64"))?;
    Ok(f64_val.is_negative_infinity())
}

#[pyfunction]
pub fn floor(py: Python<'_>, x: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let f64_val = x
        .extract::<Float64>()
        .map_err(|_| PyErr::new::<exceptions::PyTypeError, _>("Expected Float64"))?;
    let result = f64_val.floor();
    Ok(result.into_pyobject(py)?.into())
}

#[pyfunction]
pub fn ceil(py: Python<'_>, x: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let f64_val = x
        .extract::<Float64>()
        .map_err(|_| PyErr::new::<exceptions::PyTypeError, _>("Expected Float64"))?;
    let result = f64_val.ceil();
    Ok(result.into_pyobject(py)?.into())
}

#[pyfunction]
pub fn pow(x: &Float64, y: f64) -> PyResult<Float64> {
    x.__pow__(y, None)
}

#[pyfunction]
pub fn parse(x: &str) -> PyResult<Float64> {
    let value = x.trim().parse::<f64>()?;
    Ok(Float64(value))
}

/// A module for float operations
pub fn register_float_module(parent_module: &Bound<'_, PyModule>) -> PyResult<()> {
    let m = PyModule::new(parent_module.py(), "floats")?;

    // Register the float classes
    m.add_class::<Float32>()?;
    m.add_class::<Float64>()?;

    // Add infinity constants
    m.add("infinity", Float64::infinity())?;
    m.add("negative_infinity", Float64::negative_infinity())?;
    m.add("nan", Float64::nan())?;

    // Add functions to the module
    m.add_function(wrap_pyfunction!(sqrt, &m)?)?;
    m.add_function(wrap_pyfunction!(cos, &m)?)?;
    m.add_function(wrap_pyfunction!(sin, &m)?)?;
    m.add_function(wrap_pyfunction!(tan, &m)?)?;
    m.add_function(wrap_pyfunction!(cosh, &m)?)?;
    m.add_function(wrap_pyfunction!(sinh, &m)?)?;
    m.add_function(wrap_pyfunction!(tanh, &m)?)?;
    m.add_function(wrap_pyfunction!(acos, &m)?)?;
    m.add_function(wrap_pyfunction!(asin, &m)?)?;
    m.add_function(wrap_pyfunction!(atan, &m)?)?;
    m.add_function(wrap_pyfunction!(atan2, &m)?)?;
    m.add_function(wrap_pyfunction!(exp, &m)?)?;
    m.add_function(wrap_pyfunction!(log, &m)?)?;
    m.add_function(wrap_pyfunction!(log10, &m)?)?;
    m.add_function(wrap_pyfunction!(log2, &m)?)?;
    m.add_function(wrap_pyfunction!(degrees, &m)?)?;
    m.add_function(wrap_pyfunction!(radians, &m)?)?;
    m.add_function(wrap_pyfunction!(is_nan, &m)?)?;
    m.add_function(wrap_pyfunction!(is_infinity, &m)?)?;
    m.add_function(wrap_pyfunction!(is_positive_infinity, &m)?)?;
    m.add_function(wrap_pyfunction!(is_negative_infinity, &m)?)?;
    m.add_function(wrap_pyfunction!(floor, &m)?)?;
    m.add_function(wrap_pyfunction!(ceil, &m)?)?;
    m.add_function(wrap_pyfunction!(pow, &m)?)?;
    m.add_function(wrap_pyfunction!(parse, &m)?)?;
    m.add_function(wrap_pyfunction!(abs, &m)?)?;

    parent_module.add_submodule(&m)
}
