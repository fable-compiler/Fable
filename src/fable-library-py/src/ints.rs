//! Integer Types Module
//!
//! This module provides Rust-wrapped integer types that are compatible with Python's integer system
//! while maintaining F#-style semantics and performance characteristics. It implements a comprehensive
//! set of integer wrapper types (Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64) that can
//! seamlessly interoperate with Python code.
//!
//! ## Key Features
//!
//! - **Wrapping Arithmetic**: All arithmetic operations use wrapping semantics to prevent panics
//! - **Python Integration**: Full support for Python's special methods (__add__, __mul__, etc.)
//! - **Type Safety**: Strong typing while allowing controlled conversions
//! - **Performance**: Direct access to underlying values through Deref trait
//! - **Comprehensive Operations**: Support for bitwise, comparison, and arithmetic operations
//!
//! ## Design Philosophy
//!
//! The integer types are designed to:
//! 1. Maintain F#'s integer semantics (wrapping arithmetic, specific bit widths)
//! 2. Integrate seamlessly with Python's dynamic typing system
//! 3. Provide fast paths for common operations
//! 4. Handle mixed-type arithmetic gracefully
//!
//! ## Usage Examples
//!
//! ```python
//! # Creating integer instances
//! x = Int32(42)
//! y = UInt16(256)
//!
//! # Arithmetic operations
//! result = x + y  # Mixed-type arithmetic
//! overflow = UInt8(255) + UInt8(1)  # Wrapping: becomes 0
//!
//! # Python integration
//! native_int = int(x)  # Converts to Python int
//! formatted = f"{x:x}"  # Hex formatting
//!
//! # Bitwise operations
//! masked = x & 0xFF
//! shifted = y << 2
//! ```
//!
//! ## Integer Type Hierarchy
//!
//! All integer types are generated using the `integer_variant!` macro, which creates:
//! - Signed types: Int8, Int16, Int32, Int64
//! - Unsigned types: UInt8, UInt16, UInt32, UInt64
//!
//! Each type wraps the corresponding Rust primitive and implements the full Python integer protocol.

#![allow(dead_code)]
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::ops::Deref;

use pyo3::class::basic::CompareOp;
use pyo3::exceptions;
use pyo3::prelude::*;
use pyo3::types::PyBytes;

use byteorder::{BigEndian, ByteOrder, LittleEndian};

/// Trait for computing absolute values of integer types.
///
/// This trait provides a uniform interface for absolute value operations across
/// different integer types. For unsigned types, the absolute value is simply the
/// value itself since unsigned integers are always non-negative.
///
/// # Implementation Notes
///
/// - Unsigned types (u8, u16, u32, u64) return themselves unchanged
/// - This trait is used internally by the integer wrapper types
/// - The trait ensures consistent behavior across all integer variants
pub trait Abs {
    /// Returns the absolute value of the integer.
    ///
    /// For unsigned types, this is a no-op that returns the value unchanged.
    fn abs(&self) -> Self;
}

impl Abs for u8 {
    fn abs(&self) -> Self {
        *self
    }
}

impl Abs for u16 {
    fn abs(&self) -> Self {
        *self
    }
}

impl Abs for u32 {
    fn abs(&self) -> Self {
        *self
    }
}

impl Abs for u64 {
    fn abs(&self) -> Self {
        *self
    }
}

/// Enum for handling mixed-type extraction from Python objects.
///
/// This enum is used internally to handle cases where a Python value could be
/// either an integer or a float. It avoids the need to perform multiple extraction
/// attempts and provides a clean way to handle type coercion in arithmetic operations.
///
/// # Usage
///
/// This is primarily used in division operations where the divisor could be either
/// an integer or floating-point number, requiring different handling logic.
///
/// # Examples
///
/// ```rust
/// // Internal usage in division operations
/// let value = other.extract::<OtherType>();
/// match value {
///     Ok(OtherType::Int(i)) => /* handle integer division */,
///     Ok(OtherType::Float(f)) => /* handle float division */,
///     Err(_) => /* handle error */,
/// }
/// ```
#[derive(FromPyObject)]
enum OtherType {
    /// Integer variant - extracts as u64
    #[pyo3(transparent, annotation = "int")]
    Int(u64),
    /// Float variant - extracts as f64
    #[pyo3(transparent, annotation = "float")]
    Float(f64),
}

/// Macro for generating integer wrapper types with full Python integration.
///
/// This macro generates a complete integer wrapper type that:
/// - Wraps a Rust primitive integer type
/// - Implements all Python special methods for arithmetic, comparison, and bitwise operations
/// - Provides automatic dereferencing to the underlying type
/// - Supports conversion to/from Python integers and floats
/// - Implements string formatting and representation methods
///
/// # Parameters
///
/// - `$name`: The name of the generated wrapper type (e.g., Int32, UInt64)
/// - `$type`: The underlying Rust integer type (e.g., i32, u64)
/// - `$mask`: Bit mask for the type (currently unused but maintained for compatibility)
///
/// # Generated Features
///
/// ## Core Functionality
/// - `new()`: Constructor that accepts various numeric types
/// - `value()`: Direct access to the underlying value
/// - `to_string()`: String conversion with radix support (2, 8, 10, 16)
/// - `to_bytes()`: Byte array conversion with endianness control
///
/// ## Arithmetic Operations
/// - Addition (`+`), Subtraction (`-`), Multiplication (`*`)
/// - Division (`/`, `//`), Modulo (`%`)
/// - Unary negation (`-`), Absolute value (`abs()`)
/// - All operations support both left and right operand variants
///
/// ## Bitwise Operations
/// - AND (`&`), OR (`|`), XOR (`^`)
/// - Left shift (`<<`), Right shift (`>>`)
/// - Bitwise NOT (`~`)
///
/// ## Comparison Operations
/// - All comparison operators (`==`, `!=`, `<`, `<=`, `>`, `>=`)
/// - Supports comparison with other integer types and floats
///
/// ## Python Integration
/// - `__int__()`, `__float__()`: Type conversion methods
/// - `__bool__()`: Boolean conversion (false if zero)
/// - `__hash__()`: Hash support for use in dictionaries and sets
/// - `__repr__()`, `__str__()`: String representation
/// - `__format__()`: Custom formatting support
/// - `__index__()`: Support for use as array indices
///
/// # Error Handling
///
/// The generated types handle various error conditions:
/// - Type conversion errors when invalid types are passed
/// - Division by zero errors in division operations
/// - Overflow is handled through wrapping arithmetic (no panics)
///
/// # Examples
///
/// ```rust
/// // Usage of the macro
/// integer_variant!(Int32, i32, 0xffffffff_u32);
///
/// // This generates a complete Int32 type with all Python integration
/// ```
// Note that it's currently not possible to extend the `int` type in Python with pyo3.
macro_rules! integer_variant {
    ($name:ident, $type:ty, $mask:expr, $doc:expr) => {
        #[doc = $doc]
        ///
        /// This type wraps a Rust primitive integer and provides:
        /// - Wrapping arithmetic operations (no panics on overflow)
        /// - Full Python special method support
        /// - Type-safe operations with automatic conversions
        /// - Direct access to the underlying value via Deref
        #[pyclass(module = "fable", frozen)]
        #[derive(Clone)]
        pub struct $name(pub $type); // Make the inner field public

        // Implement Deref trait to allow automatic dereferencing
        impl Deref for $name {
            type Target = $type;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        // No From trait implementations - using extract directly

        #[pymethods]
        impl $name {
            /// Create a new integer instance from various numeric types.
            ///
            /// This constructor supports conversion from:
            /// - Native Rust integer types (i64, u64)
            /// - Python integers
            /// - Floating-point numbers (truncated to integer)
            /// - Custom Float64 type from the floats module
            ///
            /// # Arguments
            ///
            /// * `value` - The value to convert, can be any numeric type
            ///
            /// # Returns
            ///
            /// A new instance of the integer type
            ///
            /// # Errors
            ///
            /// Returns `PyTypeError` if the value cannot be converted to an integer
            #[new]
            pub fn new(value: &Bound<'_, PyAny>) -> PyResult<Self> {
                // Fast path: Try to extract directly as the raw type
                if let Ok(i_val) = value.extract::<$type>() {
                    return Ok(Self(i_val));
                }

                // Fast paths for our custom types
                if let Ok(float64_val) = value.extract::<crate::floats::Float64>() {
                    return Ok(Self(float64_val.value() as $type));
                }

                // Try extracting as integers
                if let Ok(i_val) = value.extract::<i64>() {
                    return Ok(Self(i_val as $type));
                }

                if let Ok(u_val) = value.extract::<u64>() {
                    return Ok(Self(u_val as $type));
                }

                // Fallback for other numeric types
                if let Ok(f64_val) = value.extract::<f64>() {
                    return Ok(Self(f64_val as $type));
                }

                Err(PyErr::new::<exceptions::PyTypeError, _>(format!(
                    "Cannot convert argument of type {} to {}",
                    value.get_type().name()?,
                    stringify!($name)
                )))
            }

            /// Convert the integer to a string representation in the specified radix.
            ///
            /// # Arguments
            ///
            /// * `radix` - The base for string conversion (2, 8, 10, or 16)
            ///
            /// # Returns
            ///
            /// String representation of the integer in the specified base
            ///
            /// # Examples
            ///
            /// ```python
            /// x = Int32(255)
            /// print(x.to_string(16))  # "ff"
            /// print(x.to_string(2))   # "11111111"
            /// print(x.to_string(8))   # "377"
            /// ```
            #[pyo3(signature = (radix=10))]
            pub fn to_string(&self, radix: u32) -> String {
                match radix {
                    10 => self.0.to_string(),
                    16 => format!("{:x}", self.0),
                    2 => format!("{:b}", self.0),
                    8 => format!("{:o}", self.0),
                    _ => self.0.to_string(),
                }
            }

            /// Unary negation operator.
            ///
            /// Uses wrapping negation to prevent panics on overflow.
            /// For unsigned types, this performs two's complement negation.
            pub fn __neg__(&self) -> PyResult<$name> {
                Ok($name(self.0.wrapping_neg()))
            }

            /// Convert the integer to a byte array.
            ///
            /// # Arguments
            ///
            /// * `length` - Number of bytes in the output array
            /// * `byteorder` - Either "little" or "big" for endianness
            ///
            /// # Returns
            ///
            /// Python bytes object containing the integer's byte representation
            ///
            /// # Errors
            ///
            /// Returns `PyValueError` if byteorder is not "little" or "big"
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

            /// Addition operator with wrapping arithmetic.
            ///
            /// Supports addition with other integer types and performs automatic
            /// type conversion. Uses wrapping addition to prevent overflow panics.
            pub fn __add__(&self, other: &Bound<'_, PyAny>) -> PyResult<$name> {
                let other = match other.extract::<$type>() {
                    Ok(other) => other,
                    Err(_) => {
                        return Err(PyErr::new::<exceptions::PyTypeError, _>(format!(
                            "Cannot convert argument {} to {}",
                            other.call_method0("__str__")?.extract::<String>()?,
                            stringify!($type)
                        )))
                    }
                };
                Ok($name(self.0.wrapping_add(other)))
            }

            /// Right-hand addition operator.
            ///
            /// Called when this integer is on the right side of an addition operation.
            /// Delegates to the left operand's addition method.
            pub fn __radd__<'py>(&self, other: &Bound<'py, PyAny>) -> PyResult<Bound<'py, PyAny>> {
                other.add(self.0)
            }

            /// Subtraction operator.
            ///
            /// Performs subtraction with automatic type conversion and wrapping arithmetic.
            pub fn __sub__(&self, other: &Bound<'_, PyAny>) -> PyResult<$name> {
                let other = match other.extract::<$type>() {
                    Ok(other) => other,
                    Err(_) => {
                        return Err(PyErr::new::<exceptions::PyTypeError, _>(format!(
                            "Cannot convert argument to {}",
                            stringify!($name)
                        )))
                    }
                };
                Ok($name(self.0 - other))
            }

            /// Right-hand subtraction operator.
            pub fn __rsub__<'py>(&self, other: &Bound<'py, PyAny>) -> PyResult<Bound<'py, PyAny>> {
                other.sub(self.0)
            }

            /// Multiplication operator with wrapping arithmetic.
            ///
            /// Supports multiplication with other integer types and performs
            /// wrapping multiplication to prevent overflow panics.
            pub fn __mul__(&self, other: &Bound<'_, PyAny>) -> PyResult<$name> {
                let other = match other.extract::<$name>() {
                    Ok(other) => other.0,
                    Err(_) => match other.extract::<$type>() {
                        Ok(other) => other,
                        Err(_) => {
                            return Err(PyErr::new::<exceptions::PyTypeError, _>(format!(
                                "Cannot convert argument to {}",
                                stringify!($name)
                            )))
                        }
                    },
                };
                Ok($name(self.0.wrapping_mul(other)))
            }

            /// Right-hand multiplication operator.
            pub fn __rmul__<'py>(&self, other: &Bound<'py, PyAny>) -> PyResult<Bound<'py, PyAny>> {
                other.mul(self.0)
            }

            /// True division operator.
            ///
            /// Performs division with support for both integer and floating-point divisors.
            /// Returns zero division error for zero divisors.
            pub fn __truediv__(&self, other: &Bound<'_, PyAny>) -> PyResult<$name> {
                let value = other.extract::<OtherType>();
                match value {
                    Ok(OtherType::Int(value)) => {
                        if value == 0 {
                            return Err(PyErr::new::<exceptions::PyZeroDivisionError, _>(
                                "Cannot divide by zero",
                            ));
                        }
                        Ok($name(self.0 as $type / value as $type))
                    }
                    Ok(OtherType::Float(value)) => {
                        if value == 0.0 {
                            return Err(PyErr::new::<exceptions::PyZeroDivisionError, _>(
                                "Cannot divide by zero",
                            ));
                        }
                        Ok($name((self.0 as f64 / value) as $type))
                    }
                    Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot divide")),
                }
            }

            /// Right-hand true division operator.
            pub fn __rtruediv__<'py>(
                &self,
                other: &Bound<'py, PyAny>,
            ) -> PyResult<Bound<'py, PyAny>> {
                other.div(self.0)
            }

            /// Floor division operator.
            ///
            /// Performs integer division, truncating towards negative infinity.
            pub fn __floordiv__(&self, other: &Bound<'_, PyAny>) -> PyResult<$name> {
                let other = other.extract::<$type>()?;
                Ok($name(self.0 / other))
            }

            /// Right-hand floor division operator.
            pub fn __rfloordiv__<'py>(
                &self,
                other: &Bound<'py, PyAny>,
            ) -> PyResult<Bound<'py, PyAny>> {
                let result = other.div(self.0)?;
                result.call_method0("__int__")
            }

            /// Modulo operator.
            ///
            /// Returns the remainder after division.
            pub fn __mod__(&self, other: &Bound<'_, PyAny>) -> PyResult<$name> {
                let other = other.extract::<$type>()?;
                Ok($name(self.0 % other))
            }

            /// Right-hand modulo operator.
            pub fn __rmod__<'py>(&self, other: &Bound<'py, PyAny>) -> PyResult<Bound<'py, PyAny>> {
                other.call_method1("__mod__", (self.0,))
            }

            /// Bitwise NOT operator (~).
            ///
            /// Returns the bitwise complement of the integer.
            pub fn __invert__(&self) -> Self {
                Self(!self.0)
            }

            /// Alternative bitwise NOT operator.
            ///
            /// Provides the same functionality as `__invert__` for compatibility.
            pub fn __inv__(&self) -> Self {
                Self(!self.0)
            }

            /// Absolute value operator.
            ///
            /// Returns the absolute value using the Abs trait implementation.
            pub fn __abs__(&self) -> Self {
                Self(self.0.abs())
            }

            /// Rich comparison operator.
            ///
            /// Implements all comparison operations (==, !=, <, <=, >, >=) with
            /// support for comparison against other integer types and floats.
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

            /// Right shift operator (>>).
            ///
            /// Performs logical right shift with wrapping behavior.
            pub fn __rshift__(&self, other: u32) -> PyResult<$name> {
                Ok($name(self.0.wrapping_shr(other)))
            }

            /// Right-hand right shift operator.
            pub fn __rrshift__<'py>(
                &self,
                other: &'py Bound<'py, PyAny>,
            ) -> PyResult<Bound<'py, PyAny>> {
                other.rshift(self.0)
            }

            /// Left shift operator (<<).
            ///
            /// Performs left rotation to handle overflow gracefully.
            pub fn __lshift__(&self, other: u32) -> PyResult<$name> {
                Ok($name(self.0.rotate_left(other)))
            }

            /// Right-hand left shift operator.
            pub fn __rlshift__<'py>(
                &self,
                other: &'py Bound<'py, PyAny>,
            ) -> PyResult<Bound<'py, PyAny>> {
                other.lshift(self.0)
            }

            /// Bitwise AND operator (&).
            ///
            /// Performs bitwise AND operation with type conversion.
            pub fn __and__(&self, other: &Bound<'_, PyAny>) -> PyResult<$name> {
                match other.extract::<u32>() {
                    Ok(other) => Ok($name((self.0 as u32 & other) as $type)),
                    Err(_) => Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot compare")),
                }
            }

            /// Right-hand bitwise AND operator.
            pub fn __rand__<'py>(
                &self,
                other: &'py Bound<'py, PyAny>,
            ) -> PyResult<Bound<'py, PyAny>> {
                other.bitand(self.0)
            }

            /// Bitwise OR operator (|).
            ///
            /// Performs bitwise OR operation with automatic type conversion.
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

            /// Right-hand bitwise OR operator.
            pub fn __ror__<'py>(
                &self,
                other: &'py Bound<'py, PyAny>,
            ) -> PyResult<Bound<'py, PyAny>> {
                other.bitor(self.0)
            }

            /// Bitwise XOR operator (^).
            ///
            /// Performs bitwise exclusive OR operation.
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

            /// Right-hand bitwise XOR operator.
            pub fn __rxor__<'py>(
                &self,
                other: &'py Bound<'py, PyAny>,
            ) -> PyResult<Bound<'py, PyAny>> {
                other.bitxor(self.0)
            }

            /// Boolean conversion operator.
            ///
            /// Returns `false` if the integer is zero, `true` otherwise.
            fn __bool__(&self) -> bool {
                self.0 != 0
            }

            /// Hash function for dictionary and set usage.
            ///
            /// Computes a hash value using Rust's default hasher.
            pub fn __hash__(&self) -> u64 {
                let mut hasher = DefaultHasher::new();
                self.0.hash(&mut hasher);
                hasher.finish()
            }

            /// .NET compatible GetHashCode method.
            ///
            /// Returns a 32-bit hash code compatible with .NET's GetHashCode() method.
            /// This method follows .NET naming conventions (PascalCase).
            #[allow(non_snake_case)]
            pub fn GetHashCode(&self) -> i32 {
                let mut hasher = DefaultHasher::new();
                self.0.hash(&mut hasher);
                hasher.finish() as i32
            }

            /// Convert to Python int.
            ///
            /// Returns the underlying integer value as a Python-compatible integer.
            pub fn __int__(&self) -> PyResult<$type> {
                Ok(self.0)
            }

            /// Convert to Python float.
            ///
            /// Returns the integer value as a floating-point number.
            pub fn __float__(&self) -> PyResult<f64> {
                Ok(self.0 as f64)
            }

            /// Index operator support.
            ///
            /// Allows the integer to be used as an array index in Python.
            /// This is required for objects to be used with sequence[index] syntax.
            pub fn __index__(&self) -> PyResult<$type> {
                Ok(self.0)
            }

            /// Developer representation string.
            ///
            /// Returns a string that could be used to recreate the object.
            pub fn __repr__(&self) -> PyResult<String> {
                Ok(self.0.to_string())
            }

            /// User-friendly string representation.
            pub fn __str__(&self) -> PyResult<String> {
                Ok(self.0.to_string())
            }

            /// Custom formatting support.
            ///
            /// Delegates to Python's built-in integer formatting for maximum compatibility.
            /// Supports all Python format specifiers (d, x, o, b, etc.).
            pub fn __format__(&self, py: Python<'_>, format: &str) -> PyResult<String> {
                // This is hard to implement so we just convert to a Python integer and let Python handle it
                let int = self.__int__()?;
                let int = int.into_pyobject(py)?;
                let result = int.call_method1("__format__", (format,))?;
                result.extract::<String>()
            }

            /// Direct access to the underlying integer value.
            ///
            /// This method provides direct access to the wrapped integer value
            /// without any Python overhead.
            ///
            /// # Returns
            ///
            /// The underlying integer value
            pub fn value(&self) -> $type {
                self.0
            }
        }

        /// Conversion from the wrapper type to the underlying integer type.
        ///
        /// This allows the wrapper to be used anywhere the underlying type is expected.
        impl From<$name> for $type {
            fn from(value: $name) -> $type {
                value.0
            }
        }

        /// Conversion from the underlying integer type to the wrapper type.
        ///
        /// This allows easy creation of wrapper instances from raw integer values.
        impl From<$type> for $name {
            fn from(value: $type) -> $name {
                $name(value)
            }
        }
    };
}

// Generate all integer wrapper types using the macro
// Each type wraps the corresponding Rust primitive and provides full Python integration

integer_variant!(Int8, i8, 0xff_u32, "8-bit signed integer type (-128 to 127).\n\nInteger wrapper type providing F#-style semantics with Python integration.");

integer_variant!(UInt8, u8, 0xff, "8-bit unsigned integer type (0 to 255).\n\nInteger wrapper type providing F#-style semantics with Python integration.");

integer_variant!(Int16, i16, 0xffff_u32, "16-bit signed integer type (-32,768 to 32,767).\n\nInteger wrapper type providing F#-style semantics with Python integration.");

integer_variant!(UInt16, u16, 0xffff_u32, "16-bit unsigned integer type (0 to 65,535).\n\nInteger wrapper type providing F#-style semantics with Python integration.");

integer_variant!(Int32, i32, 0xffffffff_u32, "32-bit signed integer type (-2,147,483,648 to 2,147,483,647).\n\nInteger wrapper type providing F#-style semantics with Python integration.");

integer_variant!(UInt32, u32, 0xffffffff_u32, "32-bit unsigned integer type (0 to 4,294,967,295).\n\nInteger wrapper type providing F#-style semantics with Python integration.");

integer_variant!(Int64, i64, 0xffffffffffffffff_u64, "64-bit signed integer type (-9,223,372,036,854,775,808 to 9,223,372,036,854,775,807).\n\nInteger wrapper type providing F#-style semantics with Python integration.");

integer_variant!(UInt64, u64, 0xffffffffffffffff_u64, "64-bit unsigned integer type (0 to 18,446,744,073,709,551,615).\n\nInteger wrapper type providing F#-style semantics with Python integration.");
