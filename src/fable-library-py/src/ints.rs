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
use std::ops::Deref;

use pyo3::class::basic::CompareOp;
use pyo3::exceptions;
use pyo3::prelude::*;
use pyo3::types::{PyAnyMethods, PyBytes};

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

            #[inline]
            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        // No From trait implementations - using extract directly

        #[pymethods]
        impl $name {
            #[classattr]
            const ZERO: $name = $name(0 as $type);

            #[classattr]
            const ONE: $name = $name(1 as $type);

            #[classattr]
            const TWO: $name = $name(2 as $type);

            #[classattr]

            const THREE: $name = $name(3 as $type);

            #[classattr]
            const FOUR: $name = $name(4 as $type);

            #[classattr]
            const FIVE: $name = $name(5 as $type);

            #[classattr]
            const SIX: $name = $name(6 as $type);

            #[classattr]
            const SEVEN: $name = $name(7 as $type);

            #[classattr]
            const EIGHT: $name = $name(8 as $type);

            #[classattr]
            const NINE: $name = $name(9 as $type);

            #[classattr]
            const TEN: $name = $name(10 as $type);

            #[classattr]
            const SIXTEEN: $name = $name(16 as $type);

            #[classattr]
            const THIRTY_TWO: $name = $name(32 as $type);

            #[classattr]
            const SIXTY_FOUR: $name = $name(64 as $type);

            #[classattr]
            const NEG_ONE: $name = $name((-1i8) as $type);

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

            /// Unary plus operator.
            ///
            /// Returns the value unchanged.
            pub fn __pos__(&self) -> PyResult<$name> {
                Ok($name(self.0))
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
            ) -> PyResult<Py<PyAny>> {
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
                // Fast path: other is same type
                if let Ok(other_val) = other.extract::<$name>() {
                    return Ok($name(self.0.wrapping_add(other_val.0)));
                }
                // Slow path: extract as primitive
                let other_val = other.extract::<$type>().map_err(|_| {
                    PyErr::new::<exceptions::PyTypeError, _>(format!(
                        "Cannot convert argument to {}",
                        stringify!($name)
                    ))
                })?;
                Ok($name(self.0.wrapping_add(other_val)))
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
                // Fast path: other is same type
                if let Ok(other_val) = other.extract::<$name>() {
                    return Ok($name(self.0.wrapping_sub(other_val.0)));
                }
                // Slow path: extract as primitive
                let other_val = other.extract::<$type>().map_err(|_| {
                    PyErr::new::<exceptions::PyTypeError, _>(format!(
                        "Cannot convert argument to {}",
                        stringify!($name)
                    ))
                })?;
                Ok($name(self.0.wrapping_sub(other_val)))
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
                // Fast path: other is same type
                if let Ok(other_val) = other.extract::<$name>() {
                    return Ok($name(self.0.wrapping_mul(other_val.0)));
                }
                // Slow path: extract as primitive
                let other_val = other.extract::<$type>().map_err(|_| {
                    PyErr::new::<exceptions::PyTypeError, _>(format!(
                        "Cannot convert argument to {}",
                        stringify!($name)
                    ))
                })?;
                Ok($name(self.0.wrapping_mul(other_val)))
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
                // Fast path: other is same type
                if let Ok(other_val) = other.extract::<$name>() {
                    if other_val.0 == 0 {
                        return Err(PyErr::new::<exceptions::PyZeroDivisionError, _>(
                            "Cannot divide by zero",
                        ));
                    }
                    return Ok($name(self.0 / other_val.0));
                }
                // Slow path: extract as primitive
                let other_val = other.extract::<$type>().map_err(|_| {
                    PyErr::new::<exceptions::PyTypeError, _>(format!(
                        "Cannot convert argument to {}",
                        stringify!($name)
                    ))
                })?;
                if other_val == 0 {
                    return Err(PyErr::new::<exceptions::PyZeroDivisionError, _>(
                        "Cannot divide by zero",
                    ));
                }
                Ok($name(self.0 / other_val))
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
                // Fast path: other is same type
                if let Ok(other_val) = other.extract::<$name>() {
                    if other_val.0 == 0 {
                        return Err(PyErr::new::<exceptions::PyZeroDivisionError, _>(
                            "Cannot modulo by zero",
                        ));
                    }
                    return Ok($name(self.0 % other_val.0));
                }
                // Slow path: extract as primitive
                let other_val = other.extract::<$type>().map_err(|_| {
                    PyErr::new::<exceptions::PyTypeError, _>(format!(
                        "Cannot convert argument to {}",
                        stringify!($name)
                    ))
                })?;
                if other_val == 0 {
                    return Err(PyErr::new::<exceptions::PyZeroDivisionError, _>(
                        "Cannot modulo by zero",
                    ));
                }
                Ok($name(self.0 % other_val))
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
                        Err(_) => match other.extract::<i64>() {
                            Ok(other) => Ok(other as $type),
                            Err(_) => match other.extract::<f64>() {
                                Ok(other) => Ok(other as $type),
                                Err(_) => {
                                    Err(PyErr::new::<exceptions::PyTypeError, _>("Cannot compare"))
                                }
                            },
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
                // Fast path: other is same type
                if let Ok(other_val) = other.extract::<$name>() {
                    return Ok($name(self.0 & other_val.0));
                }
                // Slow path: extract as primitive
                let other_val = other.extract::<$type>().map_err(|_| {
                    PyErr::new::<exceptions::PyTypeError, _>(format!(
                        "Cannot convert argument to {}",
                        stringify!($name)
                    ))
                })?;
                Ok($name(self.0 & other_val))
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
                // Fast path: other is same type
                if let Ok(other_val) = other.extract::<$name>() {
                    return Ok($name(self.0 | other_val.0));
                }
                // Slow path: extract as primitive
                let other_val = other.extract::<$type>().map_err(|_| {
                    PyErr::new::<exceptions::PyTypeError, _>(format!(
                        "Cannot convert argument to {}",
                        stringify!($name)
                    ))
                })?;
                Ok($name(self.0 | other_val))
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
                // Fast path: other is same type
                if let Ok(other_val) = other.extract::<$name>() {
                    return Ok($name(self.0 ^ other_val.0));
                }
                // Slow path: extract as primitive
                let other_val = other.extract::<$type>().map_err(|_| {
                    PyErr::new::<exceptions::PyTypeError, _>(format!(
                        "Cannot convert argument to {}",
                        stringify!($name)
                    ))
                })?;
                Ok($name(self.0 ^ other_val))
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

            /// Python hash method.
            ///
            /// Returns the full 64-bit hash that matches Python's hash() function.
            /// For integers, Python's hash(n) = n for small values.
            pub fn __hash__(&self) -> i64 {
                // Python's hash algorithm for integers: for values that fit in i64,
                // hash(n) = n. This ensures consistency with plain Python ints.
                self.0 as i64
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

            /// Pydantic v2 integration for schema generation.
            ///
            /// This method is called by Pydantic when building a model that uses this type.
            /// It returns a pydantic-core schema that enables:
            /// - Validation of input values (with range checking)
            /// - Serialization to JSON-compatible integers
            /// - JSON Schema generation for OpenAPI documentation
            ///
            /// The pydantic_core module is imported lazily - pydantic is only required
            /// if this method is actually called (i.e., when used in a Pydantic model).
            ///
            /// The schema uses a chained validator approach:
            /// 1. Before validator: Extract Python int from custom types (fixes large int64 values)
            /// 2. int_schema: Pydantic's native integer validation (enables JSON Schema)
            /// 3. After validator: Wrap result back in our custom type
            ///
            /// # Arguments
            ///
            /// * `cls` - The class type (for class methods)
            /// * `_source_type` - The source type (unused, required by Pydantic protocol)
            /// * `_handler` - Schema generation handler (unused for leaf types)
            /// * `py` - Python interpreter reference
            ///
            /// # Returns
            ///
            /// A pydantic-core schema object that defines validation and serialization
            #[classmethod]
            #[pyo3(name = "__get_pydantic_core_schema__")]
            fn get_pydantic_core_schema(
                cls: &Bound<'_, pyo3::types::PyType>,
                _source_type: &Bound<'_, PyAny>,
                _handler: &Bound<'_, PyAny>,
                py: Python<'_>,
            ) -> PyResult<Py<PyAny>> {
                // Lazy import of pydantic_core - only fails if pydantic is not installed
                // AND this type is used in a Pydantic model
                let core_schema = py.import("pydantic_core")?.getattr("core_schema")?;

                // Get all validator/serializer functions from the class
                let extractor_fn = cls.getattr("_pydantic_extractor")?;
                let validator_fn = cls.getattr("_pydantic_validator")?;
                let serializer_fn = cls.getattr("_pydantic_serializer")?;

                // Build the serialization schema
                let ser_schema = core_schema.call_method1(
                    "plain_serializer_function_ser_schema",
                    (serializer_fn,),
                )?;

                // Create an int schema as the base - this enables JSON Schema generation
                let int_schema = core_schema.call_method0("int_schema")?;

                // Build the schema chain:
                // 1. After validator wraps int_schema, converting Python int -> our type
                let after_kwargs = pyo3::types::PyDict::new(py);
                after_kwargs.set_item("serialization", ser_schema)?;

                let after_schema = core_schema.call_method(
                    "no_info_after_validator_function",
                    (validator_fn, int_schema),
                    Some(&after_kwargs),
                )?;

                // 2. Before validator extracts Python int from our type
                //    This fixes validation of large int64/uint64 values
                let full_schema = core_schema.call_method1(
                    "no_info_before_validator_function",
                    (extractor_fn, after_schema),
                )?;

                Ok(full_schema.unbind())
            }

            /// Pydantic extractor function (before validator).
            ///
            /// Called by Pydantic before int_schema validation to extract the underlying
            /// Python int from our custom type. This is necessary because Pydantic's
            /// int_schema doesn't recognize our custom types and would otherwise try
            /// to parse them as strings, failing for large int64/uint64 values.
            #[staticmethod]
            #[pyo3(name = "_pydantic_extractor")]
            fn pydantic_extractor(value: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
                let py = value.py();
                // If the value has __int__, extract it as a Python int
                if value.hasattr("__int__")? {
                    Ok(value.call_method0("__int__")?.into_pyobject(py).map(|o| o.unbind())?)
                } else {
                    Ok(value.clone().unbind())
                }
            }

            /// Pydantic validator function (after validator).
            ///
            /// Called by Pydantic after int_schema validation to wrap the validated
            /// Python int back into our custom type.
            #[staticmethod]
            #[pyo3(name = "_pydantic_validator")]
            fn pydantic_validator(value: &Bound<'_, PyAny>) -> PyResult<Self> {
                Self::new(value)
            }

            /// Pydantic serializer function.
            ///
            /// Called by Pydantic during serialization to convert this type to a
            /// JSON-compatible primitive (Python int).
            #[staticmethod]
            #[pyo3(name = "_pydantic_serializer")]
            fn pydantic_serializer(instance: &Self) -> $type {
                instance.0
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

// =============================================================================
// Integer Parsing Functions
// =============================================================================

/// .NET NumberStyles enum constants for parsing
///
/// These constants match the .NET NumberStyles enumeration used in F# and C#
/// for controlling how numeric strings are parsed.
pub const ALLOW_HEX_SPECIFIER: i32 = 0x00000200; // 512 in decimal
pub const ALLOW_LEADING_WHITE: i32 = 0x00000001; // 1 in decimal
pub const ALLOW_TRAILING_WHITE: i32 = 0x00000002; // 2 in decimal

// =============================================================================
// Helper Functions for Integer Parsing
// =============================================================================

/// Determines the actual radix based on style flags and string prefixes
#[inline]
fn determine_radix(string: &str, style: i32, default_radix: i32) -> i32 {
    // Check style flag first - early return
    if (style & ALLOW_HEX_SPECIFIER) != 0 {
        return 16;
    }

    // Single length check followed by pattern match
    if string.len() >= 2 {
        match &string[..2] {
            "0x" | "0X" => 16,
            "0b" | "0B" => 2,
            "0o" | "0O" => 8,
            _ => default_radix,
        }
    } else {
        default_radix
    }
}

/// Trims whitespace based on NumberStyles flags
#[inline]
fn trim_whitespace(string: &str, style: i32) -> &str {
    match (
        (style & ALLOW_LEADING_WHITE) != 0,
        (style & ALLOW_TRAILING_WHITE) != 0,
    ) {
        (true, true) => string.trim(),
        (true, false) => string.trim_start(),
        (false, true) => string.trim_end(),
        (false, false) => string,
    }
}

/// Removes numeric prefixes (0x, 0b, 0o) when radix is not decimal
#[inline]
fn remove_prefix(string: &str, radix: i32) -> &str {
    if radix != 10 && string.len() >= 2 {
        match &string[..2] {
            "0x" | "0X" | "0b" | "0B" | "0o" | "0O" => &string[2..],
            _ => string,
        }
    } else {
        string
    }
}

/// Creates a parsing error with consistent formatting
#[inline]
fn create_parse_error(string: &str) -> PyErr {
    PyErr::new::<exceptions::PyValueError, _>(format!(
        "The input string {} was not in a correct format.",
        string
    ))
}

/// Preprocesses a numeric string for parsing by handling radix detection,
/// whitespace trimming, prefix removal, and underscore cleanup
fn preprocess_numeric_string(string: &str, style: i32, default_radix: i32) -> (String, i32) {
    let actual_radix = determine_radix(string, style, default_radix);
    let trimmed = trim_whitespace(string, style);
    let without_prefix = remove_prefix(trimmed, actual_radix);
    let final_string = without_prefix.replace('_', "");

    (final_string, actual_radix)
}

// =============================================================================
// Public Parsing Functions
// =============================================================================

/// Range information for different integer bit sizes and signedness
///
/// Returns (min_value, max_value) for a given bit size and signedness.
/// This matches the behavior of F#'s integer parsing functions for int32.
///
/// # Arguments
/// * `unsigned` - Whether the range should be for unsigned integers
/// * `bitsize` - The bit width (8, 16, or 32)
///
/// # Returns
/// A tuple containing (minimum_value, maximum_value) for the specified type
///
/// # Panics
/// Panics if bitsize is not 8, 16, or 32
#[pyfunction]
pub fn get_range(unsigned: bool, bitsize: i32) -> (i64, i64) {
    match (unsigned, bitsize) {
        (true, 8) => (0, 255),
        (false, 8) => (-128, 127),
        (true, 16) => (0, 65535),
        (false, 16) => (-32768, 32767),
        (true, 32) => (0, 4294967295),
        (false, 32) => (-2147483648, 2147483647),
        _ => panic!("Invalid bit size: {}. Must be 8, 16, or 32", bitsize),
    }
}

/// Range information for 64-bit integers
///
/// Returns (min_value, max_value) for 64-bit integers.
/// This matches the behavior of long.py get_range function.
///
/// # Arguments
/// * `unsigned` - Whether the range should be for unsigned integers
///
/// # Returns
/// A tuple containing (minimum_value, maximum_value) for 64-bit integers
#[pyfunction]
pub fn get_range_64(unsigned: bool) -> (i64, i64) {
    if unsigned {
        (0, i64::MAX) // For unsigned 64-bit, we use i64::MAX as upper bound in Rust
    } else {
        (i64::MIN, i64::MAX)
    }
}

/// Parses a string as a 32-bit integer with F#-compatible semantics
///
/// This function matches the behavior of int32.py parse function exactly.
///
/// # Arguments
/// * `string` - The string to parse
/// * `style` - NumberStyles flags controlling parsing behavior
/// * `unsigned` - Whether to treat the result as unsigned (u32)
/// * `bitsize` - The bit size for range validation (8, 16, or 32)
/// * `radix` - Optional radix override (defaults to 10)
///
/// # Returns
/// The parsed integer value as i64 (to handle both i32 and u32 ranges)
///
/// # Errors
/// Returns `ValueError` if the string is not in a valid format or value is out of range
#[pyfunction]
#[pyo3(signature = (string, style, unsigned, bitsize, radix=10))]
pub fn parse_int32(
    string: &str,
    style: i32,
    unsigned: bool,
    bitsize: i32,
    radix: i32,
) -> PyResult<i64> {
    let (final_string, actual_radix) = preprocess_numeric_string(string, style, radix);

    // Parse the integer using Rust's from_str_radix
    let v = i64::from_str_radix(&final_string, actual_radix as u32)
        .map_err(|_| create_parse_error(string))?;

    // Handle two's complement conversion for non-decimal radixes on signed types
    let (umin, umax) = get_range(true, bitsize);
    let in_unsigned_range = v >= umin && v <= umax;
    let mask = 1i64 << (bitsize - 1);
    let has_high_bit = (v & mask) != 0;

    let result = match (
        unsigned,
        actual_radix != 10,
        in_unsigned_range,
        has_high_bit,
    ) {
        (false, true, true, true) => v - (mask << 1), // Signed, non-decimal, in range, high bit set
        _ => v,                                       // All other cases: use value as-is
    };

    // Validate final range
    let (min, max) = get_range(unsigned, bitsize);
    let in_range = result >= min && result <= max;

    match in_range {
        true => Ok(result),
        false => Err(create_parse_error(string)),
    }
}

/// Parses a string as a 64-bit integer with F#-compatible semantics
///
/// This function matches the behavior of long.py parse function exactly.
///
/// # Arguments
/// * `string` - The string to parse
/// * `style` - NumberStyles flags controlling parsing behavior
/// * `unsigned` - Whether to treat the result as unsigned (u64)
/// * `bitsize` - The bit size for range validation (must be 64)
/// * `radix` - Optional radix override (defaults to 10)
///
/// # Returns
/// The parsed integer value
///
/// # Errors
/// Returns `ValueError` if the string is not in a valid format or value is out of range
#[pyfunction]
#[pyo3(signature = (string, style, unsigned, _bitsize, radix=10))]
pub fn parse_int64(
    string: &str,
    style: i32,
    unsigned: bool,
    _bitsize: i32,
    radix: i32,
) -> PyResult<i64> {
    let (final_string, actual_radix) = preprocess_numeric_string(string, style, radix);

    // Parse the integer - handle large hex values by parsing as u64 first for non-decimal
    let v = if actual_radix != 10 {
        // For non-decimal, parse as u64 first to handle large hex values
        u64::from_str_radix(&final_string, actual_radix as u32)
            .map(|u_val| u_val as i64) // Cast performs automatic two's complement conversion
            .map_err(|_| create_parse_error(string))?
    } else {
        // For decimal, use standard i64 parsing
        i64::from_str_radix(&final_string, actual_radix as u32)
            .map_err(|_| create_parse_error(string))?
    };

    // No additional two's complement conversion needed for 64-bit when using u64 parsing
    let result = v;

    // Validate final range
    let (min, max) = get_range_64(unsigned);
    let in_range = result >= min && result <= max;

    match (unsigned, result >= 0, in_range) {
        (true, true, true) => Ok(result), // Positive unsigned in range
        (true, true, false) => Err(create_parse_error(string)), // Positive unsigned out of range
        (true, false, _) => Ok(result),   // Negative i64 (large u64) - always valid for unsigned
        (false, _, true) => Ok(result),   // Signed in range
        (false, _, false) => Err(create_parse_error(string)), // Signed out of range
    }
}

/// Attempts to parse a 32-bit integer with F#-style try semantics
#[pyfunction]
#[pyo3(signature = (string, style, unsigned, bitsize, def_value))]
pub fn try_parse_int32(
    string: &str,
    style: i32,
    unsigned: bool,
    bitsize: i32,
    def_value: &Bound<'_, PyAny>,
) -> PyResult<bool> {
    match parse_int32(string, style, unsigned, bitsize, 10) {
        Ok(value) => {
            def_value.setattr("contents", value)?;
            Ok(true)
        }
        Err(_) => Ok(false),
    }
}

/// Attempts to parse a 64-bit integer with F#-style try semantics
#[pyfunction]
#[pyo3(signature = (string, style, unsigned, bitsize, def_value))]
pub fn try_parse_int64(
    string: &str,
    style: i32,
    unsigned: bool,
    bitsize: i32,
    def_value: &Bound<'_, PyAny>,
) -> PyResult<bool> {
    match parse_int64(string, style, unsigned, bitsize, 10) {
        Ok(value) => {
            def_value.setattr("contents", value)?;
            Ok(true)
        }
        Err(_) => Ok(false),
    }
}

/// Registers integer parsing functions with the Python module
///
/// This function adds the parse and try_parse functions to the module
/// so they can be called from Python code.
pub fn register_int_module(m: &Bound<'_, PyModule>) -> PyResult<()> {
    // Specific functions
    m.add_function(wrap_pyfunction!(parse_int32, m)?)?;
    m.add_function(wrap_pyfunction!(parse_int64, m)?)?;
    m.add_function(wrap_pyfunction!(try_parse_int32, m)?)?;
    m.add_function(wrap_pyfunction!(try_parse_int64, m)?)?;

    m.add_function(wrap_pyfunction!(get_range, m)?)?;
    m.add_function(wrap_pyfunction!(get_range_64, m)?)?;
    m.add("ALLOW_HEX_SPECIFIER", ALLOW_HEX_SPECIFIER)?;
    m.add("ALLOW_LEADING_WHITE", ALLOW_LEADING_WHITE)?;
    m.add("ALLOW_TRAILING_WHITE", ALLOW_TRAILING_WHITE)?;
    Ok(())
}
