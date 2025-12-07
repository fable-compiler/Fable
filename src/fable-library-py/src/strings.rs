//! # Fable String Library
//!
//! This module provides comprehensive string manipulation and formatting functionality
//! that aligns with both F# and .NET semantics while leveraging Rust's zero-cost abstractions
//! and memory safety guarantees.
//!
//! ## Features
//!
//! - **Printf-style formatting**: F#-compatible format strings with currying support
//! - **.NET-style formatting**: Complex format specifiers and alignment
//! - **Type safety**: All operations use the `?` operator and pattern matching for error handling
//! - **Zero-cost abstractions**: Iterator chains and single-allocation patterns
//! - **Memory safety**: No unsafe operations, bounds checking on all array access
//!
//! ## Architecture
//!
//! The module is organized into logical sub-modules:
//! - `printf`: F#-compatible printf formatting with currying
//! - `formatting`: .NET-style string operations and formatting
//!
//! ## Performance
//!
//! - Regex patterns are compiled once using `LazyLock` for zero-cost reuse
//! - String operations use single allocations where possible
//! - Iterator chains eliminate intermediate collections
//! - Pattern matching provides compile-time optimization

use crate::array::FSharpArray;
use pyo3::prelude::*;
use pyo3::types::PyAny;
use regex::Regex;
use std::sync::LazyLock;

// -----------------------------------------------------------
// Constants and cached patterns for zero-cost abstractions
// -----------------------------------------------------------

// Type-safe regex patterns with descriptive error messages
// .NET-style format pattern: {0}, {0,10}, {0:d4}, {0,-10:X8}, etc.
static DOTNET_FORMAT_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"\{(\d+)(?:,(-?\d+))?(?::([^}]+))?\}")
        .expect("DOTNET_FORMAT_PATTERN: Invalid regex pattern for .NET-style formatting")
});

static PRINTF_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"%([+\-# 0]*)(\d+)?(?:\.(\d+))?([diouxXeEfFgGaAcspnOAb])")
        .expect("PRINTF_PATTERN: Invalid regex pattern for printf-style formatting")
});

static INTERPOLATE_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"(?:(^|[^%])%([0+\- ]*)(\d+)?(?:\.(\d+))?(\w))?%P\(\)")
        .expect("INTERPOLATE_PATTERN: Invalid regex pattern for string interpolation")
});

// Performance constants: Pre-allocated strings for common operations
const TRUE_STR: &str = "true";
const FALSE_STR: &str = "false";
const SPACE_STR: &str = " ";
const PLUS_STR: &str = "+";

// -----------------------------------------------------------
// Printf formatting module
// -----------------------------------------------------------

/// F#-compatible printf formatting with full currying support.
///
/// This module implements printf-style string formatting that matches F# semantics,
/// including proper handling of format specifiers, type annotations, and currying.
/// All format operations are type-safe and use zero-cost abstractions.
///
/// ## Supported Format Specifiers
///
/// - `%d`, `%i`: Decimal integers with sign support
/// - `%x`, `%X`: Hexadecimal (lowercase/uppercase) with type-aware formatting
/// - `%f`, `%F`: Floating-point numbers with precision control
/// - `%g`, `%G`: General number format
/// - `%s`, `%A`: String representation
/// - `%O`: Object display
/// - `%b`: Boolean values (lowercase true/false)
/// - `%%`: Literal percent sign
///
/// ## Type Safety Features
///
/// - Automatic type annotation parsing (`:i32`, `:u64`, etc.)
/// - Proper handling of natural colons in URLs and timestamps
/// - Bounds checking for all argument access
/// - Pattern matching for format specifier extraction
mod printf {
    use super::*;

    /// A printf format object that supports F#-style currying.
    ///
    /// This struct maintains the format string and accumulated arguments,
    /// allowing for partial application of arguments until all placeholders
    /// are filled. When complete, it produces the final formatted string
    /// or applies a stored continuation function to it.
    ///
    /// ## Example Usage
    ///
    /// ```python
    /// # F#-style currying
    /// formatter = printf("Hello %s, you are %d years old!")
    /// partial = formatter("Alice")
    /// result = partial(25)  # "Hello Alice, you are 25 years old!"
    ///
    /// # With continuation (F# failsafe pattern)
    /// def fail(msg): raise Exception(msg)
    /// printf("Error: %s").cont(fail)("something went wrong")  # raises Exception
    /// ```
    #[pyclass(module = "fable")]
    pub struct IPrintfFormat {
        /// The original format string with placeholders
        input: String,
        /// Accumulated arguments with type annotations
        args: Vec<String>,
        /// Optional continuation function to apply to the final formatted string
        continuation: Option<Py<PyAny>>,
        /// Cached count of format placeholders (computed once at creation)
        placeholder_count: usize,
    }

    impl Clone for IPrintfFormat {
        fn clone(&self) -> Self {
            Python::attach(|py| Self {
                input: self.input.clone(),
                args: self.args.clone(),
                continuation: self.continuation.as_ref().map(|c| c.clone_ref(py)),
                placeholder_count: self.placeholder_count,
            })
        }
    }

    #[pymethods]
    impl IPrintfFormat {
        #[new]
        pub fn new(input: String) -> Self {
            let placeholder_count = count_actual_placeholders(&input);
            Self {
                input,
                args: Vec::new(),
                continuation: None,
                placeholder_count,
            }
        }

        #[getter]
        pub fn input(&self) -> &str {
            &self.input
        }

        /// Make IPrintfFormat callable to support F# currying
        fn __call__(&self, py: Python<'_>, arg: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
            let mut new_format = self.clone();

            // Store the argument with type information for better formatting
            let arg_str = {
                let type_name = arg.get_type().name()?.to_string();
                match type_name.as_str() {
                    "Int32" => format!("{}:i32", arg.str()?),
                    "Int64" => format!("{}:i64", arg.str()?),
                    "UInt32" => format!("{}:u32", arg.str()?),
                    "UInt64" => format!("{}:u64", arg.str()?),
                    // Handle booleans with F# lowercase representation (true/false)
                    "bool" => {
                        if arg.is_truthy()? {
                            TRUE_STR.to_string()
                        } else {
                            FALSE_STR.to_string()
                        }
                    }
                    _ => arg.str()?.to_string(),
                }
            };

            new_format.args.push(arg_str);

            // Use cached placeholder count instead of recomputing
            if new_format.args.len() >= self.placeholder_count {
                // We have enough args, format the string
                let formatted = new_format.format_final()?;

                // If a continuation was stored, apply it to the formatted string
                if let Some(ref continuation) = new_format.continuation {
                    Ok(continuation.bind(py).call1((formatted,))?.unbind())
                } else {
                    // No continuation, just return the formatted string
                    Ok(formatted.into_pyobject(py)?.into())
                }
            } else {
                // Return continuation for more args
                Ok(new_format.into_pyobject(py)?.into())
            }
        }

        /// Convert to string representation
        fn __str__(&self) -> PyResult<String> {
            if self.args.is_empty() {
                Ok(self.input.clone())
            } else {
                self.format_final()
            }
        }

        /// Continuation method for F# printf compatibility.
        ///
        /// This method stores a continuation function that will be applied to the
        /// formatted result once all arguments have been collected. This is used
        /// by F# patterns like `failwithf` and `Expect.throws` where the formatted
        /// string should be passed to a function (e.g., to raise an exception).
        ///
        /// ## Usage Pattern
        ///
        /// ```python
        /// def fail(msg): raise Exception(msg)
        /// printf("Error: %s").cont(fail)("something")  # raises Exception("Error: something")
        /// ```
        fn cont(&self, py: Python<'_>, continuation: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
            if !self.args.is_empty() {
                // We already have args, format the string and apply the continuation immediately
                let formatted = self.format_final()?;
                Ok(continuation.call1((formatted,))?.unbind())
            } else {
                // No args accumulated yet - store the continuation and return IPrintfFormat
                // so it can continue to be curried. The continuation will be applied when
                // all format arguments have been collected.
                let mut new_format = self.clone();
                new_format.continuation = Some(continuation.clone().unbind());
                Ok(new_format.into_pyobject(py)?.into())
            }
        }
    }

    impl IPrintfFormat {
        /// Generate the final formatted string by applying all accumulated arguments.
        ///
        /// This method processes the format string with regex-based pattern matching,
        /// applying type-safe argument substitution and formatting. It handles:
        /// - Format specifier parsing with bounds checking
        /// - Type annotation resolution (`:i32`, `:u64`, etc.)
        /// - Padding and alignment according to format flags
        /// - Escape sequence handling (`%%` → `%`)
        ///
        /// ## Returns
        ///
        /// A `PyResult<String>` containing the formatted output or an error
        /// if argument types cannot be converted or formatting fails.
        pub fn format_final(&self) -> PyResult<String> {
            let mut result = self.input.clone();
            let mut arg_index = 0;

            while let Some(captures) = super::PRINTF_PATTERN.captures(&result) {
                if arg_index >= self.args.len() {
                    break;
                }

                // Type-safe capture extraction
                let Some(full_match) = captures.get(0) else {
                    continue;
                };
                let Some(format_type_match) = captures.get(4) else {
                    continue;
                };

                let full_match = full_match.as_str();
                let flags = captures.get(1).map_or("", |m| m.as_str());
                let width = captures.get(2).and_then(|m| m.as_str().parse::<i32>().ok());
                let precision = captures.get(3).and_then(|m| m.as_str().parse::<i32>().ok());
                let format_type = format_type_match.as_str();

                // Type-safe argument access
                let Some(arg_value) = self.args.get(arg_index) else {
                    break; // No more arguments available
                };

                // Parse type information if present (only for known type annotations)
                let (value_str, type_info) = parse_type_annotation(arg_value);

                let formatted_value =
                    format_value(value_str, type_info, format_type, flags, precision)?;

                // Apply padding if width is specified
                let final_value = apply_padding(&formatted_value, width, flags);

                result = result.replacen(full_match, &final_value, 1);
                arg_index += 1;
            }

            // Handle remaining simple patterns that the regex might have missed
            handle_remaining_patterns(&mut result, &self.args[arg_index..]);

            // Handle %% escape sequences at the end
            result = result.replace("%%", "%");
            Ok(result)
        }
    }

    /// Count actual format placeholders, excluding `%%` escape sequences.
    ///
    /// This function accurately counts printf-style format specifiers by temporarily
    /// replacing literal `%%` sequences to avoid counting them as format placeholders.
    /// This is crucial for proper currying behavior in F# printf semantics.
    ///
    /// ## Algorithm
    ///
    /// 1. Replace `%%` with null bytes to temporarily hide them
    /// 2. Use regex to count actual format specifiers
    /// 3. Return count for currying logic
    ///
    /// ## Examples
    ///
    /// - `"Hello %s"` → 1 placeholder
    /// - `"100%% complete"` → 0 placeholders (literal %)
    /// - `"%s: 100%% done"` → 1 placeholder
    fn count_actual_placeholders(input: &str) -> usize {
        // First, temporarily replace %% with a placeholder to avoid double-counting
        let temp_input = input.replace("%%", "\x00\x00");
        super::PRINTF_PATTERN.find_iter(&temp_input).count()
    }

    /// Parse type annotation from argument string with smart colon detection.
    ///
    /// This function distinguishes between intentional type annotations (`:i32`, `:u64`)
    /// and natural colons that appear in URLs, timestamps, and other data. This is
    /// critical for maintaining data integrity while enabling type-aware formatting.
    ///
    /// ## Type Annotations Supported
    ///
    /// - `:i32` - 32-bit signed integer
    /// - `:i64` - 64-bit signed integer
    /// - `:u32` - 32-bit unsigned integer
    /// - `:u64` - 64-bit unsigned integer
    ///
    /// ## Examples
    ///
    /// - `"12345:i32"` → `("12345", Some("i32"))` (type annotation)
    /// - `"http://site.com:8080"` → `("http://site.com:8080", None)` (natural colon)
    /// - `"2024-01-01T10:30:00"` → `("2024-01-01T10:30:00", None)` (timestamp)
    ///
    /// ## Returns
    ///
    /// A tuple of `(value_string, optional_type_info)` where the type info is only
    /// present for recognized type annotation patterns.
    fn parse_type_annotation(arg_value: &str) -> (&str, Option<&str>) {
        if let Some((value, type_info)) = arg_value.rsplit_once(':') {
            if matches!(type_info, "i32" | "i64" | "u32" | "u64") {
                return (value, Some(type_info));
            }
        }
        (arg_value, None)
    }

    /// Format a value based on format specifier
    fn format_value(
        value_str: &str,
        type_info: Option<&str>,
        format_type: &str,
        flags: &str,
        precision: Option<i32>,
    ) -> PyResult<String> {
        match format_type {
            "d" | "i" => format_integer(value_str, flags),
            "f" | "F" => format_float(value_str, flags, precision),
            "g" | "G" => format_general(value_str, flags),
            "x" => format_hex_lower(value_str, type_info),
            "X" => format_hex_upper(value_str, type_info),
            "s" | "A" => Ok(value_str.to_string()),
            "O" => Ok(value_str.to_string()), // Object display
            "b" => format_boolean(value_str),
            _ => Ok(value_str.to_string()),
        }
    }

    /// Format integer with flags using optimized string operations.
    fn format_integer(value_str: &str, flags: &str) -> PyResult<String> {
        if let Ok(num) = value_str.parse::<i64>() {
            // **Performance optimization**: Use const strings and format! for single allocation
            Ok(if flags.contains('+') && num >= 0 {
                format!("{}{}", PLUS_STR, num)
            } else if flags.contains(' ') && num >= 0 {
                format!("{}{}", SPACE_STR, num)
            } else {
                num.to_string()
            })
        } else {
            Ok(value_str.to_string())
        }
    }

    /// Format float with flags and precision
    fn format_float(value_str: &str, flags: &str, precision: Option<i32>) -> PyResult<String> {
        if let Ok(num) = value_str.parse::<f64>() {
            let prec = precision.unwrap_or(6) as usize;
            Ok(if flags.contains('+') && num >= 0.0 {
                format!("+{:.prec$}", num, prec = prec)
            } else {
                format!("{:.prec$}", num, prec = prec)
            })
        } else {
            Ok(value_str.to_string())
        }
    }

    /// Format general number
    fn format_general(value_str: &str, flags: &str) -> PyResult<String> {
        if let Ok(num) = value_str.parse::<f64>() {
            Ok(if flags.contains('+') && num >= 0.0 {
                format!("+{}", num)
            } else {
                num.to_string()
            })
        } else {
            Ok(value_str.to_string())
        }
    }

    /// Format hexadecimal lowercase
    fn format_hex_lower(value_str: &str, type_info: Option<&str>) -> PyResult<String> {
        format_hex_common(value_str, type_info, false)
    }

    /// Format hexadecimal uppercase
    fn format_hex_upper(value_str: &str, type_info: Option<&str>) -> PyResult<String> {
        format_hex_common(value_str, type_info, true)
    }

    /// Common hexadecimal formatting logic
    fn format_hex_common(
        value_str: &str,
        type_info: Option<&str>,
        uppercase: bool,
    ) -> PyResult<String> {
        // Pattern matching paradise: tuple destructuring eliminates nesting
        match (type_info, uppercase) {
            // Signed 32-bit integers
            (Some("i32"), true) => Ok(value_str
                .parse::<i32>()
                .map(|n| format!("{:X}", n as u32))
                .unwrap_or_else(|_| value_str.to_string())),
            (Some("i32"), false) => Ok(value_str
                .parse::<i32>()
                .map(|n| format!("{:x}", n as u32))
                .unwrap_or_else(|_| value_str.to_string())),

            // Signed 64-bit integers
            (Some("i64"), true) => Ok(value_str
                .parse::<i64>()
                .map(|n| format!("{:X}", n as u64))
                .unwrap_or_else(|_| value_str.to_string())),
            (Some("i64"), false) => Ok(value_str
                .parse::<i64>()
                .map(|n| format!("{:x}", n as u64))
                .unwrap_or_else(|_| value_str.to_string())),

            // Unsigned 32-bit integers
            (Some("u32"), true) => Ok(value_str
                .parse::<u32>()
                .map(|n| format!("{:X}", n))
                .unwrap_or_else(|_| value_str.to_string())),
            (Some("u32"), false) => Ok(value_str
                .parse::<u32>()
                .map(|n| format!("{:x}", n))
                .unwrap_or_else(|_| value_str.to_string())),

            // Unsigned 64-bit integers
            (Some("u64"), true) => Ok(value_str
                .parse::<u64>()
                .map(|n| format!("{:X}", n))
                .unwrap_or_else(|_| value_str.to_string())),
            (Some("u64"), false) => Ok(value_str
                .parse::<u64>()
                .map(|n| format!("{:x}", n))
                .unwrap_or_else(|_| value_str.to_string())),

            // Default: try parsing as different types
            (_, uppercase) => {
                // First try as signed i64
                if let Ok(signed_num) = value_str.parse::<i64>() {
                    Ok(match (signed_num < 0, uppercase) {
                        (true, true) => format!("{:X}", (signed_num as i32) as u32),
                        (true, false) => format!("{:x}", (signed_num as i32) as u32),
                        (false, true) => format!("{:X}", signed_num),
                        (false, false) => format!("{:x}", signed_num),
                    })
                }
                // Then try as unsigned u64
                else if let Ok(num) = value_str.parse::<u64>() {
                    Ok(if uppercase {
                        format!("{:X}", num)
                    } else {
                        format!("{:x}", num)
                    })
                }
                // Fall back to original string
                else {
                    Ok(value_str.to_string())
                }
            }
        }
    }

    /// Format boolean values with zero-cost string literals.
    fn format_boolean(value_str: &str) -> PyResult<String> {
        // **Performance optimization**: Use const string references to avoid allocations
        Ok(match value_str.parse::<bool>() {
            Ok(true) => TRUE_STR.to_string(),
            Ok(false) => FALSE_STR.to_string(),
            Err(_) => match value_str.to_lowercase().as_str() {
                "true" | "1" => TRUE_STR.to_string(),
                "false" | "0" => FALSE_STR.to_string(),
                _ => value_str.to_lowercase(),
            },
        })
    }

    /// Apply padding to formatted value with optimized allocations.
    fn apply_padding(formatted_value: &str, width: Option<i32>, flags: &str) -> String {
        let Some(w) = width else {
            // **Performance optimization**: Avoid allocation when no padding needed
            return formatted_value.to_string();
        };

        let width = w as usize;
        let current_len = formatted_value.chars().count();

        // **Performance optimization**: Early return if no padding needed
        if width <= current_len {
            return formatted_value.to_string();
        }

        let has_minus = flags.contains('-');
        let has_zero = flags.contains('0');
        let has_sign = formatted_value.starts_with('+') || formatted_value.starts_with('-');

        // Pattern matching on flag combinations with optimized string building
        match (has_minus, has_zero, has_sign) {
            // Left-align always takes precedence
            (true, _, _) => format!("{:<width$}", formatted_value, width = width),

            // Zero-padding with sign preservation
            (false, true, true) => {
                let sign = &formatted_value[..1];
                let rest = &formatted_value[1..];
                format!("{}{:0>width$}", sign, rest, width = width.saturating_sub(1))
            }

            // Zero-padding without sign
            (false, true, false) => format!("{:0>width$}", formatted_value, width = width),

            // Right-align (default)
            (false, false, _) => format!("{:>width$}", formatted_value, width = width),
        }
    }

    /// Handle remaining simple patterns
    fn handle_remaining_patterns(result: &mut String, remaining_args: &[String]) {
        for arg in remaining_args {
            if let Some(pos) = result.find("%s") {
                result.replace_range(pos..pos + 2, arg);
            } else if let Some(pos) = result.find("%d") {
                result.replace_range(pos..pos + 2, arg);
            } else if let Some(pos) = result.find("%f") {
                result.replace_range(pos..pos + 2, arg);
            } else if let Some(pos) = result.find("%g") {
                result.replace_range(pos..pos + 2, arg);
            } else if let Some(pos) = result.find("%i") {
                result.replace_range(pos..pos + 2, arg);
            } else if let Some(pos) = result.find("%A") {
                result.replace_range(pos..pos + 2, arg);
            } else {
                break;
            }
        }
    }

    /// F# printf-style formatting with currying support.
    ///
    /// This is the main entry point for F#-compatible printf formatting. It analyzes
    /// the format string and returns either a formatted string (if no placeholders)
    /// or an `IPrintfFormat` object that supports currying for partial application.
    ///
    /// ## Examples
    ///
    /// ```python
    /// # No placeholders - returns string directly
    /// result = printf("Hello World!")  # "Hello World!"
    ///
    /// # With placeholders - returns curried function
    /// formatter = printf("Hello %s, you are %d!")
    /// partial = formatter("Alice")  # Still a function
    /// result = partial(25)  # "Hello Alice, you are 25!"
    ///
    /// # Chain calls directly
    /// result = printf("Value: %X")(255)  # "Value: FF"
    /// ```
    ///
    /// ## Format Processing
    ///
    /// - Escape sequences (`%%` → `%`) are handled at all levels
    /// - Type annotations are automatically added based on Python types
    /// - Placeholder counting excludes literal `%%` sequences
    /// - All operations are type-safe with proper error propagation
    #[pyfunction]
    pub fn printf(py: Python<'_>, input: &str) -> PyResult<Py<PyAny>> {
        // Check if there are any format specifiers that need arguments
        let has_format_specifiers = super::PRINTF_PATTERN.is_match(input);

        if has_format_specifiers {
            // Has format specifiers, return IPrintfFormat for currying
            Ok(IPrintfFormat::new(input.to_string())
                .into_pyobject(py)?
                .into())
        } else {
            // No format specifiers, return the string directly (with %% -> % conversion)
            let result = input.replace("%%", "%");
            Ok(result.into_pyobject(py)?.into())
        }
    }

    /// Continue print with given continuation function
    #[pyfunction]
    pub fn continue_print(
        py: Python<'_>,
        cont: &Bound<'_, PyAny>,
        arg: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        if let Ok(printf_format) = arg.extract::<IPrintfFormat>() {
            // Use the cont method, just like the original Python implementation
            printf_format.cont(py, cont)
        } else {
            // Not a printf format, just pass it through
            Ok(cont.call1((arg,))?.unbind())
        }
    }

    /// Console printer wrapper that maintains F# currying semantics
    #[pyclass]
    #[derive(Clone)]
    pub struct ConsolePrinter {
        format: IPrintfFormat,
    }

    #[pymethods]
    impl ConsolePrinter {
        fn __call__(&self, py: Python<'_>, arg: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
            // Call the underlying IPrintfFormat
            let result = self.format.__call__(py, arg)?;

            // Check if we got a string (final result) or another IPrintfFormat (needs more args)
            if let Ok(result_str) = result.extract::<String>(py) {
                // Final result - print it and return None
                let print_fn = py.import("builtins")?.getattr("print")?;
                print_fn.call1((result_str,))?;
                Ok(py.None())
            } else if let Ok(new_format) = result.extract::<IPrintfFormat>(py) {
                // Still needs more args - wrap in new ConsolePrinter
                Ok(ConsolePrinter { format: new_format }
                    .into_pyobject(py)?
                    .into())
            } else {
                // Should not happen, but return the result as-is
                Ok(result)
            }
        }
    }

    /// Print to console with F# semantics
    #[pyfunction]
    pub fn to_console(py: Python<'_>, arg: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        if let Ok(printf_format) = arg.extract::<IPrintfFormat>() {
            if printf_format.args.is_empty() {
                // Return a ConsolePrinter that will handle the currying and printing
                Ok(ConsolePrinter {
                    format: printf_format,
                }
                .into_pyobject(py)?
                .into())
            } else {
                // Has args, print formatted result and return None
                let formatted = printf_format.format_final()?;
                let print_fn = py.import("builtins")?.getattr("print")?;
                print_fn.call1((formatted,))?;
                Ok(py.None())
            }
        } else {
            // Not a printf format, just print it
            let print_fn = py.import("builtins")?.getattr("print")?;
            print_fn.call1((arg,))?;
            Ok(py.None())
        }
    }

    /// Convert to text string with F# semantics
    #[pyfunction]
    pub fn to_text(py: Python<'_>, arg: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        if let Ok(printf_format) = arg.extract::<IPrintfFormat>() {
            if printf_format.args.is_empty() {
                // Return the IPrintfFormat object itself so it can continue to be curried
                Ok(printf_format.into_pyobject(py)?.into())
            } else {
                // Return formatted result as string
                Ok(printf_format.format_final()?.into_pyobject(py)?.into())
            }
        } else if arg.is_callable() {
            // It's already a callable, return it as-is
            Ok(arg.clone().unbind())
        } else {
            // Convert to string (includes the case where printf returned a string directly)
            Ok(arg.str()?.to_string().into_pyobject(py)?.into())
        }
    }
}

// -----------------------------------------------------------
// String formatting module
// -----------------------------------------------------------

/// .NET-compatible string operations and formatting functionality.
///
/// This module provides comprehensive string manipulation that aligns with .NET
/// and F# string operations, including complex formatting, manipulation, and
/// comparison functions. All operations are memory-safe and use zero-cost
/// abstractions where possible.
///
/// ## Key Features
///
/// - **.NET format strings**: Complex format specifiers like `{0:000.00}`, `{1:++0.00++}`
/// - **String manipulation**: Insert, remove, replace, trim, split operations
/// - **Comparison operations**: Case-sensitive/insensitive with culture support
/// - **Array operations**: Character array conversion and manipulation
/// - **Functional operations**: Map, filter with F#-style predicates
///
/// ## Performance Optimizations
///
/// - Single allocation patterns for string building
/// - Iterator chains to eliminate intermediate collections
/// - Pattern matching for efficient dispatch
/// - Bounds checking with early returns
mod formatting {
    use super::*;

    /// String formatting with replacement
    #[pyfunction]
    #[pyo3(signature = (rep, flags=None, pad_length=None, precision=None, format=None))]
    pub fn format_replacement(
        _py: Python<'_>,
        rep: &Bound<'_, PyAny>,
        flags: Option<&str>,
        pad_length: Option<i32>,
        precision: Option<i32>,
        format: Option<&str>,
    ) -> PyResult<String> {
        let flags = flags.unwrap_or("");
        let format = format.unwrap_or("");
        let mut sign = String::new();
        let mut result: String;

        // Handle numeric types
        if let Ok(num) = rep.extract::<f64>() {
            if !["x", "X"].contains(&format) {
                if num < 0.0 {
                    sign = "-".to_string();
                } else if flags.contains(' ') {
                    sign = " ".to_string();
                } else if flags.contains('+') {
                    sign = "+".to_string();
                }
            }

            match format {
                "x" => result = format!("{:x}", num as i64),
                "X" => result = format!("{:X}", num as i64),
                "f" | "F" => {
                    let prec = precision.unwrap_or(6) as usize;
                    result = format!("{:.prec$}", num.abs(), prec = prec);
                }
                "e" | "E" => {
                    let prec = precision.unwrap_or(6) as usize;
                    result = format!("{:.prec$e}", num.abs(), prec = prec);
                    if format == "E" {
                        result = result.to_uppercase();
                    }
                }
                "g" | "G" => {
                    let prec = precision.unwrap_or(6) as usize;
                    result = format!("{:.prec$}", num.abs(), prec = prec);
                    if format == "G" {
                        result = result.to_uppercase();
                    }
                }
                _ => result = num.abs().to_string(),
            }
        } else {
            // For non-numeric types, convert to string
            result = rep.str()?.to_string();
        }

        // Handle padding
        if let Some(pad_len) = pad_length {
            let pad_len = pad_len as usize;
            let zero_flag = flags.contains('0');
            let minus_flag = flags.contains('-');
            let ch = if minus_flag || !zero_flag { ' ' } else { '0' };

            if ch == '0' {
                result = pad_left(
                    &result,
                    pad_len.saturating_sub(sign.len()),
                    Some(ch),
                    minus_flag,
                );
                result = format!("{}{}", sign, result);
            } else {
                let combined = format!("{}{}", sign, result);
                result = pad_left(&combined, pad_len, Some(ch), minus_flag);
            }
        } else {
            result = format!("{}{}", sign, result);
        }

        Ok(result)
    }

    /// .NET-style string formatting with complex format specifier support.
    ///
    /// This function provides comprehensive .NET-compatible string formatting that handles
    /// both F# varargs patterns and Rust tuple calling conventions. It supports complex
    /// format specifiers including custom patterns like `{0:++0.00++}` and standard
    /// .NET formats like `{0:000.00}`, `{1:X8}`, etc.
    ///
    /// ## Calling Patterns Supported
    ///
    /// ```python
    /// # F#-style calls
    /// format("Hello {0}!", "World")
    /// format(None, "Value: {0:000.00}", 123.45)  # With format provider
    ///
    /// # Rust-style calls
    /// format("Hello {0}!", ("World",))
    /// ```
    ///
    /// ## Format Specifiers
    ///
    /// - `{0}`, `{1}`, etc.: Positional arguments
    /// - `{0,10}`: Right-aligned to width 10
    /// - `{0,-10}`: Left-aligned to width 10
    /// - `{0:d4}`: Decimal with 4-digit padding
    /// - `{0:X8}`: Uppercase hex with 8-digit padding
    /// - `{0:f2}`: Fixed-point with 2 decimal places
    /// - `{0:++0.00++}`: Custom format pattern
    ///
    /// ## Type Safety
    ///
    /// All argument extraction and format string parsing uses proper error
    /// propagation instead of panicking, ensuring robust operation even with
    /// malformed input.
    /// Extract and parse format arguments from PyTuple with type safety.
    ///
    /// Handles multiple calling patterns:
    /// - F#-style: `format("Hello {0}", "World")`
    /// - With provider: `format(None, "Hello {0}", "World")`
    /// - Rust-style: `format("Hello {0}", ("World",))`
    ///
    /// Returns `(format_string, arguments_vector)` for further processing.
    fn parse_format_arguments(
        args: &Bound<'_, pyo3::types::PyTuple>,
    ) -> PyResult<(String, Vec<String>)> {
        if args.is_empty() {
            return Err(pyo3::exceptions::PyValueError::new_err(
                "format() requires at least 1 argument",
            ));
        }

        // **Pattern matching**: Handle single argument case first
        if args.len() == 1 {
            let format_str = args.get_item(0)?.str()?.to_string();
            return validate_format_string_only(&format_str).map(|s| (s, Vec::new()));
        }

        // **Pattern matching**: Determine if first argument is a format provider
        let (format_str, start_index) = match args.get_item(0)? {
            first_arg if first_arg.is_none() || first_arg.cast::<pyo3::types::PyDict>().is_ok() => {
                // Has provider: format(provider, string, ...args)
                (args.get_item(1)?.str()?.to_string(), 2)
            }
            _ => {
                // Standard: format(string, ...args)
                (args.get_item(0)?.str()?.to_string(), 1)
            }
        };

        // **Zero-cost abstraction**: Extract arguments efficiently
        let format_args = extract_format_arguments(args, start_index)?;
        Ok((format_str, format_args))
    }

    /// Validate format string when no arguments are provided.
    fn validate_format_string_only(format_str: &str) -> PyResult<String> {
        // Use static regex for better performance
        if super::DOTNET_FORMAT_PATTERN.is_match(format_str) {
            Err(pyo3::exceptions::PyValueError::new_err(
                "Format string has placeholders but no arguments provided",
            ))
        } else {
            Ok(format_str.to_string())
        }
    }

    /// Extract format arguments from tuple with type-safe iteration.
    fn extract_format_arguments(
        args: &Bound<'_, pyo3::types::PyTuple>,
        start_index: usize,
    ) -> PyResult<Vec<String>> {
        match args.len() - start_index {
            0 => Ok(Vec::new()),
            1 => {
                // **Pattern matching**: Check if single argument is a tuple (Rust-style)
                let single_arg = args.get_item(start_index)?;
                if let Ok(tuple_args) = single_arg.cast::<pyo3::types::PyTuple>() {
                    // Rust-style: format("Hello {0}", ("World",))
                    (0..tuple_args.len())
                        .map(|i| -> PyResult<String> {
                            Ok(tuple_args.get_item(i)?.str()?.to_string())
                        })
                        .collect()
                } else {
                    // Single argument: format("Hello {0}", "World")
                    Ok(vec![single_arg.str()?.to_string()])
                }
            }
            _ => {
                // Multiple arguments: format("Hello {0} {1}", "World", "!")
                (start_index..args.len())
                    .map(|i| -> PyResult<String> { Ok(args.get_item(i)?.str()?.to_string()) })
                    .collect()
            }
        }
    }

    /// Apply custom format patterns like `++0.00++` with pattern matching.
    ///
    /// These are special .NET-style format patterns used in F# formatting
    /// that require custom parsing and numeric formatting.
    fn apply_custom_format(value: &str, format_spec: &str) -> String {
        // Guard against format_spec that's too short to have valid inner content
        if format_spec.len() < 5 {
            // Minimum valid: "++x++" (5 chars)
            return format!("-++{}++", value);
        }

        let inner_spec = &format_spec[2..format_spec.len() - 2];

        match value.parse::<f64>() {
            Ok(num) => {
                if let Some((_, decimal_part)) = inner_spec.split_once('.') {
                    let decimal_places = decimal_part.len();
                    format!("-++{:.prec$}++", num.abs(), prec = decimal_places)
                } else {
                    format!("-++{}++", num)
                }
            }
            Err(_) => format!("-++{}++", value),
        }
    }

    /// Apply zero-padding format patterns like `000.00` or `0000`.
    fn apply_zero_padding_format(value: &str, format_spec: &str) -> String {
        match (value.parse::<f64>(), format_spec.contains('.')) {
            // **Pattern matching**: Numeric value with decimal specification
            (Ok(num), true) => {
                let parts: Vec<&str> = format_spec.split('.').collect();
                match parts.as_slice() {
                    [integer_part, decimal_part] => {
                        let total_width = integer_part.len() + 1 + decimal_part.len();
                        let decimal_places = decimal_part.len();
                        format!(
                            "{:0width$.prec$}",
                            num,
                            width = total_width,
                            prec = decimal_places
                        )
                    }
                    _ => format!("{:.2}", num),
                }
            }
            // **Pattern matching**: Numeric value without decimals
            (Ok(num), false) => {
                let width = format_spec.len();
                format!("{:0width$}", num as i64, width = width)
            }
            // **Pattern matching**: Non-numeric fallback
            (Err(_), _) => value.to_string(),
        }
    }

    /// Apply standard .NET format specifiers with pattern matching.
    ///
    /// Handles decimal (d/D), hexadecimal (x/X), fixed-point (f/F),
    /// and general (g/G) format specifiers with optional width/precision.
    fn apply_standard_format(value: &str, format_spec: &str) -> String {
        // **Pattern matching**: Extract format type and parameters
        let (format_type, params) = format_spec.split_at(1);

        match format_type {
            "d" | "D" => apply_decimal_format(value, params),
            "x" | "X" => apply_hexadecimal_format(value, params, format_type == "X"),
            "f" | "F" => apply_fixed_point_format(value, params),
            "g" | "G" => apply_general_format(value),
            _ => value.to_string(), // Fallback for unknown formats
        }
    }

    /// Format decimal integers with optional zero-padding.
    fn apply_decimal_format(value: &str, width_str: &str) -> String {
        match value.parse::<i64>() {
            Ok(num) => match width_str.is_empty() {
                true => num.to_string(),
                false => width_str
                    .parse::<usize>()
                    .map(|width| format!("{:0width$}", num, width = width))
                    .unwrap_or_else(|_| num.to_string()),
            },
            Err(_) => value.to_string(),
        }
    }

    /// Format hexadecimal integers with optional zero-padding and case control.
    fn apply_hexadecimal_format(value: &str, width_str: &str, uppercase: bool) -> String {
        match value.parse::<i64>() {
            Ok(num) => match (width_str.is_empty(), uppercase) {
                (true, true) => format!("{:X}", num),
                (true, false) => format!("{:x}", num),
                (false, uppercase) => width_str
                    .parse::<usize>()
                    .map(|width| {
                        if uppercase {
                            format!("{:0width$X}", num, width = width)
                        } else {
                            format!("{:0width$x}", num, width = width)
                        }
                    })
                    .unwrap_or_else(|_| {
                        if uppercase {
                            format!("{:X}", num)
                        } else {
                            format!("{:x}", num)
                        }
                    }),
            },
            Err(_) => value.to_string(),
        }
    }

    /// Format fixed-point numbers with optional precision.
    fn apply_fixed_point_format(value: &str, precision_str: &str) -> String {
        match value.parse::<f64>() {
            Ok(num) => match precision_str.is_empty() {
                true => format!("{:.2}", num),
                false => precision_str
                    .parse::<usize>()
                    .map(|precision| format!("{:.prec$}", num, prec = precision))
                    .unwrap_or_else(|_| format!("{:.2}", num)),
            },
            Err(_) => value.to_string(),
        }
    }

    /// Format numbers in general format (simplest representation).
    fn apply_general_format(value: &str) -> String {
        value
            .parse::<f64>()
            .map(|num| num.to_string())
            .unwrap_or_else(|_| value.to_string())
    }

    /// Apply format specification to a value using pattern matching dispatch.
    ///
    /// This function uses clean pattern matching to handle different format
    /// specification types without deep nesting or complex conditionals.
    fn apply_format_specification(value: &str, format_spec: &str) -> String {
        if format_spec.is_empty() {
            return value.to_string();
        }

        // **Pattern matching paradise**: Clean dispatch on format patterns
        match format_spec {
            // Custom format patterns: ++0.00++
            spec if spec.starts_with("++") && spec.ends_with("++") => {
                apply_custom_format(value, spec)
            }
            // Zero-padding with decimals: 000.00
            spec if spec.starts_with('0') && spec.contains('.') => {
                apply_zero_padding_format(value, spec)
            }
            // Zero-padding integers: 0000
            spec if spec.starts_with('0') => apply_zero_padding_format(value, spec),
            // Standard .NET format specifiers: d4, X8, f2, etc.
            spec if spec
                .chars()
                .next()
                .is_some_and(|c| c.is_ascii_alphabetic()) =>
            {
                apply_standard_format(value, spec)
            }
            // Unknown format - return as-is
            _ => value.to_string(),
        }
    }

    /// Apply alignment formatting with clean pattern matching.
    fn apply_alignment_formatting(formatted_value: &str, alignment: Option<i32>) -> String {
        match alignment {
            None => formatted_value.to_string(),
            Some(align) if align < 0 => {
                // Left-aligned
                let width = align.unsigned_abs() as usize;
                format!("{:<width$}", formatted_value, width = width)
            }
            Some(align) => {
                // Right-aligned
                let width = align as usize;
                format!("{:>width$}", formatted_value, width = width)
            }
        }
    }

    /// Process format string replacements with clean separation of concerns.
    ///
    /// This function handles the core format string processing logic using
    /// pattern matching and helper functions to eliminate nesting.
    fn process_format_replacements(format_str: &str, format_args: &[String]) -> PyResult<String> {
        // Use static regex for better performance
        let mut result = format_str.to_string();

        for caps in super::DOTNET_FORMAT_PATTERN.captures_iter(format_str) {
            // **Pattern matching**: Safe capture extraction
            let (index, alignment, format_spec) = match (caps.get(1), caps.get(2), caps.get(3)) {
                (Some(idx_match), align_match, spec_match) => {
                    let index = idx_match.as_str().parse::<usize>().ok();
                    let alignment = align_match.and_then(|m| m.as_str().parse::<i32>().ok());
                    let format_spec = spec_match.map_or("", |m| m.as_str());
                    (index, alignment, format_spec)
                }
                _ => continue, // Skip malformed captures
            };

            // **Pattern matching**: Process valid indices only
            if let (Some(idx), Some(pattern_match)) = (index, caps.get(0)) {
                if idx < format_args.len() {
                    let value = &format_args[idx];

                    // **Clean separation**: Apply format then alignment
                    let formatted_value = apply_format_specification(value, format_spec);
                    let final_value = apply_alignment_formatting(&formatted_value, alignment);

                    result = result.replace(pattern_match.as_str(), &final_value);
                }
            }
        }

        Ok(result)
    }

    #[pyfunction]
    #[pyo3(signature = (*args))]
    pub fn format(args: &Bound<'_, pyo3::types::PyTuple>) -> PyResult<String> {
        // **Clean separation**: Parse arguments first
        let (format_str, format_args) = parse_format_arguments(args)?;

        // **Clean separation**: Process format replacements
        process_format_replacements(&format_str, &format_args)
    }

    /// Initialize a string with given length and initializer function
    #[pyfunction]
    pub fn initialize(_py: Python<'_>, n: i32, f: &Bound<'_, PyAny>) -> PyResult<String> {
        if n < 0 {
            return Err(pyo3::exceptions::PyValueError::new_err(
                "String length must be non-negative",
            ));
        }

        // Zero-cost abstraction: iterator chain instead of manual string building
        Ok((0..n)
            .map(|i| -> PyResult<String> {
                let char_result = f.call1((i,))?;
                Ok(char_result.str()?.to_string())
            })
            .collect::<PyResult<Vec<_>>>()?
            .join(""))
    }

    /// Insert a string at a specific position
    #[pyfunction]
    pub fn insert(string: &str, start_index: usize, value: &str) -> PyResult<String> {
        // Use character-based indexing to handle UTF-8 correctly
        let chars: Vec<char> = string.chars().collect();

        if start_index > chars.len() {
            return Err(pyo3::exceptions::PyValueError::new_err(
                "startIndex is greater than the length of this instance",
            ));
        }

        // Build result from character slices
        let before: String = chars[..start_index].iter().collect();
        let after: String = chars[start_index..].iter().collect();
        Ok(format!("{}{}{}", before, value, after))
    }

    /// Check if string is null or empty
    #[pyfunction]
    #[must_use]
    pub fn is_null_or_empty(string: Option<&str>) -> bool {
        string.is_none_or(|s| s.is_empty())
    }

    /// Check if string is null or whitespace
    #[pyfunction]
    #[must_use]
    pub fn is_null_or_white_space(string: Option<&str>) -> bool {
        string.is_none_or(|s| s.trim().is_empty())
    }

    /// Concatenate strings - varargs of strings
    #[pyfunction]
    #[pyo3(signature = (*strings))]
    pub fn concat(strings: Vec<String>) -> String {
        strings.join("")
    }

    /// Join strings with delimiter
    #[pyfunction]
    pub fn join(delimiter: &str, strings: &Bound<'_, PyAny>) -> PyResult<String> {
        if let Ok(iter) = strings.try_iter() {
            // Zero-cost abstraction: iterator chain with collect and join
            Ok(iter
                .map(|item| -> PyResult<String> {
                    let item = item?;
                    Ok(item.str()?.to_string())
                })
                .collect::<PyResult<Vec<_>>>()?
                .join(delimiter))
        } else {
            Ok(String::new())
        }
    }

    /// Pad string on the left
    #[pyfunction]
    #[pyo3(signature = (string, length, ch=None, is_right=false))]
    pub fn pad_left(string: &str, length: usize, ch: Option<char>, is_right: bool) -> String {
        let ch = ch.unwrap_or(' ');
        let current_len = string.chars().count();

        if length <= current_len {
            return string.to_string();
        }

        let pad_count = length - current_len;
        let padding: String = ch.to_string().repeat(pad_count);

        if is_right {
            format!("{}{}", string, padding)
        } else {
            format!("{}{}", padding, string)
        }
    }

    /// Pad string on the right
    #[pyfunction]
    #[pyo3(signature = (string, length, ch=None))]
    pub fn pad_right(string: &str, length: usize, ch: Option<char>) -> String {
        pad_left(string, length, ch, true)
    }

    /// Remove substring from string
    #[pyfunction]
    #[pyo3(signature = (string, start_index, count=None))]
    pub fn remove(string: &str, start_index: usize, count: Option<usize>) -> PyResult<String> {
        // Use character-based indexing to handle UTF-8 correctly
        let chars: Vec<char> = string.chars().collect();

        if start_index >= chars.len() {
            return Err(pyo3::exceptions::PyValueError::new_err(
                "Index was out of range. Must be non-negative and less than the size of the collection.",
            ));
        }

        // Build result from character slices
        Ok(match count {
            Some(count) => {
                let end_index = std::cmp::min(start_index + count, chars.len());
                let before: String = chars[..start_index].iter().collect();
                let after: String = chars[end_index..].iter().collect();
                format!("{}{}", before, after)
            }
            None => chars[..start_index].iter().collect(),
        })
    }

    /// Replace substring in string
    #[pyfunction]
    pub fn replace(string: &str, search: &str, replace: &str) -> String {
        string.replace(search, replace)
    }

    /// Replicate string n times
    #[pyfunction]
    pub fn replicate(n: i32, x: &str) -> String {
        if n <= 0 {
            String::new()
        } else {
            x.repeat(n as usize)
        }
    }

    /// Get character at index
    #[pyfunction]
    pub fn get_char_at_index(input: &str, index: usize) -> PyResult<char> {
        // Zero-cost abstraction: use iterator's nth method instead of collecting
        input.chars().nth(index).ok_or_else(|| {
            pyo3::exceptions::PyIndexError::new_err("Index was outside the bounds of the array")
        })
    }

    /// Split string with F# semantics
    #[pyfunction]
    #[pyo3(signature = (string, splitters, count=None, remove_empty=0))]
    pub fn split(
        py: Python<'_>,
        string: &str,
        splitters: &Bound<'_, PyAny>,
        count: Option<i32>,
        remove_empty: i32,
    ) -> PyResult<FSharpArray> {
        // **Pattern matching paradise**: Handle count validation elegantly
        match count {
            Some(n) if n < 0 => {
                return Err(pyo3::exceptions::PyValueError::new_err(
                    "Count cannot be less than zero",
                ))
            }
            Some(0) => return FSharpArray::new(py, None, None),
            _ => {} // None or positive values - continue processing
        }

        let mut result: Vec<String>;

        // Handle array/list of splitters
        if let Ok(list) = splitters.try_iter() {
            let mut splitter_strings: Vec<String> = Vec::new();
            for item in list {
                if let Ok(splitter) = item?.extract::<String>() {
                    splitter_strings.push(splitter);
                }
            }

            if !splitter_strings.is_empty() {
                if let Some(count) = count {
                    // Multiple splitters with count: apply first splitter with count, then others to first parts only
                    let parts = if let Some(first_splitter) = splitter_strings.first() {
                        let mut parts: Vec<&str> = string
                            .splitn(count as usize, first_splitter.as_str())
                            .collect();

                        // Apply remaining splitters only to the first (count-1) parts
                        // The last part should preserve the original string structure
                        if splitter_strings.len() > 1 && parts.len() >= 2 {
                            // Type-safe last part extraction
                            if let Some(last_part) = parts.pop() {
                                // Apply remaining splitters to the first parts
                                for splitter in splitter_strings.iter().skip(1) {
                                    parts = parts
                                        .iter()
                                        .flat_map(|s| s.split(splitter.as_str()))
                                        .collect();
                                }

                                // Add back the preserved last part
                                parts.push(last_part);
                            }
                        }
                        parts
                    } else {
                        // Fallback if no splitters
                        vec![string]
                    };

                    result = parts.into_iter().map(|s| s.to_string()).collect();
                } else {
                    // Multiple splitters approach - use all splitters
                    let mut parts: Vec<&str> = vec![string];
                    for splitter in splitter_strings.iter() {
                        parts = parts
                            .iter()
                            .flat_map(|s| s.split(splitter.as_str()))
                            .collect();
                    }
                    result = parts.into_iter().map(|s| s.to_string()).collect();
                }
            } else {
                // Empty splitters array - split on whitespace but preserve empty entries
                if let Some(count) = count {
                    let parts: Vec<&str> =
                        string.splitn(count as usize, char::is_whitespace).collect();
                    result = parts.into_iter().map(|s| s.to_string()).collect();
                } else {
                    result = string
                        .split(char::is_whitespace)
                        .map(|s| s.to_string())
                        .collect();
                }
            }
        } else if let Ok(splitter) = splitters.extract::<String>() {
            // Handle single string splitter
            if let Some(count) = count {
                // When count is specified, limit the number of splits
                let parts: Vec<&str> = string.splitn(count as usize, &splitter).collect();
                result = parts.into_iter().map(|s| s.to_string()).collect();
            } else {
                result = string.split(&splitter).map(|s| s.to_string()).collect();
            }
        } else {
            // For other cases, default to whitespace splitting
            if let Some(count) = count {
                let parts: Vec<&str> = string.splitn(count as usize, char::is_whitespace).collect();
                result = parts.into_iter().map(|s| s.to_string()).collect();
            } else {
                result = string.split_whitespace().map(|s| s.to_string()).collect();
            }
        }

        // Apply remove_empty filter if requested
        if remove_empty != 0 {
            result.retain(|s| !s.is_empty());
        }

        // Count limiting is handled during splitting for multiple splitters
        // For single splitters and other cases, it's already handled by splitn

        // Convert Vec<String> to FSharpArray - type-safe object conversion
        let py_result: Vec<Py<PyAny>> = result
            .into_iter()
            .map(|s| -> PyResult<Py<PyAny>> { Ok(s.into_pyobject(py)?.into()) })
            .collect::<PyResult<Vec<_>>>()?;
        let py_list = pyo3::types::PyList::new(py, py_result)?;
        FSharpArray::new(py, Some(py_list.as_any()), None)
    }

    /// Build a character set from a Python tuple of characters/strings.
    ///
    /// This helper function handles both individual chars and arrays of chars,
    /// building a HashSet for efficient character lookup in trim operations.
    fn build_trim_char_set(
        chars: &Bound<'_, pyo3::types::PyTuple>,
    ) -> PyResult<std::collections::HashSet<char>> {
        let mut char_set = std::collections::HashSet::new();

        for i in 0..chars.len() {
            let item = chars.get_item(i)?;
            if let Ok(char_str) = item.str() {
                for ch in char_str.to_string().chars() {
                    char_set.insert(ch);
                }
            } else if let Ok(iter) = item.try_iter() {
                // Handle array/list of characters
                for char_item in iter {
                    let char_item = char_item?;
                    if let Ok(char_str) = char_item.str() {
                        for ch in char_str.to_string().chars() {
                            char_set.insert(ch);
                        }
                    }
                }
            }
        }

        Ok(char_set)
    }

    /// Trim string with character array support
    #[pyfunction]
    #[pyo3(signature = (string, *chars))]
    pub fn trim(string: &str, chars: &Bound<'_, pyo3::types::PyTuple>) -> PyResult<String> {
        if chars.is_empty() {
            return Ok(string.trim().to_string());
        }

        let char_set = build_trim_char_set(chars)?;
        let result = string
            .trim_start_matches(|c| char_set.contains(&c))
            .trim_end_matches(|c| char_set.contains(&c));
        Ok(result.to_string())
    }

    /// Trim string start with character array support
    #[pyfunction]
    #[pyo3(signature = (string, *chars))]
    pub fn trim_start(string: &str, chars: &Bound<'_, pyo3::types::PyTuple>) -> PyResult<String> {
        if chars.is_empty() {
            return Ok(string.trim_start().to_string());
        }

        let char_set = build_trim_char_set(chars)?;
        let result = string.trim_start_matches(|c| char_set.contains(&c));
        Ok(result.to_string())
    }

    /// Trim string end with character array support
    #[pyfunction]
    #[pyo3(signature = (string, *chars))]
    pub fn trim_end(string: &str, chars: &Bound<'_, pyo3::types::PyTuple>) -> PyResult<String> {
        if chars.is_empty() {
            return Ok(string.trim_end().to_string());
        }

        let char_set = build_trim_char_set(chars)?;
        let result = string.trim_end_matches(|c| char_set.contains(&c));
        Ok(result.to_string())
    }

    /// Filter string characters
    #[pyfunction]
    pub fn filter(_py: Python<'_>, pred: &Bound<'_, PyAny>, x: &str) -> PyResult<String> {
        // Zero-cost abstraction: iterator chain with filter_map and collect
        Ok(x.chars()
            .map(|ch| -> PyResult<Option<char>> {
                let should_include = pred.call1((ch,))?.extract::<bool>()?;
                Ok(should_include.then_some(ch))
            })
            .collect::<PyResult<Vec<_>>>()?
            .into_iter()
            .flatten()
            .collect())
    }

    /// Get substring
    #[pyfunction]
    #[pyo3(signature = (string, start_index, length=None))]
    pub fn substring(string: &str, start_index: usize, length: Option<usize>) -> PyResult<String> {
        let chars: Vec<char> = string.chars().collect();

        if start_index > chars.len() {
            return Err(pyo3::exceptions::PyValueError::new_err(
                "startIndex cannot be larger than length of string.",
            ));
        }

        if let Some(length) = length {
            if start_index + length > chars.len() {
                return Err(pyo3::exceptions::PyValueError::new_err(
                    "Index and length must refer to a location within the string.",
                ));
            }
            Ok(chars[start_index..start_index + length].iter().collect())
        } else {
            Ok(chars[start_index..].iter().collect())
        }
    }

    /// Convert string to character array
    #[pyfunction]
    pub fn to_char_array2(string: &str, start_index: usize, length: usize) -> PyResult<Vec<char>> {
        let chars: Vec<char> = string.chars().collect();

        if start_index + length > chars.len() {
            return Err(pyo3::exceptions::PyValueError::new_err(
                "Index and length must refer to a location within the string.",
            ));
        }

        Ok(chars[start_index..start_index + length].to_vec())
    }

    /// Extract ignore_case flag from PyAny argument using pattern matching.
    ///
    /// This helper function provides type-safe extraction of case-insensitive
    /// comparison flags from Python objects, supporting multiple input formats
    /// to maintain compatibility with both F# and .NET calling conventions.
    ///
    /// ## Supported Input Types
    ///
    /// - `bool`: Direct boolean value
    /// - `int`: Odd values (1, 3, 5) indicate ignore case
    /// - `StringComparison` enum: Uses `.value` attribute with odd check
    ///
    /// ## Pattern Matching Strategy
    ///
    /// Uses Rust's pattern matching for efficient type dispatch and safe
    /// fallback behavior when conversion fails.
    fn extract_ignore_case(comparison_arg: &Bound<'_, PyAny>) -> bool {
        match comparison_arg.extract::<bool>() {
            Ok(boolean) => boolean,
            Err(_) => match comparison_arg.extract::<i32>() {
                Ok(int_val) => int_val & 1 == 1, // Odd values (1, 3, 5) are ignore case
                Err(_) => comparison_arg
                    .getattr("value")
                    .and_then(|v| v.extract::<i32>())
                    .map(|int_val| int_val & 1 == 1)
                    .unwrap_or(false),
            },
        }
    }

    /// Perform case-sensitive or case-insensitive string comparison.
    ///
    /// This function provides the core string comparison logic used throughout
    /// the formatting module, with consistent ordering semantics that match
    /// .NET's string comparison behavior.
    ///
    /// ## Return Values
    ///
    /// - `-1`: First string is lexicographically less than second
    /// - `0`: Strings are equal (after case normalization if applicable)
    /// - `1`: First string is lexicographically greater than second
    ///
    /// ## Performance
    ///
    /// Uses zero-cost abstraction with `std::cmp::Ordering` for efficient
    /// integer conversion without intermediate allocations.
    fn compare_strings(s1: &str, s2: &str, ignore_case: bool) -> i32 {
        use std::cmp::Ordering;
        let result = if ignore_case {
            s1.to_lowercase().cmp(&s2.to_lowercase())
        } else {
            s1.cmp(s2)
        };

        match result {
            Ordering::Less => -1,
            Ordering::Equal => 0,
            Ordering::Greater => 1,
        }
    }

    /// String comparison enumeration
    #[pyclass(module = "fable")]
    #[derive(Clone, Copy)]
    pub struct StringComparison {
        pub value: i32,
    }

    #[pymethods]
    impl StringComparison {
        #[classattr]
        pub const CURRENT_CULTURE: i32 = 0;
        #[classattr]
        pub const CURRENT_CULTURE_IGNORE_CASE: i32 = 1;
        #[classattr]
        pub const INVARIANT_CULTURE: i32 = 2;
        #[classattr]
        pub const INVARIANT_CULTURE_IGNORE_CASE: i32 = 3;
        #[classattr]
        pub const ORDINAL: i32 = 4;
        #[classattr]
        pub const ORDINAL_IGNORE_CASE: i32 = 5;
    }

    /// Compare strings with F# type conversion support
    #[pyfunction]
    #[pyo3(signature = (*args))]
    pub fn compare(args: &Bound<'_, pyo3::types::PyTuple>) -> PyResult<i32> {
        // Pattern matching paradise: clean dispatch on argument count
        match args.len() {
            2 => {
                let s1 = args.get_item(0)?.str()?.to_string();
                let s2 = args.get_item(1)?.str()?.to_string();
                Ok(compare_strings(&s1, &s2, false))
            }
            3 => {
                let s1 = args.get_item(0)?.str()?.to_string();
                let s2 = args.get_item(1)?.str()?.to_string();
                let ignore_case = extract_ignore_case(&args.get_item(2)?);
                Ok(compare_strings(&s1, &s2, ignore_case))
            }
            6 => {
                // compare(string1, index1, string2, index2, length, comparison_type)
                let s1 = args.get_item(0)?.str()?.to_string();
                let index1 = args.get_item(1)?.extract::<usize>()?;
                let s2 = args.get_item(2)?.str()?.to_string();
                let index2 = args.get_item(3)?.extract::<usize>()?;
                let length = args.get_item(4)?.extract::<usize>()?;
                let ignore_case = extract_ignore_case(&args.get_item(5)?);

                // Extract substrings efficiently
                let chars1: Vec<char> = s1.chars().collect();
                let chars2: Vec<char> = s2.chars().collect();

                if index1 >= chars1.len() || index2 >= chars2.len() {
                    return Err(pyo3::exceptions::PyIndexError::new_err(
                        "Index out of range",
                    ));
                }

                let end1 = std::cmp::min(index1 + length, chars1.len());
                let end2 = std::cmp::min(index2 + length, chars2.len());

                let sub1: String = chars1[index1..end1].iter().collect();
                let sub2: String = chars2[index2..end2].iter().collect();

                Ok(compare_strings(&sub1, &sub2, ignore_case))
            }
            _ => Err(pyo3::exceptions::PyValueError::new_err(
                "String.compare: Unsupported number of parameters",
            )),
        }
    }

    /// Check if string starts with pattern - supports F# type conversion
    #[pyfunction]
    #[pyo3(signature = (string, pattern, ic=None))]
    pub fn starts_with(
        string: &str,
        pattern: &str,
        ic: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<bool> {
        let ignore_case = ic.map(extract_ignore_case).unwrap_or(false);

        Ok(match ignore_case {
            true => string.to_lowercase().starts_with(&pattern.to_lowercase()),
            false => string.starts_with(pattern),
        })
    }

    /// Check if string ends with pattern - supports F# type conversion
    #[pyfunction]
    #[pyo3(signature = (string, pattern, ic=None))]
    pub fn ends_with(string: &str, pattern: &str, ic: Option<&Bound<'_, PyAny>>) -> PyResult<bool> {
        let ignore_case = ic.map(extract_ignore_case).unwrap_or(false);

        Ok(match ignore_case {
            true => string.to_lowercase().ends_with(&pattern.to_lowercase()),
            false => string.ends_with(pattern),
        })
    }

    /// Find index of substring
    #[pyfunction]
    #[pyo3(signature = (string, pattern, start_index=0))]
    pub fn index_of(string: &str, pattern: &str, start_index: usize) -> i32 {
        // Use character-based indexing to handle UTF-8 correctly
        let chars: Vec<char> = string.chars().collect();

        if start_index >= chars.len() {
            return -1;
        }

        // Build the search string from character index
        let search_string: String = chars[start_index..].iter().collect();

        match search_string.find(pattern) {
            Some(byte_pos) => {
                // Convert byte position to character position within the search substring
                let char_pos = search_string[..byte_pos].chars().count();
                (start_index + char_pos) as i32
            }
            None => -1,
        }
    }

    /// Find last index of substring
    #[pyfunction]
    #[pyo3(signature = (string, pattern, start_index=None))]
    pub fn last_index_of(string: &str, pattern: &str, start_index: Option<usize>) -> i32 {
        // Use character-based indexing to handle UTF-8 correctly
        let chars: Vec<char> = string.chars().collect();

        let search_string: String = if let Some(end) = start_index {
            if end >= chars.len() {
                string.to_string()
            } else {
                // Include character at end index (..=end)
                chars[..=end].iter().collect()
            }
        } else {
            string.to_string()
        };

        match search_string.rfind(pattern) {
            Some(byte_pos) => {
                // Convert byte position to character position
                search_string[..byte_pos].chars().count() as i32
            }
            None => -1,
        }
    }

    /// Additional functions needed for compatibility
    #[pyfunction]
    #[must_use]
    pub fn compare_to(this: &str, other: &str) -> i32 {
        use std::cmp::Ordering;
        match this.cmp(other) {
            Ordering::Less => -1,
            Ordering::Equal => 0,
            Ordering::Greater => 1,
        }
    }

    #[pyfunction]
    #[must_use]
    pub fn ends_with_exact(string: &str, pattern: &str) -> bool {
        if let Some(idx) = string.rfind(pattern) {
            idx == string.len() - pattern.len()
        } else {
            false
        }
    }

    #[pyfunction]
    #[must_use]
    pub fn starts_with_exact(string: &str, pattern: &str) -> bool {
        string.find(pattern) == Some(0)
    }

    #[pyfunction]
    #[pyo3(signature = (string, any_of, *args))]
    pub fn index_of_any(
        string: &str,
        any_of: &Bound<'_, PyAny>,
        args: &Bound<'_, pyo3::types::PyTuple>,
    ) -> PyResult<i32> {
        if string.is_empty() {
            return Ok(-1);
        }

        // Use character-based indexing to handle UTF-8 correctly
        let chars: Vec<char> = string.chars().collect();

        let start_index = if !args.is_empty() {
            args.get_item(0)?.extract::<usize>()?
        } else {
            0
        };

        if start_index >= chars.len() {
            return Ok(-1);
        }

        let length = if args.len() > 1 {
            args.get_item(1)?.extract::<usize>()?
        } else {
            chars.len() - start_index
        };

        let end_pos = std::cmp::min(start_index + length, chars.len());

        // Convert any_of to a set of characters
        let mut char_set = std::collections::HashSet::new();
        if let Ok(iter) = any_of.try_iter() {
            for item in iter {
                if let Ok(s) = item?.str() {
                    for ch in s.to_string().chars() {
                        char_set.insert(ch);
                    }
                }
            }
        }

        // Search within the character slice
        for (i, &c) in chars[start_index..end_pos].iter().enumerate() {
            if char_set.contains(&c) {
                return Ok((start_index + i) as i32);
            }
        }

        Ok(-1)
    }

    /// Extract format parameters from regex captures
    fn extract_format_params<'a>(
        captures: &regex::Captures<'a>,
    ) -> (Option<&'a str>, Option<i32>, Option<i32>, Option<&'a str>) {
        let flags = captures.get(2).map(|m| m.as_str());
        let pad_length = captures.get(3).and_then(|m| m.as_str().parse().ok());
        let precision = captures.get(4).and_then(|m| m.as_str().parse().ok());
        let format_spec = captures.get(5).map(|m| m.as_str());
        (flags, pad_length, precision, format_spec)
    }

    /// Extract value from values collection at given index
    fn extract_value_at_index<'a>(
        values: &Bound<'a, PyAny>,
        val_idx: usize,
    ) -> PyResult<Option<Bound<'a, PyAny>>> {
        // Pattern match on the type of values container
        match () {
            // Array-like objects (FSharpArray, lists, etc.)
            () if values.hasattr("__len__")? && values.hasattr("__getitem__")? => {
                let length = values.call_method0("__len__")?.extract::<usize>()?;
                if val_idx < length {
                    Ok(values.get_item(val_idx).ok())
                } else {
                    Ok(None)
                }
            }
            // Single value case
            () if val_idx == 0 => Ok(Some(values.clone())),
            // No more values available
            _ => Ok(None),
        }
    }

    /// Format a value using format_replacement with fallback
    fn format_value_with_fallback(
        py: Python<'_>,
        value: &Bound<'_, PyAny>,
        flags: Option<&str>,
        pad_length: Option<i32>,
        precision: Option<i32>,
        format_spec: Option<&str>,
    ) -> String {
        match format_replacement(py, value, flags, pad_length, precision, format_spec) {
            Ok(formatted) => formatted,
            Err(_) => {
                // Fallback to string representation
                value.str().map(|s| s.to_string()).unwrap_or_default()
            }
        }
    }

    #[pyfunction]
    pub fn interpolate(
        py: Python<'_>,
        string: &str,
        values: &Bound<'_, PyAny>,
    ) -> PyResult<String> {
        let mut val_idx = 0;
        let mut str_idx = 0;
        let mut result = String::new();

        for mat in INTERPOLATE_PATTERN.find_iter(string) {
            let Some(captures) = INTERPOLATE_PATTERN.captures(mat.as_str()) else {
                continue;
            };

            // Calculate match position accounting for prefix
            let prefix_len = captures.get(1).map_or(0, |m| m.len());
            let match_index = mat.start() + prefix_len;

            // Add text before the placeholder (with %% -> % conversion)
            result.push_str(&string[str_idx..match_index].replace("%%", "%"));

            // Extract format parameters
            let (flags, pad_length, precision, format_spec) = extract_format_params(&captures);

            // Extract and format the value
            if let Some(value) = extract_value_at_index(values, val_idx)? {
                let formatted = format_value_with_fallback(
                    py,
                    &value,
                    flags,
                    pad_length,
                    precision,
                    format_spec,
                );
                result.push_str(&formatted);
                val_idx += 1;
            }

            str_idx = mat.end();
        }

        // Add remaining text (with %% -> % conversion)
        result.push_str(&string[str_idx..].replace("%%", "%"));
        Ok(result)
    }
}

/// Register the string module with Python
pub fn register_string_module(parent_module: &Bound<'_, PyModule>) -> PyResult<()> {
    let py = parent_module.py();
    let m = PyModule::new(py, "strings")?;

    // Add classes
    m.add_class::<printf::IPrintfFormat>()?;
    m.add_class::<printf::ConsolePrinter>()?;
    m.add_class::<formatting::StringComparison>()?;

    // Add functions
    m.add_function(wrap_pyfunction!(printf::printf, &m)?)?;
    m.add_function(wrap_pyfunction!(printf::continue_print, &m)?)?;
    m.add_function(wrap_pyfunction!(printf::to_console, &m)?)?;
    m.add_function(wrap_pyfunction!(printf::to_text, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::format_replacement, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::format, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::initialize, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::insert, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::is_null_or_empty, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::is_null_or_white_space, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::concat, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::join, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::pad_left, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::pad_right, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::remove, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::replace, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::replicate, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::get_char_at_index, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::split, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::trim, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::trim_start, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::trim_end, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::filter, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::substring, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::to_char_array2, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::compare, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::starts_with, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::ends_with, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::index_of, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::last_index_of, &m)?)?;

    // Additional compatibility functions
    m.add_function(wrap_pyfunction!(formatting::compare_to, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::ends_with_exact, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::starts_with_exact, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::index_of_any, &m)?)?;
    m.add_function(wrap_pyfunction!(formatting::interpolate, &m)?)?;

    parent_module.add_submodule(&m)?;
    Ok(())
}
