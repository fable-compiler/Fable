use crate::floats::{Float32, Float64};
use crate::ints::{Int16, Int32, Int64, Int8, UInt16, UInt32, UInt64, UInt8};
use pyo3::class::basic::CompareOp;
use pyo3::prelude::*;
use pyo3::types::PyAnyMethods;
use pyo3::IntoPyObjectExt;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::sync::{Arc, Mutex};

#[derive(Clone, Debug, PartialEq)]
pub enum ArrayType {
    Int8,
    UInt8,
    Int16,
    UInt16,
    Int32,
    UInt32,
    Int64,
    UInt64,
    Float32,
    Float64,
    Bool,
    Generic,
}

// Implement FromPyObject for ArrayType
impl<'py> FromPyObject<'_, 'py> for ArrayType {
    type Error = PyErr;

    fn extract(ob: Borrowed<'_, 'py, PyAny>) -> Result<Self, Self::Error> {
        let s: &str = ob.extract()?;
        Ok(ArrayType::from_str(s))
    }
}

impl ArrayType {
    /// Parses a string into an `ArrayType` variant.
    ///
    /// Returns `ArrayType::Generic` for any unrecognized type string.
    ///
    /// # Arguments
    /// * `s` - The type name string (e.g., "Int8", "Float64", "Bool")
    pub fn from_str(s: &str) -> Self {
        match s {
            "Int8" => ArrayType::Int8,
            "UInt8" => ArrayType::UInt8,
            "Int16" => ArrayType::Int16,
            "UInt16" => ArrayType::UInt16,
            "Int32" => ArrayType::Int32,
            "UInt32" => ArrayType::UInt32,
            "Int64" => ArrayType::Int64,
            "UInt64" => ArrayType::UInt64,
            "Float32" => ArrayType::Float32,
            "Float64" => ArrayType::Float64,
            "Bool" => ArrayType::Bool,
            _ => ArrayType::Generic,
        }
    }

    /// Returns the string representation of this array type.
    ///
    /// This is the inverse of `from_str`.
    pub fn as_str(&self) -> &'static str {
        match self {
            ArrayType::Int8 => "Int8",
            ArrayType::UInt8 => "UInt8",
            ArrayType::Int16 => "Int16",
            ArrayType::UInt16 => "UInt16",
            ArrayType::Int32 => "Int32",
            ArrayType::UInt32 => "UInt32",
            ArrayType::Int64 => "Int64",
            ArrayType::UInt64 => "UInt64",
            ArrayType::Float32 => "Float32",
            ArrayType::Float64 => "Float64",
            ArrayType::Bool => "Bool",
            ArrayType::Generic => "Generic",
        }
    }

    /// Returns the default value for this array type as a Python object.
    ///
    /// Default values are:
    /// - Numeric types: 0 (or 0.0 for floats)
    /// - Bool: false
    /// - Generic: Python None
    pub fn default_value<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        match self {
            ArrayType::Int8 => Ok(0i8.into_py_any(py)?.into_bound(py)),
            ArrayType::UInt8 => Ok(0u8.into_py_any(py)?.into_bound(py)),
            ArrayType::Int16 => Ok(0i16.into_py_any(py)?.into_bound(py)),
            ArrayType::UInt16 => Ok(0u16.into_py_any(py)?.into_bound(py)),
            ArrayType::Int32 => Ok(0i32.into_py_any(py)?.into_bound(py)),
            ArrayType::UInt32 => Ok(0u32.into_py_any(py)?.into_bound(py)),
            ArrayType::Int64 => Ok(0i64.into_py_any(py)?.into_bound(py)),
            ArrayType::UInt64 => Ok(0u64.into_py_any(py)?.into_bound(py)),
            ArrayType::Float32 => Ok(0.0f32.into_py_any(py)?.into_bound(py)),
            ArrayType::Float64 => Ok(0.0f64.into_py_any(py)?.into_bound(py)),
            ArrayType::Bool => Ok(false.into_py_any(py)?.into_bound(py)),
            ArrayType::Generic => Ok(py.None().into_py_any(py)?.into_bound(py)),
        }
    }
}

// Implement ToPyObject for ArrayType
impl<'py> IntoPyObject<'py> for ArrayType {
    type Target = PyAny;
    type Output = Bound<'py, Self::Target>;
    type Error = std::convert::Infallible;

    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        // Convert directly to a Python string object
        Ok(self.as_str().into_pyobject(py)?.into_any())
    }
}

#[derive(Debug)]
pub enum NativeArray {
    Int8(Vec<i8>),
    UInt8(Vec<u8>),
    Int16(Vec<i16>),
    UInt16(Vec<u16>),
    Int32(Vec<i32>),
    UInt32(Vec<u32>),
    Int64(Vec<i64>),
    UInt64(Vec<u64>),
    Float32(Vec<f32>),
    Float64(Vec<f64>),
    Bool(Vec<bool>),
    PyObject(Arc<Mutex<Vec<Py<PyAny>>>>),
}

impl Clone for NativeArray {
    fn clone(&self) -> Self {
        match self {
            NativeArray::Int8(vec) => NativeArray::Int8(vec.clone()),
            NativeArray::UInt8(vec) => NativeArray::UInt8(vec.clone()),
            NativeArray::Int16(vec) => NativeArray::Int16(vec.clone()),
            NativeArray::UInt16(vec) => NativeArray::UInt16(vec.clone()),
            NativeArray::Int32(vec) => NativeArray::Int32(vec.clone()),
            NativeArray::UInt32(vec) => NativeArray::UInt32(vec.clone()),
            NativeArray::Int64(vec) => NativeArray::Int64(vec.clone()),
            NativeArray::UInt64(vec) => NativeArray::UInt64(vec.clone()),
            NativeArray::Float32(vec) => NativeArray::Float32(vec.clone()),
            NativeArray::Float64(vec) => NativeArray::Float64(vec.clone()),
            NativeArray::Bool(vec) => NativeArray::Bool(vec.clone()),
            NativeArray::PyObject(arc) => {
                // Clone the Arc, which increments the reference count
                NativeArray::PyObject(Arc::clone(arc))
            }
        }
    }
}

// Helper macro for filling storage
macro_rules! fill_typed_vec {
    ($vec:expr, $value:expr, $target_index:expr, $count:expr, $type:ty) => {{
        let typed_value: $type = $value.extract()?;
        for i in 0..$count {
            $vec[$target_index + i] = *typed_value;
        }
    }};
}

// Helper macro for reversing a vector
macro_rules! reverse_vec {
    ($vec:expr) => {{
        let mut new_vec = $vec.clone();
        new_vec.reverse();
        new_vec
    }};
}

// Helper macro for inserting values
macro_rules! insert_typed_value {
    ($vec:expr, $index:expr, $value:expr, $type:ty) => {{
        let typed_value: $type = $value.extract()?;
        $vec.insert($index, *typed_value);
    }};
}

// Helper macro for pushing values
macro_rules! push_typed_value {
    ($vec:expr, $value:expr) => {{
        $vec.push($value.extract()?);
    }};
}

// Helper macro for simple vector operations (like remove, truncate)
macro_rules! simple_vec_operation {
    ($vec:expr, $op:ident, $($args:expr),*) => {{
        $vec.$op($($args),*);
    }};
}

// Helper macro for sort_by with error capture
// Captures any Python errors that occur during comparison and returns them after sorting
macro_rules! sort_with_error_capture {
    ($vec:expr, $comparer:expr, $py:expr, $convert:expr) => {{
        let error: RefCell<Option<PyErr>> = RefCell::new(None);
        $vec.sort_by(|a, b| {
            if error.borrow().is_some() {
                return Ordering::Equal; // Skip comparisons after first error
            }
            let result = (|| -> PyResult<Ordering> {
                let py_a = $convert(*a, $py)?;
                let py_b = $convert(*b, $py)?;
                let cmp_result = $comparer.call1((py_a, py_b))?;
                let cmp_int: i32 = cmp_result.extract()?;
                Ok(cmp_int.cmp(&0))
            })();
            match result {
                Ok(ordering) => ordering,
                Err(e) => {
                    *error.borrow_mut() = Some(e);
                    Ordering::Equal
                }
            }
        });
        // Check if an error was captured during sorting
        if let Some(e) = error.into_inner() {
            return Err(e);
        }
    }};
}

impl NativeArray {
    /// Returns the number of elements in the array.
    #[inline]
    pub fn len(&self) -> usize {
        match self {
            NativeArray::Int8(vec) => vec.len(),
            NativeArray::UInt8(vec) => vec.len(),
            NativeArray::Int16(vec) => vec.len(),
            NativeArray::UInt16(vec) => vec.len(),
            NativeArray::Int32(vec) => vec.len(),
            NativeArray::UInt32(vec) => vec.len(),
            NativeArray::Int64(vec) => vec.len(),
            NativeArray::UInt64(vec) => vec.len(),
            NativeArray::Float32(vec) => vec.len(),
            NativeArray::Float64(vec) => vec.len(),
            NativeArray::Bool(vec) => vec.len(),
            NativeArray::PyObject(vec) => vec.lock().unwrap().len(),
        }
    }

    /// Compares two arrays for element-wise equality.
    ///
    /// Arrays of different types can be compared if one is a PyObject array
    /// and elements can be extracted to the other type. Returns false for
    /// incompatible types or if lengths differ.
    pub fn equals(&self, other: &NativeArray, py: Python<'_>) -> bool {
        // Helper for comparing PyObject with other types
        fn compare_pyobject_with<T: PartialEq + for<'a, 'py> pyo3::FromPyObject<'a, 'py>>(
            py_vec: &std::sync::MutexGuard<Vec<Py<PyAny>>>,
            other_vec: &[T],
            py: Python<'_>,
        ) -> bool {
            if py_vec.len() != other_vec.len() {
                return false;
            }
            py_vec
                .iter()
                .zip(other_vec.iter())
                .all(|(x, y)| x.bind(py).extract::<T>().map(|v| v == *y).unwrap_or(false))
        }
        match (self, other) {
            (NativeArray::Int8(vec), NativeArray::Int8(other_vec)) => vec == other_vec,
            (NativeArray::UInt8(vec), NativeArray::UInt8(other_vec)) => vec == other_vec,
            (NativeArray::Int16(vec), NativeArray::Int16(other_vec)) => vec == other_vec,
            (NativeArray::UInt16(vec), NativeArray::UInt16(other_vec)) => vec == other_vec,
            (NativeArray::Int32(vec), NativeArray::Int32(other_vec)) => vec == other_vec,
            (NativeArray::UInt32(vec), NativeArray::UInt32(other_vec)) => vec == other_vec,
            (NativeArray::Int64(vec), NativeArray::Int64(other_vec)) => vec == other_vec,
            (NativeArray::UInt64(vec), NativeArray::UInt64(other_vec)) => vec == other_vec,
            (NativeArray::Float32(vec), NativeArray::Float32(other_vec)) => vec == other_vec,
            (NativeArray::Float64(vec), NativeArray::Float64(other_vec)) => vec == other_vec,
            (NativeArray::Bool(vec), NativeArray::Bool(other_vec)) => vec == other_vec,
            (NativeArray::PyObject(vec), NativeArray::PyObject(other_vec)) => {
                let xs = vec.lock().unwrap();
                let ys = other_vec.lock().unwrap();
                if xs.len() != ys.len() {
                    return false;
                }
                xs.iter().zip(ys.iter()).all(|(x, y)| {
                    x.bind(py)
                        .rich_compare(y.bind(py), CompareOp::Eq)
                        .and_then(|r| r.is_truthy())
                        .unwrap_or(false)
                })
            }
            (NativeArray::PyObject(vec), other) => {
                let xs = vec.lock().unwrap();
                match other {
                    NativeArray::Int8(vec) => compare_pyobject_with(&xs, vec, py),
                    NativeArray::UInt8(vec) => compare_pyobject_with(&xs, vec, py),
                    NativeArray::Int16(vec) => compare_pyobject_with(&xs, vec, py),
                    NativeArray::UInt16(vec) => compare_pyobject_with(&xs, vec, py),
                    NativeArray::Int32(vec) => compare_pyobject_with(&xs, vec, py),
                    NativeArray::UInt32(vec) => compare_pyobject_with(&xs, vec, py),
                    NativeArray::Int64(vec) => compare_pyobject_with(&xs, vec, py),
                    NativeArray::UInt64(vec) => compare_pyobject_with(&xs, vec, py),
                    NativeArray::Float32(vec) => compare_pyobject_with(&xs, vec, py),
                    NativeArray::Float64(vec) => compare_pyobject_with(&xs, vec, py),
                    NativeArray::Bool(vec) => compare_pyobject_with(&xs, vec, py),
                    _ => false,
                }
            }
            (other, NativeArray::PyObject(vec)) => {
                let ys = vec.lock().unwrap();
                match other {
                    NativeArray::Int8(vec) => compare_pyobject_with(&ys, vec, py),
                    NativeArray::UInt8(vec) => compare_pyobject_with(&ys, vec, py),
                    NativeArray::Int16(vec) => compare_pyobject_with(&ys, vec, py),
                    NativeArray::UInt16(vec) => compare_pyobject_with(&ys, vec, py),
                    NativeArray::Int32(vec) => compare_pyobject_with(&ys, vec, py),
                    NativeArray::UInt32(vec) => compare_pyobject_with(&ys, vec, py),
                    NativeArray::Int64(vec) => compare_pyobject_with(&ys, vec, py),
                    NativeArray::UInt64(vec) => compare_pyobject_with(&ys, vec, py),
                    NativeArray::Float32(vec) => compare_pyobject_with(&ys, vec, py),
                    NativeArray::Float64(vec) => compare_pyobject_with(&ys, vec, py),
                    NativeArray::Bool(vec) => compare_pyobject_with(&ys, vec, py),
                    _ => false,
                }
            }
            _ => false,
        }
    }

    /// Copies elements from this array to a target array.
    ///
    /// # Arguments
    /// * `target` - The destination array (must be same type)
    /// * `source_index` - Starting index in the source array
    /// * `target_index` - Starting index in the target array
    /// * `count` - Number of elements to copy
    /// * `py` - Python interpreter handle
    ///
    /// # Errors
    /// Returns `PyTypeError` if source and target arrays have different types.
    pub fn copy_to(
        &self,
        target: &mut NativeArray,
        source_index: usize,
        target_index: usize,
        count: usize,
        py: Python<'_>,
    ) -> PyResult<()> {
        match (self, target) {
            (NativeArray::Int8(src), NativeArray::Int8(dst)) => {
                dst[target_index..target_index + count]
                    .copy_from_slice(&src[source_index..source_index + count]);
                Ok(())
            }
            (NativeArray::UInt8(src), NativeArray::UInt8(dst)) => {
                dst[target_index..target_index + count]
                    .copy_from_slice(&src[source_index..source_index + count]);
                Ok(())
            }
            (NativeArray::Int16(src), NativeArray::Int16(dst)) => {
                dst[target_index..target_index + count]
                    .copy_from_slice(&src[source_index..source_index + count]);
                Ok(())
            }
            (NativeArray::UInt16(src), NativeArray::UInt16(dst)) => {
                dst[target_index..target_index + count]
                    .copy_from_slice(&src[source_index..source_index + count]);
                Ok(())
            }
            (NativeArray::Int32(src), NativeArray::Int32(dst)) => {
                dst[target_index..target_index + count]
                    .copy_from_slice(&src[source_index..source_index + count]);
                Ok(())
            }
            (NativeArray::UInt32(src), NativeArray::UInt32(dst)) => {
                dst[target_index..target_index + count]
                    .copy_from_slice(&src[source_index..source_index + count]);
                Ok(())
            }
            (NativeArray::Int64(src), NativeArray::Int64(dst)) => {
                dst[target_index..target_index + count]
                    .copy_from_slice(&src[source_index..source_index + count]);
                Ok(())
            }
            (NativeArray::UInt64(src), NativeArray::UInt64(dst)) => {
                dst[target_index..target_index + count]
                    .copy_from_slice(&src[source_index..source_index + count]);
                Ok(())
            }
            (NativeArray::Float32(src), NativeArray::Float32(dst)) => {
                dst[target_index..target_index + count]
                    .copy_from_slice(&src[source_index..source_index + count]);
                Ok(())
            }
            (NativeArray::Float64(src), NativeArray::Float64(dst)) => {
                dst[target_index..target_index + count]
                    .copy_from_slice(&src[source_index..source_index + count]);
                Ok(())
            }
            (NativeArray::Bool(src), NativeArray::Bool(dst)) => {
                dst[target_index..(count + target_index)]
                    .clone_from_slice(&src[source_index..(count + source_index)]);
                Ok(())
            }
            (NativeArray::PyObject(src), NativeArray::PyObject(dst)) => {
                let src = src.lock().unwrap();
                let mut dst = dst.lock().unwrap();
                for i in 0..count {
                    dst[target_index + i] = src[source_index + i].clone_ref(py);
                }
                Ok(())
            }
            (src, dst) => Err(pyo3::exceptions::PyTypeError::new_err(format!(
                "Cannot copy between different array types: {} -> {}",
                src.type_name(),
                dst.type_name()
            ))),
        }
    }

    /// Creates a new empty array of the specified type with optional initial capacity.
    ///
    /// # Arguments
    /// * `array_type` - The element type for the new array
    /// * `capacity` - Optional pre-allocated capacity (defaults to 0)
    pub fn new(array_type: &ArrayType, capacity: Option<usize>) -> Self {
        match array_type {
            ArrayType::Int8 => NativeArray::Int8(Vec::with_capacity(capacity.unwrap_or(0))),
            ArrayType::UInt8 => NativeArray::UInt8(Vec::with_capacity(capacity.unwrap_or(0))),
            ArrayType::Int16 => NativeArray::Int16(Vec::with_capacity(capacity.unwrap_or(0))),
            ArrayType::UInt16 => NativeArray::UInt16(Vec::with_capacity(capacity.unwrap_or(0))),
            ArrayType::Int32 => NativeArray::Int32(Vec::with_capacity(capacity.unwrap_or(0))),
            ArrayType::UInt32 => NativeArray::UInt32(Vec::with_capacity(capacity.unwrap_or(0))),
            ArrayType::Int64 => NativeArray::Int64(Vec::with_capacity(capacity.unwrap_or(0))),
            ArrayType::UInt64 => NativeArray::UInt64(Vec::with_capacity(capacity.unwrap_or(0))),
            ArrayType::Float32 => NativeArray::Float32(Vec::with_capacity(capacity.unwrap_or(0))),
            ArrayType::Float64 => NativeArray::Float64(Vec::with_capacity(capacity.unwrap_or(0))),
            ArrayType::Bool => NativeArray::Bool(Vec::with_capacity(capacity.unwrap_or(0))),
            ArrayType::Generic => NativeArray::PyObject(Arc::new(Mutex::new(Vec::with_capacity(
                capacity.unwrap_or(0),
            )))),
        }
    }

    /// Returns the type name of this array as a string slice.
    #[inline]
    pub fn type_name(&self) -> &str {
        match self {
            NativeArray::Int8(_) => "Int8",
            NativeArray::UInt8(_) => "UInt8",
            NativeArray::Int16(_) => "Int16",
            NativeArray::UInt16(_) => "UInt16",
            NativeArray::Int32(_) => "Int32",
            NativeArray::UInt32(_) => "UInt32",
            NativeArray::Int64(_) => "Int64",
            NativeArray::UInt64(_) => "UInt64",
            NativeArray::Float32(_) => "Float32",
            NativeArray::Float64(_) => "Float64",
            NativeArray::Bool(_) => "Bool",
            NativeArray::PyObject(_) => "Generic",
        }
    }

    /// Returns a reference to the `ArrayType` enum variant for this array.
    #[inline]
    pub fn get_type(&self) -> &ArrayType {
        match self {
            NativeArray::Int8(_) => &ArrayType::Int8,
            NativeArray::UInt8(_) => &ArrayType::UInt8,
            NativeArray::Int16(_) => &ArrayType::Int16,
            NativeArray::UInt16(_) => &ArrayType::UInt16,
            NativeArray::Int32(_) => &ArrayType::Int32,
            NativeArray::UInt32(_) => &ArrayType::UInt32,
            NativeArray::Int64(_) => &ArrayType::Int64,
            NativeArray::UInt64(_) => &ArrayType::UInt64,
            NativeArray::Float32(_) => &ArrayType::Float32,
            NativeArray::Float64(_) => &ArrayType::Float64,
            NativeArray::Bool(_) => &ArrayType::Bool,
            NativeArray::PyObject(_) => &ArrayType::Generic,
        }
    }

    /// Creates a new empty array of the specified type with zero capacity.
    ///
    /// This is equivalent to `NativeArray::new(array_type, None)`.
    pub fn create_empty_storage(array_type: &ArrayType) -> NativeArray {
        match array_type {
            ArrayType::Int8 => NativeArray::Int8(Vec::new()),
            ArrayType::UInt8 => NativeArray::UInt8(Vec::new()),
            ArrayType::Int16 => NativeArray::Int16(Vec::new()),
            ArrayType::UInt16 => NativeArray::UInt16(Vec::new()),
            ArrayType::Int32 => NativeArray::Int32(Vec::new()),
            ArrayType::UInt32 => NativeArray::UInt32(Vec::new()),
            ArrayType::Int64 => NativeArray::Int64(Vec::new()),
            ArrayType::UInt64 => NativeArray::UInt64(Vec::new()),
            ArrayType::Float32 => NativeArray::Float32(Vec::new()),
            ArrayType::Float64 => NativeArray::Float64(Vec::new()),
            ArrayType::Bool => NativeArray::Bool(Vec::new()),
            ArrayType::Generic => NativeArray::PyObject(Arc::new(Mutex::new(Vec::new()))),
        }
    }

    /// Pushes an element from another array at the given index onto this array.
    ///
    /// # Arguments
    /// * `source_storage` - The source array to copy from (must be same type)
    /// * `index` - Index of the element in the source array
    /// * `py` - Python interpreter handle
    ///
    /// # Panics
    /// Panics if the source and destination arrays have different types.
    pub fn push_from_storage(
        &mut self,
        source_storage: &NativeArray,
        index: usize,
        py: Python<'_>,
    ) {
        match (self, source_storage) {
            (NativeArray::Int8(dst), NativeArray::Int8(src)) => dst.push(src[index]),
            (NativeArray::UInt8(dst), NativeArray::UInt8(src)) => dst.push(src[index]),
            (NativeArray::Int16(dst), NativeArray::Int16(src)) => dst.push(src[index]),
            (NativeArray::UInt16(dst), NativeArray::UInt16(src)) => dst.push(src[index]),
            (NativeArray::Int32(dst), NativeArray::Int32(src)) => dst.push(src[index]),
            (NativeArray::UInt32(dst), NativeArray::UInt32(src)) => dst.push(src[index]),
            (NativeArray::Int64(dst), NativeArray::Int64(src)) => dst.push(src[index]),
            (NativeArray::UInt64(dst), NativeArray::UInt64(src)) => dst.push(src[index]),
            (NativeArray::Float32(dst), NativeArray::Float32(src)) => dst.push(src[index]),
            (NativeArray::Float64(dst), NativeArray::Float64(src)) => dst.push(src[index]),
            (NativeArray::Bool(dst), NativeArray::Bool(src)) => dst.push(src[index]),
            (NativeArray::PyObject(dst), NativeArray::PyObject(src)) => {
                let src_guard = src.lock().unwrap();
                let mut dst_guard = dst.lock().unwrap();
                dst_guard.push(src_guard[index].clone_ref(py));
            }
            _ => panic!("Cannot push between different array types"),
        }
    }

    /// Pushes a Python value onto the end of this array.
    ///
    /// The value is extracted/converted to the array's element type.
    ///
    /// # Errors
    /// Returns an error if the value cannot be converted to the array's element type.
    pub fn push_value(&mut self, value: &Bound<'_, PyAny>, _py: Python<'_>) -> PyResult<()> {
        match self {
            NativeArray::Int8(vec) => push_typed_value!(vec, value),
            NativeArray::UInt8(vec) => push_typed_value!(vec, value),
            NativeArray::Int16(vec) => push_typed_value!(vec, value),
            NativeArray::UInt16(vec) => push_typed_value!(vec, value),
            NativeArray::Int32(vec) => push_typed_value!(vec, value),
            NativeArray::UInt32(vec) => push_typed_value!(vec, value),
            NativeArray::Int64(vec) => push_typed_value!(vec, value),
            NativeArray::UInt64(vec) => push_typed_value!(vec, value),
            NativeArray::Float32(vec) => push_typed_value!(vec, value),
            NativeArray::Float64(vec) => push_typed_value!(vec, value),
            NativeArray::Bool(vec) => push_typed_value!(vec, value),
            NativeArray::PyObject(vec) => {
                let mut guard = vec.lock().unwrap();
                guard.push(value.clone().unbind());
            }
        }
        Ok(())
    }

    /// Fills a range of the array with a single value.
    ///
    /// # Arguments
    /// * `storage` - The array to fill
    /// * `target_index` - Starting index for the fill operation
    /// * `count` - Number of elements to fill
    /// * `value` - The value to fill with (converted to the array's element type)
    ///
    /// # Errors
    /// Returns an error if the value cannot be converted to the array's element type.
    pub fn fill_storage(
        storage: &mut NativeArray,
        target_index: usize,
        count: usize,
        value: &Bound<'_, PyAny>,
    ) -> PyResult<()> {
        match storage {
            NativeArray::Int8(vec) => fill_typed_vec!(vec, value, target_index, count, Int8),
            NativeArray::UInt8(vec) => fill_typed_vec!(vec, value, target_index, count, UInt8),
            NativeArray::Int16(vec) => fill_typed_vec!(vec, value, target_index, count, Int16),
            NativeArray::UInt16(vec) => fill_typed_vec!(vec, value, target_index, count, UInt16),
            NativeArray::Int32(vec) => fill_typed_vec!(vec, value, target_index, count, Int32),
            NativeArray::UInt32(vec) => fill_typed_vec!(vec, value, target_index, count, UInt32),
            NativeArray::Int64(vec) => fill_typed_vec!(vec, value, target_index, count, Int64),
            NativeArray::UInt64(vec) => fill_typed_vec!(vec, value, target_index, count, UInt64),
            NativeArray::Float32(vec) => fill_typed_vec!(vec, value, target_index, count, Float32),
            NativeArray::Float64(vec) => fill_typed_vec!(vec, value, target_index, count, Float64),
            NativeArray::Bool(vec) => {
                let value: bool = value.extract()?;
                for i in 0..count {
                    vec[target_index + i] = value;
                }
            }
            NativeArray::PyObject(vec) => {
                let mut guard = vec.lock().unwrap();
                let py_value = value.into_py_any(value.py())?;
                for i in 0..count {
                    guard[target_index + i] = py_value.clone_ref(value.py());
                }
            }
        }
        Ok(())
    }

    /// Creates a new array with elements in reversed order.
    ///
    /// Does not modify the original array.
    pub fn reverse_storage(storage: &NativeArray, py: Python<'_>) -> NativeArray {
        match storage {
            NativeArray::Int8(vec) => NativeArray::Int8(reverse_vec!(vec)),
            NativeArray::UInt8(vec) => NativeArray::UInt8(reverse_vec!(vec)),
            NativeArray::Int16(vec) => NativeArray::Int16(reverse_vec!(vec)),
            NativeArray::UInt16(vec) => NativeArray::UInt16(reverse_vec!(vec)),
            NativeArray::Int32(vec) => NativeArray::Int32(reverse_vec!(vec)),
            NativeArray::UInt32(vec) => NativeArray::UInt32(reverse_vec!(vec)),
            NativeArray::Int64(vec) => NativeArray::Int64(reverse_vec!(vec)),
            NativeArray::UInt64(vec) => NativeArray::UInt64(reverse_vec!(vec)),
            NativeArray::Float32(vec) => NativeArray::Float32(reverse_vec!(vec)),
            NativeArray::Float64(vec) => NativeArray::Float64(reverse_vec!(vec)),
            NativeArray::Bool(vec) => NativeArray::Bool(reverse_vec!(vec)),
            NativeArray::PyObject(arc) => {
                let mut new_vec = Vec::with_capacity(arc.lock().unwrap().len());
                {
                    let guard = arc.lock().unwrap();
                    for i in (0..guard.len()).rev() {
                        new_vec.push(guard[i].clone_ref(py));
                    }
                }
                NativeArray::PyObject(Arc::new(Mutex::new(new_vec)))
            }
        }
    }

    /// Removes the element at the specified index, shifting subsequent elements left.
    ///
    /// # Panics
    /// Panics if the index is out of bounds.
    pub fn remove_at_index(&mut self, index: usize) {
        match self {
            NativeArray::Int8(vec) => simple_vec_operation!(vec, remove, index),
            NativeArray::UInt8(vec) => simple_vec_operation!(vec, remove, index),
            NativeArray::Int16(vec) => simple_vec_operation!(vec, remove, index),
            NativeArray::UInt16(vec) => simple_vec_operation!(vec, remove, index),
            NativeArray::Int32(vec) => simple_vec_operation!(vec, remove, index),
            NativeArray::UInt32(vec) => simple_vec_operation!(vec, remove, index),
            NativeArray::Int64(vec) => simple_vec_operation!(vec, remove, index),
            NativeArray::UInt64(vec) => simple_vec_operation!(vec, remove, index),
            NativeArray::Float32(vec) => simple_vec_operation!(vec, remove, index),
            NativeArray::Float64(vec) => simple_vec_operation!(vec, remove, index),
            NativeArray::Bool(vec) => simple_vec_operation!(vec, remove, index),
            NativeArray::PyObject(arc_vec) => {
                arc_vec.lock().unwrap().remove(index);
            }
        }
    }

    /// Inserts an element at the specified index, shifting subsequent elements right.
    ///
    /// # Arguments
    /// * `index` - Position to insert at (0 <= index <= len)
    /// * `value` - The value to insert (converted to the array's element type)
    /// * `_py` - Python interpreter handle
    ///
    /// # Errors
    /// Returns an error if the value cannot be converted to the array's element type.
    ///
    /// # Panics
    /// Panics if the index is out of bounds (index > len).
    pub fn insert(
        &mut self,
        index: usize,
        value: &Bound<'_, PyAny>,
        _py: Python<'_>,
    ) -> PyResult<()> {
        match self {
            NativeArray::Int8(vec) => insert_typed_value!(vec, index, value, Int8),
            NativeArray::UInt8(vec) => insert_typed_value!(vec, index, value, UInt8),
            NativeArray::Int16(vec) => insert_typed_value!(vec, index, value, Int16),
            NativeArray::UInt16(vec) => insert_typed_value!(vec, index, value, UInt16),
            NativeArray::Int32(vec) => insert_typed_value!(vec, index, value, Int32),
            NativeArray::UInt32(vec) => insert_typed_value!(vec, index, value, UInt32),
            NativeArray::Int64(vec) => insert_typed_value!(vec, index, value, Int64),
            NativeArray::UInt64(vec) => insert_typed_value!(vec, index, value, UInt64),
            NativeArray::Float32(vec) => insert_typed_value!(vec, index, value, Float32),
            NativeArray::Float64(vec) => insert_typed_value!(vec, index, value, Float64),
            NativeArray::Bool(vec) => {
                let bool_value: bool = value.extract()?;
                vec.insert(index, bool_value);
            }
            NativeArray::PyObject(arc_mutex_vec) => {
                let mut vec = arc_mutex_vec.lock().unwrap();
                vec.insert(index, value.clone().into());
            }
        }
        Ok(())
    }

    /// Truncates the array to the specified size, removing elements beyond `new_size`.
    ///
    /// If `new_size` is greater than or equal to the current length, this has no effect.
    pub fn truncate(&mut self, new_size: usize) {
        match self {
            NativeArray::Int8(vec) => simple_vec_operation!(vec, truncate, new_size),
            NativeArray::UInt8(vec) => simple_vec_operation!(vec, truncate, new_size),
            NativeArray::Int16(vec) => simple_vec_operation!(vec, truncate, new_size),
            NativeArray::UInt16(vec) => simple_vec_operation!(vec, truncate, new_size),
            NativeArray::Int32(vec) => simple_vec_operation!(vec, truncate, new_size),
            NativeArray::UInt32(vec) => simple_vec_operation!(vec, truncate, new_size),
            NativeArray::Int64(vec) => simple_vec_operation!(vec, truncate, new_size),
            NativeArray::UInt64(vec) => simple_vec_operation!(vec, truncate, new_size),
            NativeArray::Float32(vec) => simple_vec_operation!(vec, truncate, new_size),
            NativeArray::Float64(vec) => simple_vec_operation!(vec, truncate, new_size),
            NativeArray::Bool(vec) => simple_vec_operation!(vec, truncate, new_size),
            NativeArray::PyObject(vec) => {
                if let Ok(mut vec) = vec.lock() {
                    vec.truncate(new_size);
                }
            }
        }
    }

    /// Sorts the array in place using a custom comparison function.
    ///
    /// Implements F#'s `Array.sortInPlaceWith`.
    ///
    /// # Arguments
    /// * `comparer` - A Python callable taking two elements and returning an int:
    ///   - Negative if first < second
    ///   - Zero if first == second
    ///   - Positive if first > second
    ///
    /// # Errors
    /// Returns any Python error raised by the comparer function.
    pub fn sort_in_place_with(&mut self, comparer: &Bound<'_, PyAny>) -> PyResult<()> {
        let py = comparer.py();
        match self {
            NativeArray::Int8(vec) => {
                sort_with_error_capture!(vec, comparer, py, |v: i8, py| Int8(v)
                    .into_pyobject(py)
                    .map(|o| o.into_any()));
                Ok(())
            }
            NativeArray::UInt8(vec) => {
                sort_with_error_capture!(vec, comparer, py, |v: u8, py| UInt8(v)
                    .into_pyobject(py)
                    .map(|o| o.into_any()));
                Ok(())
            }
            NativeArray::Int16(vec) => {
                sort_with_error_capture!(vec, comparer, py, |v: i16, py| Int16(v)
                    .into_pyobject(py)
                    .map(|o| o.into_any()));
                Ok(())
            }
            NativeArray::UInt16(vec) => {
                sort_with_error_capture!(vec, comparer, py, |v: u16, py| UInt16(v)
                    .into_pyobject(py)
                    .map(|o| o.into_any()));
                Ok(())
            }
            NativeArray::Int32(vec) => {
                sort_with_error_capture!(vec, comparer, py, |v: i32, py| Int32(v)
                    .into_pyobject(py)
                    .map(|o| o.into_any()));
                Ok(())
            }
            NativeArray::UInt32(vec) => {
                sort_with_error_capture!(vec, comparer, py, |v: u32, py| UInt32(v)
                    .into_pyobject(py)
                    .map(|o| o.into_any()));
                Ok(())
            }
            NativeArray::Int64(vec) => {
                sort_with_error_capture!(vec, comparer, py, |v: i64, py| Int64(v)
                    .into_pyobject(py)
                    .map(|o| o.into_any()));
                Ok(())
            }
            NativeArray::UInt64(vec) => {
                sort_with_error_capture!(vec, comparer, py, |v: u64, py| UInt64(v)
                    .into_pyobject(py)
                    .map(|o| o.into_any()));
                Ok(())
            }
            NativeArray::Float32(vec) => {
                sort_with_error_capture!(vec, comparer, py, |v: f32, py| Float32(v)
                    .into_pyobject(py)
                    .map(|o| o.into_any()));
                Ok(())
            }
            NativeArray::Float64(vec) => {
                sort_with_error_capture!(vec, comparer, py, |v: f64, py| Float64(v)
                    .into_pyobject(py)
                    .map(|o| o.into_any()));
                Ok(())
            }
            NativeArray::Bool(vec) => {
                let error: RefCell<Option<PyErr>> = RefCell::new(None);
                vec.sort_by(|a, b| {
                    if error.borrow().is_some() {
                        return Ordering::Equal;
                    }
                    let result = (|| -> PyResult<Ordering> {
                        let py_a = a.into_pyobject(py)?;
                        let py_b = b.into_pyobject(py)?;
                        let cmp_result = comparer.call1((py_a, py_b))?;
                        let cmp_int: i32 = cmp_result.extract()?;
                        Ok(cmp_int.cmp(&0))
                    })();
                    match result {
                        Ok(ordering) => ordering,
                        Err(e) => {
                            *error.borrow_mut() = Some(e);
                            Ordering::Equal
                        }
                    }
                });
                if let Some(e) = error.into_inner() {
                    return Err(e);
                }
                Ok(())
            }
            NativeArray::PyObject(vec) => {
                let error: RefCell<Option<PyErr>> = RefCell::new(None);
                let mut guard = vec.lock().unwrap();
                guard.sort_by(|a, b| {
                    if error.borrow().is_some() {
                        return Ordering::Equal;
                    }
                    let result = (|| -> PyResult<Ordering> {
                        let cmp_result = comparer.call1((a.bind(py), b.bind(py)))?;
                        let cmp_int: i32 = cmp_result.extract()?;
                        Ok(cmp_int.cmp(&0))
                    })();
                    match result {
                        Ok(ordering) => ordering,
                        Err(e) => {
                            *error.borrow_mut() = Some(e);
                            Ordering::Equal
                        }
                    }
                });
                if let Some(e) = error.into_inner() {
                    return Err(e);
                }
                Ok(())
            }
        }
    }

    /// Returns a new sorted array using a custom comparison function.
    ///
    /// Does not modify the original array.
    ///
    /// # Arguments
    /// * `comparer` - A Python callable taking two elements and returning an int
    ///
    /// # Errors
    /// Returns any Python error raised by the comparer function.
    pub fn sort_with(&mut self, comparer: &Bound<'_, PyAny>) -> PyResult<NativeArray> {
        let mut result = self.clone();
        result.sort_in_place_with(comparer)?;
        Ok(result)
    }

    /// Returns a new sorted array using a projection function and IComparer.
    ///
    /// Implements F#'s `Array.sortBy` with a custom comparer. Elements are sorted
    /// by comparing projected values using the comparer's `Compare` method.
    ///
    /// # Arguments
    /// * `py` - Python interpreter handle
    /// * `projection` - A Python callable that transforms each element to a comparable key
    /// * `comparer` - An IComparer object with a `Compare(a, b)` method returning an int
    ///
    /// # Errors
    /// Returns any Python error raised by the projection or comparer.
    pub fn sort_by_with_projection(
        &self,
        py: Python<'_>,
        projection: &Bound<'_, PyAny>,
        comparer: &Bound<'_, PyAny>,
    ) -> PyResult<NativeArray> {
        // Helper function that computes ordering with projection, returning Result
        fn get_result<'a, T: Clone + IntoPyObject<'a>>(
            a: &T,
            b: &T,
            py: Python<'a>,
            projection: &Bound<'_, PyAny>,
            comparer: &Bound<'_, PyAny>,
        ) -> PyResult<Ordering> {
            let py_a: Py<PyAny> = a.clone().into_py_any(py)?;
            let py_b: Py<PyAny> = b.clone().into_py_any(py)?;
            let proj_a = projection.call1((py_a,))?;
            let proj_b = projection.call1((py_b,))?;
            // Call the Compare method on the IComparer object
            let result = comparer.call_method1("Compare", (proj_a, proj_b))?;
            Ok(result.extract::<i32>()?.cmp(&0))
        }

        // Macro to handle sorting with error capture for each type
        macro_rules! sort_projection_with_error {
            ($vec:expr, $wrapper:ident) => {{
                let error: RefCell<Option<PyErr>> = RefCell::new(None);
                let mut new_vec = $vec.clone();
                new_vec.sort_by(|a, b| {
                    if error.borrow().is_some() {
                        return Ordering::Equal;
                    }
                    match get_result(a, b, py, projection, comparer) {
                        Ok(ord) => ord,
                        Err(e) => {
                            *error.borrow_mut() = Some(e);
                            Ordering::Equal
                        }
                    }
                });
                if let Some(e) = error.into_inner() {
                    return Err(e);
                }
                NativeArray::$wrapper(new_vec)
            }};
        }

        let result = match self {
            NativeArray::Int8(vec) => sort_projection_with_error!(vec, Int8),
            NativeArray::UInt8(vec) => sort_projection_with_error!(vec, UInt8),
            NativeArray::Int16(vec) => sort_projection_with_error!(vec, Int16),
            NativeArray::UInt16(vec) => sort_projection_with_error!(vec, UInt16),
            NativeArray::Int32(vec) => sort_projection_with_error!(vec, Int32),
            NativeArray::UInt32(vec) => sort_projection_with_error!(vec, UInt32),
            NativeArray::Int64(vec) => sort_projection_with_error!(vec, Int64),
            NativeArray::UInt64(vec) => sort_projection_with_error!(vec, UInt64),
            NativeArray::Float32(vec) => sort_projection_with_error!(vec, Float32),
            NativeArray::Float64(vec) => sort_projection_with_error!(vec, Float64),
            NativeArray::Bool(vec) => sort_projection_with_error!(vec, Bool),
            NativeArray::PyObject(vec) => {
                let error: RefCell<Option<PyErr>> = RefCell::new(None);
                let mut new_vec = vec
                    .lock()
                    .unwrap()
                    .iter()
                    .map(|x| x.clone_ref(py))
                    .collect::<Vec<_>>();
                new_vec.sort_by(|a, b| {
                    if error.borrow().is_some() {
                        return Ordering::Equal;
                    }
                    let result = (|| -> PyResult<Ordering> {
                        let py_a = a.bind(py);
                        let py_b = b.bind(py);
                        let proj_a = projection.call1((py_a,))?;
                        let proj_b = projection.call1((py_b,))?;
                        let cmp_result = comparer.call_method1("Compare", (proj_a, proj_b))?;
                        Ok(cmp_result.extract::<i32>()?.cmp(&0))
                    })();
                    match result {
                        Ok(ord) => ord,
                        Err(e) => {
                            *error.borrow_mut() = Some(e);
                            Ordering::Equal
                        }
                    }
                });
                if let Some(e) = error.into_inner() {
                    return Err(e);
                }
                NativeArray::PyObject(Arc::new(Mutex::new(new_vec)))
            }
        };
        Ok(result)
    }

    /// Returns the element at the specified index as a Python object.
    ///
    /// # Panics
    /// Panics if the index is out of bounds.
    #[inline]
    pub fn get(&self, py: Python<'_>, index: usize) -> PyResult<Py<PyAny>> {
        match self {
            NativeArray::Int8(vec) => Ok(vec[index].into_py_any(py)?),
            NativeArray::UInt8(vec) => Ok(vec[index].into_py_any(py)?),
            NativeArray::Int16(vec) => Ok(vec[index].into_py_any(py)?),
            NativeArray::UInt16(vec) => Ok(vec[index].into_py_any(py)?),
            NativeArray::Int32(vec) => Ok(vec[index].into_py_any(py)?),
            NativeArray::UInt32(vec) => Ok(vec[index].into_py_any(py)?),
            NativeArray::Int64(vec) => Ok(vec[index].into_py_any(py)?),
            NativeArray::UInt64(vec) => Ok(vec[index].into_py_any(py)?),
            NativeArray::Float32(vec) => Ok(vec[index].into_py_any(py)?),
            NativeArray::Float64(vec) => Ok(vec[index].into_py_any(py)?),
            NativeArray::Bool(vec) => Ok(vec[index].into_py_any(py)?),
            NativeArray::PyObject(vec) => {
                let guard = vec.lock().unwrap();
                Ok(guard[index].clone_ref(py))
            }
        }
    }
}
