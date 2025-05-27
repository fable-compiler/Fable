use crate::floats::{Float32, Float64};
use crate::ints::{Int16, Int32, Int64, Int8, UInt16, UInt32, UInt64, UInt8};
use pyo3::class::basic::CompareOp;
use pyo3::prelude::*;
use pyo3::types::PyAnyMethods;
use pyo3::IntoPyObjectExt;
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
    String,
    Generic,
}

// Implement FromPyObject for ArrayType
impl<'source> FromPyObject<'source> for ArrayType {
    fn extract_bound(ob: &Bound<'source, PyAny>) -> PyResult<Self> {
        let s: &str = ob.extract()?;
        match s {
            "Int8" => Ok(ArrayType::Int8),
            "UInt8" => Ok(ArrayType::UInt8),
            "Int16" => Ok(ArrayType::Int16),
            "UInt16" => Ok(ArrayType::UInt16),
            "Int32" => Ok(ArrayType::Int32),
            "UInt32" => Ok(ArrayType::UInt32),
            "Int64" => Ok(ArrayType::Int64),
            "UInt64" => Ok(ArrayType::UInt64),
            "Float32" => Ok(ArrayType::Float32),
            "Float64" => Ok(ArrayType::Float64),
            "String" => Ok(ArrayType::String),
            _ => Ok(ArrayType::Generic),
        }
    }
}

// Implement ToPyObject for ArrayType
impl<'py> IntoPyObject<'py> for ArrayType {
    type Target = PyAny;
    type Output = Bound<'py, Self::Target>;
    type Error = std::convert::Infallible;

    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        let s = match self {
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
            ArrayType::String => "String",
            ArrayType::Generic => "Generic",
        };
        // Convert directly to a Python string object
        Ok(s.into_pyobject(py)?.into_any())
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
    String(Vec<String>),
    PyObject(Arc<Mutex<Vec<PyObject>>>),
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
            NativeArray::String(vec) => NativeArray::String(vec.clone()),
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

impl NativeArray {
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
            NativeArray::String(vec) => vec.len(),
            NativeArray::PyObject(vec) => vec.lock().unwrap().len(),
        }
    }

    pub fn equals(&self, other: &NativeArray, py: Python<'_>) -> bool {
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
            (NativeArray::String(vec), NativeArray::String(other_vec)) => vec == other_vec,
            (NativeArray::PyObject(vec), NativeArray::PyObject(other_vec)) => {
                let xs = vec.lock().unwrap();
                let ys = other_vec.lock().unwrap();
                if xs.len() != ys.len() {
                    return false;
                }
                // Compare each element using Python's rich comparison
                xs.iter().zip(ys.iter()).all(|(x, y)| {
                    x.bind(py)
                        .rich_compare(&y.bind(py), CompareOp::Eq)
                        .and_then(|r| r.is_truthy())
                        .unwrap_or(false)
                })
            }
            _ => false,
        }
    }

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
            (NativeArray::String(src), NativeArray::String(dst)) => {
                for i in 0..count {
                    dst[target_index + i] = src[source_index + i].clone();
                }
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
            ArrayType::String => NativeArray::String(Vec::with_capacity(capacity.unwrap_or(0))),
            ArrayType::Generic => NativeArray::PyObject(Arc::new(Mutex::new(Vec::with_capacity(
                capacity.unwrap_or(0),
            )))),
        }
    }

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
            NativeArray::String(_) => "String",
            NativeArray::PyObject(_) => "Generic",
        }
    }

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
            NativeArray::String(_) => &ArrayType::String,
            NativeArray::PyObject(_) => &ArrayType::Generic,
        }
    }

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
            ArrayType::String => NativeArray::String(Vec::new()),
            ArrayType::Generic => NativeArray::PyObject(Arc::new(Mutex::new(Vec::new()))),
        }
    }

    pub fn create_from_storage(storage: &NativeArray, py: Python<'_>) -> NativeArray {
        match storage {
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
            NativeArray::String(vec) => NativeArray::String(vec.clone()),
            NativeArray::PyObject(vec) => {
                let new_vec = vec
                    .lock()
                    .unwrap()
                    .iter()
                    .map(|item| item.clone_ref(py))
                    .collect();
                NativeArray::PyObject(Arc::new(Mutex::new(new_vec)))
            }
        }
    }

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
            (NativeArray::String(dst), NativeArray::String(src)) => dst.push(src[index].clone()),
            (NativeArray::PyObject(dst), NativeArray::PyObject(src)) => {
                let src_guard = src.lock().unwrap();
                let mut dst_guard = dst.lock().unwrap();
                dst_guard.push(src_guard[index].clone_ref(py));
            }
            _ => panic!("Cannot push between different array types"),
        }
    }

    pub fn push_value(&mut self, value: &Bound<'_, PyAny>, _py: Python<'_>) -> PyResult<()> {
        match self {
            NativeArray::Int8(vec) => vec.push(value.extract()?),
            NativeArray::UInt8(vec) => vec.push(value.extract()?),
            NativeArray::Int16(vec) => vec.push(value.extract()?),
            NativeArray::UInt16(vec) => vec.push(value.extract()?),
            NativeArray::Int32(vec) => vec.push(value.extract()?),
            NativeArray::UInt32(vec) => vec.push(value.extract()?),
            NativeArray::Int64(vec) => vec.push(value.extract()?),
            NativeArray::UInt64(vec) => vec.push(value.extract()?),
            NativeArray::Float32(vec) => vec.push(value.extract()?),
            NativeArray::Float64(vec) => vec.push(value.extract()?),
            NativeArray::String(vec) => vec.push(value.extract()?),
            NativeArray::PyObject(vec) => {
                let mut guard = vec.lock().unwrap();
                guard.push(value.clone().unbind());
            }
        }
        Ok(())
    }

    pub fn sort_storage_with(
        storage: &mut NativeArray,
        compare_func: &Bound<'_, PyAny>,
    ) -> PyResult<()> {
        match storage {
            NativeArray::PyObject(vec) => {
                let mut guard = vec.lock().unwrap();
                guard.sort_by(|a, b| {
                    let result = compare_func
                        .call1((a.bind(compare_func.py()), b.bind(compare_func.py())))
                        .unwrap();
                    result.extract::<i32>().unwrap().cmp(&0)
                });
            }
            _ => {
                return Err(pyo3::exceptions::PyTypeError::new_err(
                    "Cannot sort non-generic array with custom comparison function",
                ));
            }
        }
        Ok(())
    }

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
            NativeArray::String(vec) => {
                let value: String = value.extract()?;
                for i in 0..count {
                    vec[target_index + i] = value.clone();
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
            NativeArray::String(vec) => NativeArray::String(reverse_vec!(vec)),
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

    pub fn remove_at_index(&mut self, index: usize) {
        match self {
            NativeArray::Int8(vec) => {
                vec.remove(index);
            }
            NativeArray::UInt8(vec) => {
                vec.remove(index);
            }
            NativeArray::Int16(vec) => {
                vec.remove(index);
            }
            NativeArray::UInt16(vec) => {
                vec.remove(index);
            }
            NativeArray::Int32(vec) => {
                vec.remove(index);
            }
            NativeArray::UInt32(vec) => {
                vec.remove(index);
            }
            NativeArray::Int64(vec) => {
                vec.remove(index);
            }
            NativeArray::UInt64(vec) => {
                vec.remove(index);
            }
            NativeArray::Float32(vec) => {
                vec.remove(index);
            }
            NativeArray::Float64(vec) => {
                vec.remove(index);
            }
            NativeArray::String(vec) => {
                vec.remove(index);
            }
            NativeArray::PyObject(arc_vec) => {
                arc_vec.lock().unwrap().remove(index);
            }
        }
    }

    pub fn insert(
        &mut self,
        index: usize,
        value: &Bound<'_, PyAny>,
        _py: Python<'_>,
    ) -> PyResult<()> {
        match self {
            NativeArray::Int8(vec) => {
                let int8: Int8 = value.extract()?;
                vec.insert(index, *int8);
            }
            NativeArray::UInt8(vec) => {
                let uint8: UInt8 = value.extract()?;
                vec.insert(index, *uint8);
            }
            NativeArray::Int16(vec) => {
                let int16: Int16 = value.extract()?;
                vec.insert(index, *int16);
            }
            NativeArray::UInt16(vec) => {
                let uint16: UInt16 = value.extract()?;
                vec.insert(index, *uint16);
            }
            NativeArray::Int32(vec) => {
                let int32: Int32 = value.extract()?;
                vec.insert(index, *int32);
            }
            NativeArray::UInt32(vec) => {
                let uint32: UInt32 = value.extract()?;
                vec.insert(index, *uint32);
            }
            NativeArray::Int64(vec) => {
                let int64: Int64 = value.extract()?;
                vec.insert(index, *int64);
            }
            NativeArray::UInt64(vec) => {
                let uint64: UInt64 = value.extract()?;
                vec.insert(index, *uint64);
            }
            NativeArray::Float32(vec) => {
                let float32: Float32 = value.extract()?;
                vec.insert(index, *float32);
            }
            NativeArray::Float64(vec) => {
                let float64: Float64 = value.extract()?;
                vec.insert(index, *float64);
            }
            NativeArray::String(vec) => {
                let string_value: String = value.extract()?;
                vec.insert(index, string_value);
            }
            NativeArray::PyObject(arc_mutex_vec) => {
                let mut vec = arc_mutex_vec.lock().unwrap();
                vec.insert(index, value.clone().into());
            }
        }
        Ok(())
    }

    /// Truncates the array to the specified size.
    pub fn truncate(&mut self, new_size: usize) {
        match self {
            NativeArray::Int8(vec) => vec.truncate(new_size),
            NativeArray::UInt8(vec) => vec.truncate(new_size),
            NativeArray::Int16(vec) => vec.truncate(new_size),
            NativeArray::UInt16(vec) => vec.truncate(new_size),
            NativeArray::Int32(vec) => vec.truncate(new_size),
            NativeArray::UInt32(vec) => vec.truncate(new_size),
            NativeArray::Int64(vec) => vec.truncate(new_size),
            NativeArray::UInt64(vec) => vec.truncate(new_size),
            NativeArray::Float32(vec) => vec.truncate(new_size),
            NativeArray::Float64(vec) => vec.truncate(new_size),
            NativeArray::String(vec) => vec.truncate(new_size),
            NativeArray::PyObject(vec) => {
                if let Ok(mut vec) = vec.lock() {
                    vec.truncate(new_size);
                }
            }
        }
    }
}
