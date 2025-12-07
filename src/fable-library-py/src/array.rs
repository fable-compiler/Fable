use crate::floats::{Float32, Float64};
use crate::ints::{Int16, Int32, Int64, Int8, UInt16, UInt32, UInt64, UInt8};
use crate::native_array::{ArrayType, NativeArray};
use crate::options::SomeWrapper;
use crate::types::FSharpRef;
use crate::util::{DefaultComparer, ProjectionComparer};
use pyo3::class::basic::CompareOp;
use pyo3::types::PyNotImplemented;
use pyo3::types::{PyBool, PyInt};
use pyo3::types::{PyBytes, PyTuple, PyType};
use pyo3::BoundObject;
use pyo3::{exceptions, IntoPyObjectExt, PyTypeInfo};
use pyo3::{
    prelude::*,
    types::{PyAnyMethods, PyList},
};
use std::sync::{Arc, Mutex};

#[pyclass(module = "fable", subclass)]
#[derive(Clone, Debug)]

pub struct FSharpArray {
    storage: NativeArray,
}

#[pyclass(module="fable", extends=FSharpArray)]
struct Int8Array {}

#[pyclass(module="fable", extends=FSharpArray)]
struct UInt8Array {}

#[pyclass(module="fable", extends=FSharpArray)]
struct Int16Array {}

#[pyclass(module="fable", extends=FSharpArray)]
struct UInt16Array {}

#[pyclass(module="fable", extends=FSharpArray)]
struct Int32Array {}

#[pyclass(module="fable", extends=FSharpArray)]
struct UInt32Array {}

#[pyclass(module="fable", extends=FSharpArray)]
struct Int64Array {}

#[pyclass(module="fable", extends=FSharpArray)]
struct UInt64Array {}

#[pyclass(module="fable", extends=FSharpArray)]
struct Float32Array {}

#[pyclass(module="fable", extends=FSharpArray)]
struct Float64Array {}

#[pyclass(module="fable", extends=FSharpArray)]
struct BoolArray {}

#[pyclass(module="fable", extends=FSharpArray)]
struct GenericArray {}

// Macro to reduce repetition in type extraction for FSharpArray::new.
// This ensures both elegance and performance, following best Rust and craftsman practices.
macro_rules! try_extract_array {
    ($elements:expr, $py:expr, $variant:ident, $wrapper:ty, $native:ty) => {
        if let Ok(vec) = extract_typed_vec_from_iterable::<$native>($elements) {
            Some(FSharpArray {
                storage: NativeArray::$variant(vec),
            })
        } else {
            None
        }
    };
}

// Utility function to convert Python objects to FSharpArray.
fn ensure_array(py: Python<'_>, ob: &Bound<'_, PyAny>) -> PyResult<FSharpArray> {
    // If it's already a FSharpArray, just extract it
    if let Ok(array) = ob.extract::<PyRef<'_, FSharpArray>>() {
        return Ok(array.clone());
    }

    // If the object is None (null), create an empty array
    if ob.is_none() {
        return FSharpArray::new(py, None, None);
    }

    // Check if the object is iterable
    if let Ok(iter) = ob.try_iter() {
        // Convert iterable directly to FSharpArray
        return FSharpArray::new(py, Some(iter.as_any()), None);
    }

    // If it's a single item, create a singleton array
    let singleton_list = PyList::new(py, [ob])?;
    FSharpArray::new(py, Some(&singleton_list), None)
}

#[pymethods]
impl FSharpArray {
    #[new]
    #[pyo3(signature = (elements=None, array_type=None))]
    pub fn new(
        py: Python<'_>,
        elements: Option<&Bound<'_, PyAny>>,
        array_type: Option<&str>,
    ) -> PyResult<Self> {
        let nominal_type = array_type
            .map(ArrayType::from_str)
            .unwrap_or(ArrayType::Generic);

        // Handle empty array case early
        let Some(elements) = elements else {
            return Ok(FSharpArray {
                storage: NativeArray::create_empty_storage(&nominal_type),
            });
        };

        // Try to extract as typed arrays first - if any succeed, return immediately
        let typed_array = match &nominal_type {
            ArrayType::Int8 => {
                try_extract_array!(elements, py, Int8, Int8, i8)
            }
            ArrayType::UInt8 => {
                try_extract_array!(elements, py, UInt8, UInt8, u8)
            }
            ArrayType::Int16 => {
                try_extract_array!(elements, py, Int16, Int16, i16)
            }
            ArrayType::UInt16 => {
                try_extract_array!(elements, py, UInt16, UInt16, u16)
            }
            ArrayType::Int32 => {
                try_extract_array!(elements, py, Int32, Int32, i32)
            }
            ArrayType::UInt32 => {
                try_extract_array!(elements, py, UInt32, UInt32, u32)
            }
            ArrayType::Int64 => {
                try_extract_array!(elements, py, Int64, Int64, i64)
            }
            ArrayType::UInt64 => {
                try_extract_array!(elements, py, UInt64, UInt64, u64)
            }
            ArrayType::Float32 => {
                try_extract_array!(elements, py, Float32, Float32, f32)
            }
            ArrayType::Float64 => {
                try_extract_array!(elements, py, Float64, Float64, f64)
            }
            ArrayType::Bool => {
                try_extract_array!(elements, py, Bool, bool, bool)
            }
            _ => None,
        };

        // If typed extraction succeeded, return it
        if let Some(array) = typed_array {
            return Ok(array);
        }

        // Fallback to PyObject storage if type extraction fails.
        // This allows for generic or mixed-type arrays, at the cost of dynamic dispatch and locking.
        // Arc<Mutex<...>> is used for thread safety and Python interop.
        let len = elements.len().unwrap_or(0);
        let mut vec = Vec::with_capacity(len);
        if let Ok(iter) = elements.try_iter() {
            for item in iter {
                vec.push(item?.into_pyobject(py)?.into());
            }
        }
        Ok(FSharpArray {
            storage: NativeArray::PyObject(Arc::new(Mutex::new(vec))),
        })
    }

    #[classmethod]
    pub fn __class_getitem__(
        _cls: &Bound<'_, PyType>,
        item: &Bound<'_, PyAny>,
        py: Python<'_>,
    ) -> PyResult<Py<PyAny>> {
        // Get type name - either from string or from type.__name__
        let type_name: Option<String> = if let Ok(s) = item.extract::<String>() {
            Some(s)
        } else if let Ok(py_type) = item.cast::<PyType>() {
            py_type.getattr("__name__")?.extract()?
        } else {
            None
        };

        // Match on the type name
        let array_class = match type_name.map(|s| s.to_lowercase()).as_deref() {
            Some("int8") | Some("sbyte") => Int8Array::type_object(py),
            Some("uint8") | Some("byte") => UInt8Array::type_object(py),
            Some("int16") => Int16Array::type_object(py),
            Some("uint16") => UInt16Array::type_object(py),
            Some("int32") => Int32Array::type_object(py),
            Some("uint32") => UInt32Array::type_object(py),
            Some("int64") => Int64Array::type_object(py),
            Some("uint64") => UInt64Array::type_object(py),
            Some("float32") => Float32Array::type_object(py),
            Some("float64") => Float64Array::type_object(py),
            Some("bool") => BoolArray::type_object(py),
            _ => GenericArray::type_object(py),
        };

        Ok(array_class.into_pyobject(py)?.into())
    }

    /// Creates an array whose elements are all initially the given value.
    #[staticmethod]
    pub fn create(
        _py: Python<'_>,
        count: usize,
        value: &Bound<'_, PyAny>,
    ) -> PyResult<FSharpArray> {
        // Attempt to create specialized arrays based on value type
        if let Ok(int8) = value.extract::<Int8>() {
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, *int8);
            return Ok(FSharpArray {
                storage: NativeArray::Int8(vec),
            });
        } else if let Ok(uint8) = value.extract::<UInt8>() {
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, *uint8);
            return Ok(FSharpArray {
                storage: NativeArray::UInt8(vec),
            });
        } else if let Ok(int16) = value.extract::<Int16>() {
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, *int16);
            return Ok(FSharpArray {
                storage: NativeArray::Int16(vec),
            });
        } else if let Ok(uint16) = value.extract::<UInt16>() {
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, *uint16);
            return Ok(FSharpArray {
                storage: NativeArray::UInt16(vec),
            });
        } else if let Ok(int32) = value.extract::<Int32>() {
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, *int32);
            return Ok(FSharpArray {
                storage: NativeArray::Int32(vec),
            });
        } else if let Ok(uint32) = value.extract::<UInt32>() {
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, *uint32);
            return Ok(FSharpArray {
                storage: NativeArray::UInt32(vec),
            });
        } else if let Ok(int64) = value.extract::<Int64>() {
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, *int64);
            return Ok(FSharpArray {
                storage: NativeArray::Int64(vec),
            });
        } else if let Ok(uint64) = value.extract::<UInt64>() {
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, *uint64);
            return Ok(FSharpArray {
                storage: NativeArray::UInt64(vec),
            });
        } else if let Ok(float32) = value.extract::<Float32>() {
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, *float32);
            return Ok(FSharpArray {
                storage: NativeArray::Float32(vec),
            });
        } else if let Ok(float64) = value.extract::<Float64>() {
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, *float64);
            return Ok(FSharpArray {
                storage: NativeArray::Float64(vec),
            });
        } else if let Ok(bool_val) = value.extract::<bool>() {
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, bool_val);
            return Ok(FSharpArray {
                storage: NativeArray::Bool(vec),
            });
        }

        // Fallback to generic PyObject storage
        let mut vec = Vec::with_capacity(count);
        for _ in 0..count {
            vec.push(value.clone().into());
        }

        Ok(FSharpArray {
            storage: NativeArray::PyObject(Arc::new(Mutex::new(vec))),
        })
    }

    pub fn __richcmp__<'py>(
        &self,
        other: &Bound<'_, PyAny>,
        op: CompareOp,
        py: Python<'py>,
    ) -> PyResult<Borrowed<'py, 'py, PyAny>> {
        // First check if other is a FSharpArray
        if let Ok(other_array) = other.extract::<PyRef<'_, FSharpArray>>() {
            match op {
                CompareOp::Eq => {
                    let result = self.storage.equals(&other_array.storage, py);
                    Ok(PyBool::new(py, result).into_any())
                }
                CompareOp::Ne => {
                    // For inequality, negate the equality result
                    let result = self.storage.equals(&other_array.storage, py);
                    Ok(PyBool::new(py, !result).into_any())
                }
                CompareOp::Lt | CompareOp::Le | CompareOp::Gt | CompareOp::Ge => {
                    // Create a default comparer using direct Python comparison
                    let default_comparer = DefaultComparer::new()?.into_pyobject(py)?;
                    let comparison_result =
                        self.compare_with(py, &default_comparer, &other_array)?;

                    let result = match op {
                        CompareOp::Lt => comparison_result < 0,
                        CompareOp::Le => comparison_result <= 0,
                        CompareOp::Gt => comparison_result > 0,
                        CompareOp::Ge => comparison_result >= 0,
                        _ => unreachable!(),
                    };
                    Ok(PyBool::new(py, result).into_any())
                }
            }
        } else {
            // If other is not a FSharpArray, return NotImplemented
            Ok(PyNotImplemented::get(py).into_any())
        }
    }

    pub fn __add__(&self, other: &Bound<'_, PyAny>, py: Python<'_>) -> PyResult<Py<PyAny>> {
        // Use append to implement array concatenation
        let result = self.append(py, other, None)?;
        Ok(result.into_pyobject(py)?.into())
    }

    #[staticmethod]
    pub fn initialize(
        py: Python<'_>,
        count: usize,
        initializer: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        if count == 0 {
            return FSharpArray::empty(py, cons);
        }

        // Create the builder for results
        let fs_cons = FSharpCons::extract(cons, &ArrayType::Generic);
        let mut results = fs_cons.create(count);

        // Initialize each element using the provided initializer function
        for i in 0..count {
            let item = initializer.call1((i,))?;
            results.push_value(&item, py)?;
        }

        // Construct the result array
        Ok(FSharpArray { storage: results })
    }

    #[staticmethod]
    #[pyo3(signature = (value, cons=None))]
    pub fn singleton(
        py: Python<'_>,
        value: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        // Determine the type from constructor

        let fs_cons = FSharpCons::extract(cons, &ArrayType::Generic);
        let mut builder = fs_cons.create(1);

        // Set the single element
        builder.push_value(value, py)?;

        let array = FSharpArray { storage: builder };
        Ok(array)
    }

    #[inline]
    pub fn __len__(&self) -> usize {
        self.storage.len()
    }

    pub fn __iter__(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        let iter = FSharpArrayIter {
            array: Py::new(py, self.clone())?,
            index: 0,
            len: self.storage.len(),
        };
        iter.into_py_any(py)
    }

    /// Pydantic v2 integration for schema generation.
    ///
    /// This method is called by Pydantic when building a model that uses FSharpArray.
    /// It returns a pydantic-core schema that enables:
    /// - Validation of input values (any iterable)
    /// - Serialization to JSON-compatible lists
    /// - JSON Schema generation for OpenAPI documentation
    ///
    /// The pydantic_core module is imported lazily - pydantic is only required
    /// if this method is actually called (i.e., when used in a Pydantic model).
    #[classmethod]
    #[pyo3(name = "__get_pydantic_core_schema__")]
    fn get_pydantic_core_schema(
        cls: &Bound<'_, PyType>,
        _source_type: &Bound<'_, PyAny>,
        _handler: &Bound<'_, PyAny>,
        py: Python<'_>,
    ) -> PyResult<Py<PyAny>> {
        // Lazy import of pydantic_core - only fails if pydantic is not installed
        // AND this type is used in a Pydantic model
        let core_schema = py.import("pydantic_core")?.getattr("core_schema")?;

        // Create a validator function that wraps the constructor
        let validator_fn = cls.getattr("_pydantic_validator")?;

        // Create a serializer function that converts to list
        let serializer_fn = cls.getattr("_pydantic_serializer")?;

        // Build the serialization schema
        let ser_schema = core_schema.call_method1(
            "plain_serializer_function_ser_schema",
            (serializer_fn,),
        )?;

        // Create a list schema as the base - this enables JSON Schema generation
        let list_schema = core_schema.call_method0("list_schema")?;

        // Wrap with validator function, keeping list_schema for JSON Schema
        let validator_kwargs = pyo3::types::PyDict::new(py);
        validator_kwargs.set_item("serialization", ser_schema)?;

        let validator_schema = core_schema.call_method(
            "no_info_after_validator_function",
            (validator_fn, list_schema),
            Some(&validator_kwargs),
        )?;

        Ok(validator_schema.unbind())
    }

    /// Pydantic validator function.
    ///
    /// Called by Pydantic during validation to convert input values to FSharpArray.
    /// Accepts any iterable.
    #[staticmethod]
    #[pyo3(name = "_pydantic_validator")]
    fn pydantic_validator(py: Python<'_>, value: &Bound<'_, PyAny>) -> PyResult<Self> {
        // If already a FSharpArray, return a clone
        if let Ok(array) = value.extract::<PyRef<'_, FSharpArray>>() {
            return Ok(array.clone());
        }
        // Otherwise create from iterable
        FSharpArray::new(py, Some(value), None)
    }

    /// Pydantic serializer function.
    ///
    /// Called by Pydantic during serialization to convert FSharpArray to a
    /// JSON-compatible list.
    #[staticmethod]
    #[pyo3(name = "_pydantic_serializer")]
    fn pydantic_serializer(py: Python<'_>, instance: &Self) -> PyResult<Py<PyAny>> {
        // Convert FSharpArray to a Python list for JSON serialization
        let len = instance.storage.len();
        let list = PyList::empty(py);
        for i in 0..len {
            let item = instance.storage.get(py, i)?;
            list.append(item)?;
        }
        Ok(list.into())
    }

    /// Returns the raw bytes of the array's underlying storage.
    ///
    /// For numeric types (Int8, UInt8, Int16, etc.), this returns the raw memory
    /// representation of the array elements as bytes. The byte order is native
    /// (platform-dependent): little-endian on x86/x64/ARM, big-endian on some
    /// older architectures.
    ///
    /// For Int8/UInt8 arrays, the result is a direct byte representation.
    /// For larger types (Int16, Int32, Float64, etc.), each element occupies
    /// multiple bytes in native byte order.
    ///
    /// Returns NotImplemented for String and PyObject arrays.
    pub fn __bytes__(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        match &self.storage {
            // For UInt8/Int8 arrays, we can create bytes directly
            NativeArray::UInt8(vec) => {
                let bytes = PyBytes::new(py, vec.as_slice());
                Ok(bytes.into())
            }
            NativeArray::Int8(vec) => {
                // Convert i8 slice to u8 slice with unsafe transmute
                // This is safe because we're just reinterpreting the bits
                let bytes = PyBytes::new(py, unsafe {
                    std::slice::from_raw_parts(vec.as_ptr() as *const u8, vec.len())
                });
                Ok(bytes.into())
            }
            // For other numeric types, create a bytearray from their raw memory
            NativeArray::Int16(vec) => {
                let bytes = PyBytes::new(py, unsafe {
                    std::slice::from_raw_parts(
                        vec.as_ptr() as *const u8,
                        vec.len() * std::mem::size_of::<i16>(),
                    )
                });
                Ok(bytes.into())
            }
            NativeArray::UInt16(vec) => {
                let bytes = PyBytes::new(py, unsafe {
                    std::slice::from_raw_parts(
                        vec.as_ptr() as *const u8,
                        vec.len() * std::mem::size_of::<u16>(),
                    )
                });
                Ok(bytes.into())
            }
            // Similar patterns for other numeric types
            NativeArray::Int32(vec) => {
                let bytes = PyBytes::new(py, unsafe {
                    std::slice::from_raw_parts(
                        vec.as_ptr() as *const u8,
                        vec.len() * std::mem::size_of::<i32>(),
                    )
                });
                Ok(bytes.into())
            }
            NativeArray::UInt32(vec) => {
                let bytes = PyBytes::new(py, unsafe {
                    std::slice::from_raw_parts(
                        vec.as_ptr() as *const u8,
                        vec.len() * std::mem::size_of::<u32>(),
                    )
                });
                Ok(bytes.into())
            }
            NativeArray::Int64(vec) => {
                let bytes = PyBytes::new(py, unsafe {
                    std::slice::from_raw_parts(
                        vec.as_ptr() as *const u8,
                        vec.len() * std::mem::size_of::<i64>(),
                    )
                });
                Ok(bytes.into())
            }
            NativeArray::UInt64(vec) => {
                let bytes = PyBytes::new(py, unsafe {
                    std::slice::from_raw_parts(
                        vec.as_ptr() as *const u8,
                        vec.len() * std::mem::size_of::<u64>(),
                    )
                });
                Ok(bytes.into())
            }
            NativeArray::Float32(vec) => {
                let bytes = PyBytes::new(py, unsafe {
                    std::slice::from_raw_parts(
                        vec.as_ptr() as *const u8,
                        vec.len() * std::mem::size_of::<f32>(),
                    )
                });
                Ok(bytes.into())
            }
            NativeArray::Float64(vec) => {
                let bytes = PyBytes::new(py, unsafe {
                    std::slice::from_raw_parts(
                        vec.as_ptr() as *const u8,
                        vec.len() * std::mem::size_of::<f64>(),
                    )
                });
                Ok(bytes.into())
            }
            // For non-numeric types, return NotImplemented
            _ => Ok(py.NotImplemented()),
        }
    }

    // Separate function to handle slice access
    fn get_item_slice(
        &self,
        slice: &Bound<'_, pyo3::types::PySlice>,
        py: Python<'_>,
    ) -> PyResult<Py<PyAny>> {
        let len = self.storage.len();
        let indices = slice.indices(len as isize)?;
        let start = indices.start as usize;
        let stop = indices.stop as usize;
        let step = indices.step;

        // If step is not 1, we need to handle it differently
        if step != 1 {
            // Calculate the size of the resulting array
            let mut size = 0;
            let mut i = indices.start;
            while (step > 0 && i < indices.stop) || (step < 0 && i > indices.stop) {
                size += 1;
                i += step;
            }

            // Create a new array with the same type
            let fs_cons = FSharpCons::new(
                &self
                    .storage
                    .type_name()
                    .into_pyobject(py)?
                    .extract::<String>()?,
            )?;
            let mut result = fs_cons.allocate(py, size)?;

            // Fill the result array with sliced elements
            let mut result_idx = 0;
            i = indices.start;
            while (step > 0 && i < indices.stop) || (step < 0 && i > indices.stop) {
                let item = self.get_item_at_index(i, py)?;
                result.__setitem__(result_idx, item.bind(py), py)?;
                result_idx += 1;
                i += step;
            }

            return Ok(Py::new(py, result)?.into());
        }

        // For step=1, use set_slice
        // Create a new array of the appropriate size
        let slice_len = if step > 0 {
            stop - start
        } else {
            // For negative steps, the calculation is different
            start - stop
        };
        // println!("Slice length: {:?}", slice_len);
        // println!("Nominal type: {:?}", self.nominal_type);
        let mut builder = NativeArray::new(self.storage.get_type(), Some(slice_len));

        // Add each element from the slice range
        for i in 0..slice_len {
            builder.push_from_storage(&self.storage, start + i, py);
        }

        // Create the result array
        let result = FSharpArray { storage: builder };
        Ok(Py::new(py, result)?.into())
    }

    pub fn __getitem__(&self, idx: &Bound<'_, PyAny>, py: Python<'_>) -> PyResult<Py<PyAny>> {
        // Try to downcast to a slice first
        if let Ok(slice) = idx.cast::<pyo3::types::PySlice>() {
            // println!("Slice: {:?}", slice);
            self.get_item_slice(slice, py)
        }
        // Then try to extract as an integer
        else if let Ok(i) = idx.extract::<isize>() {
            // println!("Integer: {:?}", i);
            self.get_item_at_index(i, py)
        }
        // If neither works, raise TypeError
        else {
            Err(PyErr::new::<exceptions::PyTypeError, _>(
                "indices must be integers or slices",
            ))
        }
    }

    // Helper method to get an item at a specific index
    #[inline]
    fn get_item_at_index(&self, idx: isize, py: Python<'_>) -> PyResult<Py<PyAny>> {
        let len = self.storage.len();
        let idx = if idx < 0 { len as isize + idx } else { idx };

        if idx < 0 || idx as usize >= len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "index out of range",
            ));
        }

        self.storage.get(py, idx as usize)
    }

    pub fn __setitem__(
        &mut self,
        idx: isize,
        value: &Bound<'_, PyAny>,
        _py: Python<'_>,
    ) -> PyResult<()> {
        let len = self.storage.len();
        let idx = if idx < 0 { len as isize + idx } else { idx };

        if idx < 0 || idx as usize >= len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "index out of range",
            ));
        }

        match &mut self.storage {
            NativeArray::Int8(vec) => {
                if let Ok(i_val) = value.extract::<i8>() {
                    vec[idx as usize] = i_val;
                    return Ok(());
                }
            }
            NativeArray::UInt8(vec) => {
                if let Ok(u_val) = value.extract::<u8>() {
                    vec[idx as usize] = u_val;
                    return Ok(());
                }
            }
            NativeArray::Int16(vec) => {
                if let Ok(i_val) = value.extract::<i16>() {
                    vec[idx as usize] = i_val;
                    return Ok(());
                }
            }
            NativeArray::UInt16(vec) => {
                if let Ok(u_val) = value.extract::<u16>() {
                    vec[idx as usize] = u_val;
                    return Ok(());
                }
            }
            NativeArray::Int32(vec) => {
                if let Ok(i_val) = value.extract::<i32>() {
                    vec[idx as usize] = i_val;
                    return Ok(());
                }
            }
            NativeArray::UInt32(vec) => {
                if let Ok(u_val) = value.extract::<u32>() {
                    vec[idx as usize] = u_val;
                    return Ok(());
                }
            }
            NativeArray::Int64(vec) => {
                if let Ok(i_val) = value.extract::<i64>() {
                    vec[idx as usize] = i_val;
                    return Ok(());
                }
            }
            NativeArray::UInt64(vec) => {
                // Fast path: Try to extract directly as u64
                if let Ok(u_val) = value.extract::<u64>() {
                    vec[idx as usize] = u_val;
                    return Ok(());
                }

                // Fast path: Try to extract as UInt64 wrapper
                if let Ok(uint64_value) = value.extract::<UInt64>() {
                    vec[idx as usize] = *uint64_value;
                    return Ok(());
                }

                // Fast path: Try to extract as Float32/Float64 and convert
                if let Ok(f32_val) = value.extract::<crate::floats::Float32>() {
                    vec[idx as usize] = f32_val.0 as u64;
                    return Ok(());
                }

                if let Ok(f64_val) = value.extract::<crate::floats::Float64>() {
                    vec[idx as usize] = f64_val.0 as u64;
                    return Ok(());
                }

                // Fast path: Try to extract as other integer types
                if let Ok(i_val) = value.extract::<i64>() {
                    vec[idx as usize] = i_val as u64;
                    return Ok(());
                }

                // Fallback to full conversion
                let uint64_value: UInt64 = UInt64::new(value)?;
                vec[idx as usize] = *uint64_value;
            }
            NativeArray::Float32(vec) => {
                // Fast path: Try to extract as Float32 wrapper first
                if let Ok(float32_value) = value.extract::<Float32>() {
                    vec[idx as usize] = *float32_value;
                    return Ok(());
                }

                // Fast path: Try to extract directly as f32
                if let Ok(f_val) = value.extract::<f32>() {
                    vec[idx as usize] = f_val;
                    return Ok(());
                }

                // Fallback to full conversion
                let float32_value = value.extract::<Float32>()?;
                vec[idx as usize] = *float32_value;
            }
            NativeArray::Float64(vec) => {
                // Fast path: Try to extract as Float64 wrapper first
                if let Ok(float64_value) = value.extract::<Float64>() {
                    vec[idx as usize] = *float64_value;
                    return Ok(());
                }

                // Fast path: Try to extract directly as f64
                if let Ok(f_val) = value.extract::<f64>() {
                    vec[idx as usize] = f_val;
                    return Ok(());
                }

                // Fallback to full conversion
                let float64_value = value.extract::<Float64>()?;
                vec[idx as usize] = *float64_value;
            }
            NativeArray::Bool(vec) => {
                vec[idx as usize] = value.extract()?;
            }
            NativeArray::PyObject(arc_mutex_vec) => {
                let mut vec = arc_mutex_vec.lock().unwrap(); // Acquire the lock
                vec[idx as usize] = value.clone().into();
            }
        }

        Ok(())
    }

    pub fn __delitem__(&mut self, idx: isize, _py: Python<'_>) -> PyResult<()> {
        let len = self.storage.len();
        let idx = if idx < 0 { len as isize + idx } else { idx };

        if idx < 0 || idx as usize >= len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "index out of range",
            ));
        }

        self.storage.remove_at_index(idx as usize);
        Ok(())
    }

    pub fn insert(&mut self, idx: isize, value: &Bound<'_, PyAny>, py: Python<'_>) -> PyResult<()> {
        let len = self.storage.len();
        let idx = if idx < 0 { len as isize + idx } else { idx };

        if idx < 0 || idx as usize > len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "index out of range",
            ));
        }
        self.storage.insert(idx as usize, value, py)
    }

    #[staticmethod]
    #[pyo3(signature = (cons=None))]
    pub fn empty(py: Python<'_>, cons: Option<&Bound<'_, PyAny>>) -> PyResult<FSharpArray> {
        // Determine the type from constructor
        let fs_cons = FSharpCons::extract(cons, &ArrayType::Generic);

        // Create an empty array with this type
        let array = fs_cons.allocate(py, 0)?;
        Ok(array)
    }

    pub fn remove_at(&mut self, py: Python<'_>, index: isize) -> PyResult<FSharpArray> {
        let len = self.storage.len();
        if index < 0 || index as usize >= len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "index out of range",
            ));
        }

        // Create a new array with the same type and size - 1
        let mut builder = NativeArray::new(self.storage.get_type(), Some(len - 1));

        // Copy all elements except the one at the specified index
        for i in 0..len {
            if i as isize != index {
                builder.push_from_storage(&self.storage, i, py);
            }
        }

        // Create the result array
        Ok(FSharpArray { storage: builder })
    }

    pub fn remove_many_at(
        &mut self,
        py: Python<'_>,
        index: isize,
        count: usize,
    ) -> PyResult<FSharpArray> {
        let len = self.storage.len();

        // Check if the index is out of bounds in either direction
        if index < -(len as isize) || index as usize >= len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "index out of range",
            ));
        }

        // Create a new array with the same type and size - count
        let mut builder = NativeArray::new(self.storage.get_type(), Some(len - count));

        // Copy all elements except the ones at the specified indices
        for i in 0..len {
            if i < index as usize || i >= index as usize + count {
                builder.push_from_storage(&self.storage, i, py);
            }
        }

        // Create the result array
        let result = FSharpArray { storage: builder };
        Ok(result)
    }

    // Map implementation (Refactored with ArrayStorage)
    #[pyo3(signature = (f, cons=None))]
    pub fn map(
        &self,
        py: Python<'_>,
        f: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<Self> {
        let len = self.storage.len();

        // Determine target type from cons or preserve source type
        let target_type = if let Some(cons) = cons {
            if let Ok(fs_cons) = cons.extract::<PyRef<'_, FSharpCons>>() {
                fs_cons.array_type.clone()
            } else {
                // Fallback if cons is not a FSharpCons
                self.storage.get_type().clone()
            }
        } else {
            // If no constructor is provided, assume the type might change and default to Generic
            ArrayType::Generic
        };

        // Create a helper to collect results based on the target type
        let mut results = NativeArray::new(&target_type, Some(len));

        // Map each element
        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            let mapped = f.call1((item,))?;
            // Push the mapped item into the results collector
            results.push_value(&mapped, py)?;
        }

        // Convert the collected results into the final storage
        Ok(FSharpArray { storage: results })
    }

    pub fn map2(
        &self,
        py: Python<'_>,
        f: &Bound<'_, PyAny>,
        source2: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<Self> {
        let len = self.storage.len();
        let source2 = ensure_array(py, source2)?;

        // Check lengths match
        if len != source2.storage.len() {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "Arrays had different lengths",
            ));
        }

        // Get constructor from cons parameter
        let fs_cons = FSharpCons::extract(cons, self.storage.get_type());
        let mut results = fs_cons.create(len);

        // Map each element with counterpart in source2
        for i in 0..len {
            let item1 = self.get_item_at_index(i as isize, py)?;
            let item2 = source2.get_item_at_index(i as isize, py)?;
            let mapped = f.call1((item1, item2))?;
            results.push_value(&mapped, py)?;
        }

        // Return final array
        Ok(FSharpArray { storage: results })
    }

    pub fn map_indexed(
        &self,
        py: Python<'_>,
        f: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<Self> {
        let len = self.storage.len();

        // Determine target type from cons or preserve source type
        let target_type = if let Some(cons) = cons {
            if let Ok(fs_cons) = cons.extract::<PyRef<'_, FSharpCons>>() {
                fs_cons.array_type.clone()
            } else {
                // Fallback if cons is not a FSharpCons
                self.storage.get_type().clone()
            }
        } else {
            // If no constructor is provided, assume the type might change and default to Generic
            ArrayType::Generic
        };

        // Create a helper to collect results based on the target type
        let mut results = NativeArray::new(&target_type, Some(len));

        // Map each element with its index
        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            let mapped = f.call1((i, item))?;
            // Push the mapped item into the results collector
            results.push_value(&mapped, py)?;
        }

        // Convert the collected results into the final storage
        Ok(FSharpArray { storage: results })
    }

    // Filter implementation (Refactored with ArrayStorage)
    // Expose this method to Python
    #[pyo3(signature = (predicate))]
    pub fn filter(&self, py: Python<'_>, predicate: &Bound<'_, PyAny>) -> PyResult<Self> {
        let len = self.storage.len();
        let original_type = self.storage.get_type().clone();

        // Create a helper to collect results based on the original type
        let mut results = NativeArray::new(&original_type, None); // No initial capacity needed

        for i in 0..len {
            // Avoid cloning item_obj if possible, only clone for predicate call
            let item_obj = self.get_item_at_index(i as isize, py)?;
            let keep = predicate.call1((item_obj.clone_ref(py),))?.is_truthy()?;

            if keep {
                // Push the original item (by index) into the results collector
                results.push_from_storage(&self.storage, i, py);
            }
        }

        // Convert the collected results into the final storage
        Ok(FSharpArray { storage: results })
    }

    // Skip implementation using our refined ArrayStorage
    #[pyo3(signature = (count, cons=None))]
    pub fn skip(
        &self,
        py: Python<'_>,
        count: isize,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let len = self.storage.len();

        // Handle negative count by treating it as 0 (F# behavior)
        let count = if count < 0 { 0 } else { count as usize };

        // Check if count is greater than array length (F# throws error)
        if count > len {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "count is greater than array length",
            ));
        }

        // Use the source array's type instead of defaulting to Generic
        let fs_cons = FSharpCons::extract(cons, self.storage.get_type());

        // If count equals array length, return empty array
        if count == len {
            return fs_cons.allocate(py, 0);
        }

        // Create the builder for results
        let mut results = fs_cons.create(len - count);

        // Add the remaining elements (after skipping)
        for i in count..len {
            results.push_from_storage(&self.storage, i, py);
        }

        // Construct the result array
        Ok(FSharpArray { storage: results })
    }

    #[pyo3(signature = (predicate, cons=None))]
    pub fn skip_while(
        &self,
        py: Python<'_>,
        predicate: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let len = self.storage.len();
        let mut count = 0;

        while count < len {
            let item = self.get_item_at_index(count as isize, py)?;
            if !predicate.call1((item,))?.is_truthy()? {
                break;
            }
            count += 1;
        }

        if count == len {
            let fs_cons = FSharpCons::extract(cons, self.storage.get_type());
            return fs_cons.allocate(py, 0);
        }

        self.skip(py, count as isize, cons)
    }

    #[pyo3(signature = (predicate, cons=None))]
    pub fn take_while(
        &self,
        py: Python<'_>,
        predicate: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let len = self.storage.len();
        let mut count = 0;

        while count < len {
            let item = self.get_item_at_index(count as isize, py)?;
            if !predicate.call1((item,))?.is_truthy()? {
                break;
            }
            count += 1;
        }

        self.take(py, count as isize, cons)
    }

    pub fn chunk_by_size(&self, py: Python<'_>, chunk_size: usize) -> PyResult<Self> {
        if chunk_size < 1 {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "The input must be positive.",
            ));
        }

        let len = self.storage.len();
        if len == 0 {
            // Return an empty array
            return Ok(FSharpArray {
                storage: NativeArray::PyObject(Arc::new(Mutex::new(vec![]))),
            });
        }

        // Create an array of arrays (chunks)
        let mut chunks = NativeArray::PyObject(Arc::new(Mutex::new(vec![])));

        // Create each chunk
        for x in 0..len.div_ceil(chunk_size) {
            let start = x * chunk_size;
            let end = std::cmp::min(start + chunk_size, len);

            // Create a new array for this chunk
            let mut chunk = NativeArray::new(self.storage.get_type(), Some(end - start));

            // Fill the chunk with elements from the source array
            for i in start..end {
                chunk.push_from_storage(&self.storage, i, py);
            }

            // Add the chunk to results
            let chunk_array = FSharpArray { storage: chunk };
            chunks.push_value(&chunk_array.into_pyobject(py)?.into_any(), py)?;
        }

        // Return the array of chunks
        Ok(FSharpArray { storage: chunks })
    }

    pub fn fill(
        &mut self,
        target_index: isize,
        count: usize,
        value: &Bound<'_, PyAny>,
        _py: Python<'_>,
    ) -> PyResult<()> {
        // Validate input parameters
        let len = self.storage.len();

        if target_index < 0 || target_index as usize >= len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "target_index out of range",
            ));
        }

        // Make sure we don't go beyond array bounds
        let available_slots = len - target_index as usize;
        let actual_count = std::cmp::min(count, available_slots);

        // For all other types, use the helper
        NativeArray::fill_storage(
            &mut self.storage,
            target_index as usize,
            actual_count,
            value,
        )?;

        Ok(())
    }

    pub fn fold(
        &self,
        py: Python<'_>,
        folder: &Bound<'_, PyAny>,
        state: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        let len = self.storage.len();
        let mut acc = state.clone();

        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            acc = folder.call1((acc, item))?;
        }

        Ok(acc.into())
    }

    pub fn fold_indexed(
        &self,
        py: Python<'_>,
        folder: &Bound<'_, PyAny>,
        state: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        let len = self.storage.len();
        let mut acc = state.clone();

        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            acc = folder.call1((i, acc, item))?;
        }

        Ok(acc.into())
    }

    pub fn fold_back(
        &self,
        py: Python<'_>,
        folder: &Bound<'_, PyAny>,
        state: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        let len = self.storage.len();
        let mut acc = state.clone();

        for i in (0..len).rev() {
            let item = self.get_item_at_index(i as isize, py)?;
            acc = folder.call1((item, acc))?;
        }

        Ok(acc.into())
    }

    // let resize
    //     (xs: byref<'T[]>)
    //     (newSize: int)
    //     ([<OptionalArgument>] zero: 'T option)
    //     ([<OptionalArgument; Inject>] cons: Cons<'T>)
    //     : unit
    //     =
    //     if newSize < 0 then
    //         invalidArg "newSize" "The input must be non-negative."

    //     let zero = defaultArg zero Unchecked.defaultof<_>

    //     if isNull xs then
    //         xs <- fillImpl (allocateArrayFromCons cons newSize) zero 0 newSize

    //     else
    //         let len = xs.Length

    //         if newSize < len then
    //             xs <- subArrayImpl xs 0 newSize

    //         elif newSize > len then
    //             let target = allocateArrayFromCons cons newSize

    //             if len > 0 then
    //                 copyTo xs 0 target 0 len

    //             xs <- fillImpl target zero len (newSize - len)

    #[pyo3(signature = (new_size, zero=None, cons=None))]
    pub fn resize(
        &mut self,
        py: Python<'_>,
        new_size: usize,
        zero: Option<&Bound<'_, PyAny>>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<()> {
        // Check for negative size (though usize is unsigned, keeping for consistency with F#)
        if new_size > isize::MAX as usize {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "The input must be non-negative.",
            ));
        }

        let current_len = self.storage.len();

        let zero_value = if let Some(zero) = zero {
            zero.clone() // Use the provided zero value directly
        } else {
            self.storage.get_type().default_value(py)?
        };

        if new_size < current_len {
            // Shrink the array
            self.storage.truncate(new_size);
        } else if new_size > current_len {
            // Create a new array of the target size
            let fs_cons = FSharpCons::extract(cons, self.storage.get_type());
            let mut new_storage = fs_cons.create(new_size);

            // Copy existing elements from current storage to new storage
            for i in 0..current_len {
                new_storage.push_from_storage(&self.storage, i, py);
            }

            // Fill remaining space with zero value
            for _ in current_len..new_size {
                new_storage.push_value(&zero_value, zero_value.py())?;
            }

            // Replace the old storage with the new one
            self.storage = new_storage;
        }

        Ok(())
    }

    pub fn fold_back_indexed(
        &self,
        py: Python<'_>,
        folder: &Bound<'_, PyAny>,
        state: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        let len = self.storage.len();
        let mut acc = state.clone(); // Updated to use clone()

        for i in (0..len).rev() {
            let item = self.get_item_at_index(i as isize, py)?;
            acc = folder.call1((i, acc, item))?;
        }

        Ok(acc.into())
    }

    pub fn sort_in_place(&mut self, py: Python<'_>, comparer: &Bound<'_, PyAny>) -> PyResult<()> {
        // Extract the Compare method from the comparer
        let compare_func = comparer.getattr("Compare")?;
        self.sort_in_place_with(py, &compare_func)
    }

    pub fn sort_in_place_by(
        &mut self,
        py: Python<'_>,
        projection: &Bound<'_, PyAny>,
        comparer: &Bound<'_, PyAny>,
    ) -> PyResult<()> {
        // Create a comparison helper that applies projection before comparing
        let compare_func =
            ProjectionComparer::new(projection.clone().into(), comparer.clone().into())?;
        let compare_func_obj = compare_func.into_pyobject(py)?;
        self.sort_in_place_with(py, &compare_func_obj)
    }

    pub fn sort(&self, py: Python<'_>, comparer: &Bound<'_, PyAny>) -> PyResult<FSharpArray> {
        let mut result = self.clone();
        let compare_func = comparer.getattr("Compare")?;
        result.sort_in_place_with(py, &compare_func)?;
        Ok(result)
    }

    pub fn sort_in_place_with(
        &mut self,
        _py: Python<'_>,
        compare_func: &Bound<'_, PyAny>,
    ) -> PyResult<()> {
        self.storage.sort_in_place_with(compare_func)
    }

    pub fn equals_with(
        &self,
        py: Python<'_>,
        equals_func: &Bound<'_, PyAny>,
        other: &Bound<'_, PyAny>,
    ) -> PyResult<bool> {
        // Check if the other object is a FSharpArray
        if let Ok(other_array) = other.extract::<PyRef<'_, FSharpArray>>() {
            // Compare lengths first
            if self.storage.len() != other_array.storage.len() {
                return Ok(false);
            }

            // Compare elements using the provided equals function
            for i in 0..self.storage.len() {
                let item1 = self.get_item_at_index(i as isize, py)?;
                let item2 = other_array.get_item_at_index(i as isize, py)?;

                let result = equals_func.call1((item1, item2))?;
                if !result.is_truthy()? {
                    return Ok(false);
                }
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn reduce(&self, py: Python<'_>, reduction: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        reduce_impl(self, py, |acc, item, _py| {
            reduction.call1((acc, item)).map(|o| o.into())
        })
    }

    pub fn reduce_back(&self, py: Python<'_>, reduction: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let len = self.storage.len();
        if len == 0 {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "Cannot reduce an empty array.",
            ));
        }

        // Initialize the accumulator with the last element
        let mut acc = self.get_item_at_index(len as isize - 1, py)?;

        for i in (0..len - 1).rev() {
            let item = self.get_item_at_index(i as isize, py)?;
            acc = reduction.call1((item, acc))?.into();
        }

        Ok(acc)
    }

    pub fn fold_back_indexed2(
        &self,
        py: Python<'_>,
        folder: &Bound<'_, PyAny>,
        other: &Bound<'_, PyAny>,
        state: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        // Check if the other object is a FSharpArray
        if let Ok(other_array) = other.extract::<PyRef<'_, FSharpArray>>() {
            // Compare lengths first
            if self.storage.len() != other_array.storage.len() {
                return Err(PyErr::new::<exceptions::PyValueError, _>(
                    "Arrays must have the same length.",
                ));
            }

            let len = self.storage.len();
            let mut acc = state.clone();

            for i in (0..len).rev() {
                let item1 = self.get_item_at_index(i as isize, py)?;
                let item2 = other_array.get_item_at_index(i as isize, py)?;
                acc = folder.call1((i, item1, item2, acc))?;
            }

            Ok(acc.into())
        } else {
            Err(PyErr::new::<exceptions::PyTypeError, _>(
                "The second argument must be a FSharpArray.",
            ))
        }
    }

    pub fn fold_back2(
        &self,
        py: Python<'_>,
        f: &Bound<'_, PyAny>,
        other: &Bound<'_, PyAny>,
        state: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        // Check if the other object is a FSharpArray
        if let Ok(other_array) = other.extract::<PyRef<'_, FSharpArray>>() {
            // Compare lengths first
            if self.storage.len() != other_array.storage.len() {
                return Err(PyErr::new::<exceptions::PyValueError, _>(
                    "Arrays must have the same length.",
                ));
            }

            let len = self.storage.len();
            let mut acc = state.clone();

            for i in (0..len).rev() {
                let item1 = self.get_item_at_index(i as isize, py)?;
                let item2 = other_array.get_item_at_index(i as isize, py)?;
                acc = f.call1((item1, item2, acc))?;
            }

            Ok(acc.into())
        } else {
            Err(PyErr::new::<exceptions::PyTypeError, _>(
                "The second argument must be a FSharpArray.",
            ))
        }
    }

    pub fn iterate(&self, py: Python<'_>, action: &Bound<'_, PyAny>) -> PyResult<()> {
        let len = self.storage.len();
        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            action.call1((item,))?;
        }
        Ok(())
    }
    pub fn iterate_indexed(&self, py: Python<'_>, action: &Bound<'_, PyAny>) -> PyResult<()> {
        let len = self.storage.len();
        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            action.call1((i, item))?;
        }
        Ok(())
    }

    pub fn sum(&self, py: Python<'_>, adder: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let len = self.storage.len();
        let mut acc = adder.call_method0("GetZero")?;

        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            acc = adder.call_method1("Add", (acc, item))?;
        }

        Ok(acc.into())
    }

    pub fn average(&self, py: Python<'_>, averager: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let len = self.storage.len();
        if len == 0 {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "The input array was empty",
            ));
        }

        let mut acc = averager.call_method0("GetZero")?;

        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            acc = averager.call_method1("Add", (acc, item))?;
        }

        let result = averager.call_method1("DivideByInt", (acc, len))?;
        Ok(result.into())
    }

    pub fn average_by(
        &self,
        py: Python<'_>,
        projection: &Bound<'_, PyAny>,
        averager: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        let len = self.storage.len();
        if len == 0 {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "The input array was empty",
            ));
        }

        let mut acc = averager.call_method0("GetZero")?;

        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            let projected_item = projection.call1((item,))?;
            acc = averager.call_method1("Add", (acc, projected_item))?;
        }

        let result = averager.call_method1("DivideByInt", (acc, len))?;
        Ok(result.into())
    }

    pub fn pairwise(&self, py: Python<'_>) -> PyResult<FSharpArray> {
        let len = self.storage.len();
        if len < 2 {
            return FSharpArray::empty(py, None);
        }

        let count = len - 1;
        let builder = NativeArray::new(&ArrayType::Generic, Some(count));
        let mut result = FSharpArray { storage: builder };

        for i in 0..count {
            let item1 = self.get_item_at_index(i as isize, py)?;
            let item2 = self.get_item_at_index((i + 1) as isize, py)?;
            let tuple = PyTuple::new(py, &[item1, item2])?;
            result.storage.push_value(&tuple, py)?;
        }

        Ok(result)
    }

    pub fn permute(&self, py: Python<'_>, f: &Bound<'_, PyAny>) -> PyResult<Py<FSharpArray>> {
        let len = self.storage.len();
        let mut check_flags = vec![0; len];
        let mut temp = Vec::with_capacity(len);

        // First pass: calculate new positions and validate
        for i in 0..len {
            let new_index = f.call1((i,))?.extract::<isize>()?;

            if new_index < 0 || new_index >= len as isize {
                return Err(PyErr::new::<exceptions::PyValueError, _>(
                    "Not a valid permutation",
                ));
            }

            let new_index = new_index as usize;
            check_flags[new_index] = 1;
            temp.push((new_index, self.get_item_at_index(i as isize, py)?));
        }

        // Verify that all positions were used exactly once
        if !check_flags.iter().all(|&x| x == 1) {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "Not a valid permutation",
            ));
        }

        // Sort by new index to get elements in correct order
        temp.sort_by_key(|&(i, _)| i);

        // Create result array with elements in permuted order
        let builder = NativeArray::new(self.storage.get_type(), Some(len));
        let mut result = FSharpArray { storage: builder };
        for (_, item) in temp {
            result.storage.push_value(item.bind(py), py)?;
        }

        Ok(result.into_pyobject(py)?.into())
    }

    pub fn scan(
        &self,
        py: Python<'_>,
        folder: &Bound<'_, PyAny>,
        state: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<Py<FSharpArray>> {
        let len = self.storage.len();
        let fs_cons = FSharpCons::extract(cons, self.storage.get_type());
        let mut results = fs_cons.create(len + 1);

        // Start with initial state
        let mut current_state = state.clone();
        results.push_value(&current_state, py)?;

        // Process each element, updating the state as we go
        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            current_state = folder.call1((current_state, item))?;
            results.push_value(&current_state, py)?;
        }

        Ok(FSharpArray { storage: results }.into_pyobject(py)?.into())
    }

    pub fn scan_back(
        &self,
        py: Python<'_>,
        folder: &Bound<'_, PyAny>,
        state: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<Py<FSharpArray>> {
        let len = self.storage.len();
        let fs_cons = FSharpCons::extract(cons, self.storage.get_type());
        let mut results = fs_cons.create(len + 1);

        // Start with initial state
        let mut current_state = state.clone();

        // Process elements from right to left, building states
        let mut states = Vec::with_capacity(len + 1);
        states.push(current_state.clone());

        for i in (0..len).rev() {
            let x = self.get_item_at_index(i as isize, py)?;
            // F# signature: 'T -> 'State -> 'State, so element comes first
            current_state = folder.call1((x, current_state))?;
            states.push(current_state.clone());
        }

        // Reverse the states to get the correct order and add them to results
        for state in states.into_iter().rev() {
            results.push_value(&state, py)?;
        }

        Ok(FSharpArray { storage: results }.into_pyobject(py)?.into())
    }

    pub fn split_into(&self, py: Python<'_>, chunks: usize) -> PyResult<FSharpArray> {
        if chunks < 1 {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "The input must be positive.",
            ));
        }

        let len = self.storage.len();
        if len == 0 {
            // Return an empty array
            return Ok(FSharpArray {
                storage: NativeArray::new(self.storage.get_type(), Some(0)),
            });
        }

        // Create array to hold chunks
        let mut results = NativeArray::new(&ArrayType::Generic, Some(chunks));

        // Calculate chunk sizes
        let chunks = std::cmp::min(chunks, len);
        let min_chunk_size = len / chunks;
        let chunks_with_extra_item = len % chunks;

        // Create each chunk
        for i in 0..chunks {
            let chunk_size = if i < chunks_with_extra_item {
                min_chunk_size + 1
            } else {
                min_chunk_size
            };

            let start = i * min_chunk_size + std::cmp::min(chunks_with_extra_item, i);

            // Create a new array for this chunk
            let mut chunk = NativeArray::new(self.storage.get_type(), Some(chunk_size));

            // Copy elements for this chunk
            for j in 0..chunk_size {
                chunk.push_from_storage(&self.storage, start + j, py);
            }

            // Add the chunk to results
            let chunk_array = FSharpArray { storage: chunk };
            results.push_value(&chunk_array.into_pyobject(py)?.into_any(), py)?;
        }

        // Construct the result array
        Ok(FSharpArray { storage: results })
    }

    pub fn truncate(&self, py: Python<'_>, count: isize) -> PyResult<FSharpArray> {
        // Handle negative count by treating it as 0 (F# behavior: max 0 count)
        let count = if count < 0 { 0 } else { count as usize };

        // F# truncate uses subArrayImpl which doesn't error if count > length
        // It just takes what's available, so we use min(count, len)
        let actual_count = std::cmp::min(count, self.storage.len());

        // Use get_sub_array to get elements from 0 to actual_count
        self.get_sub_array(py, 0, actual_count, None)
    }

    #[pyo3(signature = (f, cons=None))]
    pub fn partition(
        &self,
        py: Python<'_>,
        f: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let len = self.storage.len();
        let fs_cons = FSharpCons::extract(cons, self.storage.get_type());
        // Don't pre-allocate full size - just let vectors grow as needed
        let mut results = fs_cons.create(0);
        let mut results2 = fs_cons.create(0);

        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            // Bind once and reuse
            let bound_item = item.bind(py);
            if f.call1((&bound_item,))?.is_truthy()? {
                results.push_value(bound_item, py)?;
            } else {
                results2.push_value(bound_item, py)?;
            }
        }

        // Arrays already have correct sizes from push_value calls
        let left = FSharpArray { storage: results };
        let right = FSharpArray { storage: results2 };

        // Create a new array containing the left and right results
        let mut final_results = NativeArray::new(&ArrayType::Generic, Some(2));
        final_results.push_value(Py::new(py, left)?.bind(py), py)?;
        final_results.push_value(Py::new(py, right)?.bind(py), py)?;
        Ok(FSharpArray {
            storage: final_results,
        })
    }

    pub fn transpose(
        &self,
        py: Python<'_>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let len = self.storage.len();
        if len == 0 {
            return Ok(FSharpArray {
                storage: NativeArray::PyObject(Arc::new(Mutex::new(vec![]))),
            });
        }

        let first_array = self.get_item_at_index(0, py)?;
        let len_inner = first_array.bind(py).len()?;

        // Check if all inner arrays have the same length
        for i in 1..len {
            let item = self.get_item_at_index(i as isize, py)?;
            if item.bind(py).len()? != len_inner {
                return Err(PyErr::new::<exceptions::PyValueError, _>(
                    "All inner arrays must have the same length.",
                ));
            }
        }

        // Create the result array
        let mut results = NativeArray::new(&ArrayType::Generic, Some(len_inner));

        // Fill the result array
        let fs_cons = FSharpCons::extract(cons, &ArrayType::Generic);
        for i in 0..len_inner {
            let mut inner_array = fs_cons.create(len);
            for j in 0..len {
                let outer_array = self.get_item_at_index(j as isize, py)?;
                let inner_item = outer_array.bind(py).get_item(i)?;
                inner_array.push_value(&inner_item, py)?;
            }
            let inner_array = FSharpArray {
                storage: inner_array,
            };
            results.push_value(Py::new(py, inner_array)?.bind(py), py)?;
        }

        Ok(FSharpArray { storage: results })
    }

    pub fn try_find_back(
        &self,
        py: Python<'_>,
        predicate: &Bound<'_, PyAny>,
    ) -> PyResult<Option<Py<PyAny>>> {
        let len = self.storage.len();
        for i in (0..len).rev() {
            let item = self.get_item_at_index(i as isize, py)?;
            if predicate.call1((&item,))?.is_truthy()? {
                // Since Rust options are erased when converted to Python, we need to
                // wrap None in an extra SomeWrapper
                if item.is_none(py) {
                    return Ok(Some(SomeWrapper::new(item).into_py_any(py)?));
                }
                return Ok(Some(item));
            }
        }
        Ok(None)
    }

    pub fn is_empty(&self) -> bool {
        self.storage.len() == 0
    }

    pub fn try_find_index_back(
        &self,
        py: Python<'_>,
        predicate: &Bound<'_, PyAny>,
    ) -> PyResult<Option<usize>> {
        if self.is_empty() {
            return Ok(None);
        }
        for i in (0..self.storage.len()).rev() {
            let item = self.get_item_at_index(i as isize, py)?;
            if predicate.call1((item,))?.is_truthy()? {
                return Ok(Some(i));
            }
        }
        Ok(None)
    }

    pub fn find_index_back(&self, py: Python<'_>, predicate: &Bound<'_, PyAny>) -> PyResult<usize> {
        match self.try_find_index_back(py, predicate)? {
            Some(index) => Ok(index),
            None => Err(pyo3::exceptions::PyValueError::new_err(
                "No element matches the predicate",
            )),
        }
    }

    pub fn try_find_index(
        &self,
        py: Python<'_>,
        predicate: &Bound<'_, PyAny>,
    ) -> PyResult<Option<usize>> {
        let len = self.storage.len();
        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            if predicate.call1((item,))?.is_truthy()? {
                return Ok(Some(i));
            }
        }
        Ok(None)
    }

    pub fn windowed(&self, py: Python<'_>, window_size: usize) -> PyResult<FSharpArray> {
        if window_size == 0 {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "windowSize must be positive.",
            ));
        }

        let len = self.storage.len();
        let count = std::cmp::max(0, len as isize - window_size as isize + 1) as usize;

        // Create an array of arrays
        let builder = NativeArray::new(&ArrayType::Generic, Some(count));
        let mut result = FSharpArray { storage: builder };

        for i in 0..count {
            // For each window position, create a new array containing window_size elements
            let fs_cons = FSharpCons::new(
                &self
                    .storage
                    .get_type()
                    .clone()
                    .into_pyobject(py)?
                    .extract::<String>()?,
            )?;
            let mut window = fs_cons.allocate(py, window_size)?;

            // Fill the window with elements from the source array
            for j in 0..window_size {
                let item = self.get_item_at_index((i + j) as isize, py)?;
                window.storage.push_value(item.bind(py), py)?;
            }

            // Add the window to the result array
            result
                .storage
                .push_value(Py::new(py, window)?.bind(py), py)?;
        }

        Ok(result)
    }

    #[pyo3(signature = (mapping, state, cons=None))]
    pub fn map_fold(
        &self,
        py: Python<'_>,
        mapping: &Bound<'_, PyAny>,
        state: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<Py<PyAny>> {
        let len = self.storage.len();

        // Get the constructor or use default
        let fs_cons = FSharpCons::extract(cons, self.storage.get_type());

        if len == 0 {
            // Return empty array and original state
            let empty_array = fs_cons.create(0);
            let result_array = FSharpArray {
                storage: empty_array,
            };
            let result_tuple =
                PyTuple::new(py, [Py::new(py, result_array)?.bind(py), &state.clone()]);
            return Ok(result_tuple?.into());
        }

        let mut result_array_builder = fs_cons.create(len);
        let mut current_state = state.clone();

        // Process each element
        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;

            // Call the mapping function: mapping(state, item) -> (mapped_item, new_state)
            let result = mapping.call1((current_state, item))?;

            // Get the first item (mapped result) and second item (new state)
            let mapped_item = result.get_item(0)?;
            current_state = result.get_item(1)?;

            // Push the mapped item in the result array
            result_array_builder.push_value(&mapped_item, py)?;
        }

        let result_array = FSharpArray {
            storage: result_array_builder,
        };

        // Return tuple of (result_array, final_state)
        let result_tuple = PyTuple::new(py, [Py::new(py, result_array)?.bind(py), &current_state]);
        Ok(result_tuple?.into())
    }

    #[pyo3(signature = (mapping, state, cons=None))]
    pub fn map_fold_back(
        &self,
        py: Python<'_>,
        mapping: &Bound<'_, PyAny>,
        state: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<Py<PyAny>> {
        let len = self.storage.len();

        // Get the constructor or use default
        let fs_cons = FSharpCons::extract(cons, self.storage.get_type());

        if len == 0 {
            // Return empty array and original state
            let empty_array = fs_cons.create(0);
            let result_array = FSharpArray {
                storage: empty_array,
            };
            let result_tuple =
                PyTuple::new(py, [Py::new(py, result_array)?.bind(py), &state.clone()]);
            return Ok(result_tuple?.into());
        }

        let mut result_array_builder = fs_cons.create(len);
        let mut current_state = state.clone();

        // Process each element in reverse order
        for i in (0..len).rev() {
            let item = self.get_item_at_index(i as isize, py)?;

            // Call the mapping function: mapping(item, state) -> (mapped_item, new_state)
            let result = mapping.call1((item, current_state))?;

            // Get the first item (mapped result) and second item (new state)
            let mapped_item = result.get_item(0)?;
            current_state = result.get_item(1)?;

            // Push the mapped item in the result array
            result_array_builder.push_value(&mapped_item, py)?;
        }

        let result_array = FSharpArray {
            storage: result_array_builder,
        };

        // Return tuple of (result_array, final_state)
        let result_tuple = PyTuple::new(py, [Py::new(py, result_array)?.bind(py), &current_state]);
        Ok(result_tuple?.into())
    }

    // Get the first element of the array
    pub fn head(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        if self.storage.len() == 0 {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "The input array was empty",
            ));
        }
        self.get_item_at_index(0, py)
    }

    // Try to get the first element, returning None if array is empty
    pub fn try_head(&self, py: Python<'_>) -> PyResult<Option<Py<PyAny>>> {
        if self.storage.len() == 0 {
            Ok(None)
        } else {
            Ok(Some(self.get_item_at_index(0, py)?))
        }
    }

    // Return a new array with all elements except the first one
    pub fn tail(&self, py: Python<'_>, cons: Option<&Bound<'_, PyAny>>) -> PyResult<FSharpArray> {
        if self.storage.len() == 0 {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "Not enough elements",
            ));
        }

        // Reuse skip implementation to skip the first element
        self.skip(py, 1, cons)
    }

    // Get an item at a specific index
    pub fn item(&self, py: Python<'_>, index: isize) -> PyResult<Py<PyAny>> {
        self.get_item_at_index(index, py)
    }

    // Try to get an item at a specific index, returning None if out of bounds
    pub fn try_item(&self, py: Python<'_>, index: isize) -> PyResult<Option<Py<PyAny>>> {
        let len = self.storage.len();

        // Simple bounds check matching F# implementation
        if index < 0 || index >= len as isize {
            Ok(None)
        } else {
            Ok(Some(self.get_item_at_index(index, py)?))
        }
    }

    // Then simplify the FSharpArray method
    pub fn reverse(&self, py: Python<'_>) -> PyResult<FSharpArray> {
        // Use the helper method from ArrayStorage
        let reversed_storage = NativeArray::reverse_storage(&self.storage, py);

        Ok(FSharpArray {
            storage: reversed_storage,
        })
    }

    pub fn compare_with(
        &self,
        py: Python<'_>,
        comparer: &Bound<'_, PyAny>,
        other: &FSharpArray,
    ) -> PyResult<isize> {
        let len1 = self.storage.len();
        let len2 = other.storage.len();
        let min_len = std::cmp::min(len1, len2);

        // Compare elements one by one up to the minimum length
        for i in 0..min_len {
            let item1 = self.get_item_at_index(i as isize, py)?;
            let item2 = other.get_item_at_index(i as isize, py)?;

            let res = comparer.call1((item1, item2))?;
            let res: isize = res.extract()?;
            if res != 0 {
                return Ok(res);
            }
        }
        // If all compared elements are equal, compare lengths
        if len1 < len2 {
            Ok(-1)
        } else if len1 > len2 {
            Ok(1)
        } else {
            Ok(0)
        }
    }

    pub fn exists_offset(
        &self,
        py: Python<'_>,
        predicate: &Bound<'_, PyAny>,
        index: usize,
    ) -> PyResult<bool> {
        let len = self.storage.len();
        // Iteratively check from index to end of array
        for i in index..len {
            let item = self.get_item_at_index(i as isize, py)?;
            if predicate.call1((item,))?.is_truthy()? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub fn exists(&self, py: Python<'_>, predicate: &Bound<'_, PyAny>) -> PyResult<bool> {
        let len = self.storage.len();
        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            if predicate.call1((item,))?.is_truthy()? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub fn update_at(
        &self,
        py: Python<'_>,
        index: usize,
        value: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let len = self.storage.len();
        if index >= len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "Index out of bounds",
            ));
        }

        // Create a new array using the constructor
        let fs_cons = FSharpCons::extract(cons, self.storage.get_type());
        let mut target = fs_cons.create(len);

        // Fill the new array with values from the original array
        for i in 0..len {
            if i == index {
                target.push_value(value, py)?;
            } else {
                let item = self.get_item_at_index(i as isize, py)?;
                target.push_value(item.bind(py), py)?;
            }
        }

        Ok(FSharpArray { storage: target })
    }

    pub fn set_slice(
        &self, // self is the source array
        py: Python<'_>,
        target: &mut FSharpArray, // Target array to copy to
        lower: Option<usize>,     // Starting index in target
        upper: Option<usize>,     // Upper bound in target
    ) -> PyResult<()> {
        let lower = lower.unwrap_or(0);
        let upper = upper.unwrap_or(0);

        // Calculate length to copy (following F# logic)
        let length = if upper > 0 {
            upper
        } else {
            target.storage.len() - 1
        } - lower;

        // Check if target has enough elements
        // println!("Target length: {}", target.storage.len());
        // println!("Length to copy: {}", length);
        if target.storage.len() < length {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "Target array is not large enough to hold the copied elements",
            ));
        }

        // Copy elements from source (self) to target
        for i in 0..=length {
            let item = self.get_item_at_index(i as isize, py)?;
            target.__setitem__((i + lower) as isize, item.bind(py), py)?;
        }

        Ok(())
    }

    // Format array as F# style string: [|1; 2; 3|] or [|1; 2; 3; ... |] for longer arrays
    pub fn __str__(&self, py: Python<'_>) -> PyResult<String> {
        // Define max elements to show before truncating
        const MAX_DISPLAY_ELEMENTS: usize = 10;

        let len = self.storage.len();

        // Empty array case
        if len == 0 {
            return Ok("[||]".to_string());
        }

        let show_ellipsis = len > MAX_DISPLAY_ELEMENTS;
        let elements_to_show = if show_ellipsis {
            MAX_DISPLAY_ELEMENTS
        } else {
            len
        };

        let mut result = String::from("[|");

        // Add the elements to display
        for i in 0..elements_to_show {
            let item = self.get_item_at_index(i as isize, py)?;
            let item_str = item.bind(py).str()?.extract::<String>()?;

            result.push_str(&item_str);

            // Add separator if not the last element
            if i < elements_to_show - 1 || show_ellipsis {
                result.push_str("; ");
            }
        }

        // Add ellipsis for longer arrays
        if show_ellipsis {
            result.push_str("... ");
        }

        result.push_str("|]");
        Ok(result)
    }

    // Provide a repr that wraps the str representation in single quotes
    pub fn __repr__(&self, py: Python<'_>) -> PyResult<String> {
        let storage_type = match &self.storage {
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
            NativeArray::PyObject(_) => "PyObject",
        };
        let contents = self.__str__(py)?;
        Ok(format!("{}({})", storage_type, contents))
    }

    pub fn insert_at(
        &self,
        py: Python<'_>,
        index: usize,
        value: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let len = self.storage.len();
        if index > len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "Index out of bounds",
            ));
        }

        // Create a new array using the constructor
        let fs_cons = FSharpCons::extract(cons, self.storage.get_type());
        let mut target = fs_cons.allocate(py, len + 1)?;

        // Copy elements before the insertion point
        for i in 0..index {
            target.storage.push_from_storage(&self.storage, i, py);
        }

        // Insert the new value
        target.storage.push_value(value, py)?;

        // Copy elements after the insertion point
        for i in index..len {
            target.storage.push_from_storage(&self.storage, i, py);
        }

        Ok(target)
    }

    pub fn insert_many_at(
        &self,
        py: Python<'_>,
        index: usize,
        ys: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let len = self.storage.len();

        if index > len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "Index out of bounds",
            ));
        }

        // Convert ys to an iterator to get the elements to insert
        let ys_iter = ys.try_iter()?;

        // Collect the items to insert into a Vec to know the count
        let mut items_to_insert = Vec::new();
        for item in ys_iter {
            items_to_insert.push(item?);
        }

        let insert_count = items_to_insert.len();

        // Create a new array using the constructor with proper capacity
        let fs_cons = FSharpCons::extract(cons, self.storage.get_type());
        let mut target = fs_cons.allocate(py, len + insert_count)?;

        // Copy elements before the insertion point
        if index > 0 {
            for i in 0..index {
                target.storage.push_from_storage(&self.storage, i, py);
            }
        }

        // Insert all the new values from ys at the correct position
        for item in items_to_insert {
            target.storage.push_value(&item, py)?;
        }

        // Copy elements after the insertion point
        if index < len {
            for i in index..len {
                target.storage.push_from_storage(&self.storage, i, py);
            }
        }

        Ok(target)
    }

    pub fn map3(
        &self,
        py: Python<'_>,
        f: &Bound<'_, PyAny>,
        source2: &FSharpArray,
        source3: &FSharpArray,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let len = self.storage.len();

        // Check lengths match
        if len != source2.storage.len() || source2.storage.len() != source3.storage.len() {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "Arrays had different lengths",
            ));
        }

        // Get constructor from cons parameter
        let fs_cons = FSharpCons::extract(cons, self.storage.get_type());
        let mut results = fs_cons.create(len);

        // Map each element with counterparts in source2 and source3
        for i in 0..len {
            let item1 = self.get_item_at_index(i as isize, py)?;
            let item2 = source2.get_item_at_index(i as isize, py)?;
            let item3 = source3.get_item_at_index(i as isize, py)?;
            let mapped = f.call1((item1, item2, item3))?;
            results.push_value(&mapped, py)?;
        }

        // Return final array
        Ok(FSharpArray { storage: results })
    }

    pub fn map_indexed2(
        &self,
        py: Python<'_>,
        f: &Bound<'_, PyAny>,
        source2: &FSharpArray,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let len = self.storage.len();

        // Check lengths match
        if len != source2.storage.len() {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "Arrays had different lengths",
            ));
        }

        // Get constructor from cons parameter
        let fs_cons = FSharpCons::extract(cons, self.storage.get_type());
        let mut results = fs_cons.create(len);

        // Map each element with its index and counterpart in source2
        for i in 0..len {
            let item1 = self.get_item_at_index(i as isize, py)?;
            let item2 = source2.get_item_at_index(i as isize, py)?;
            let mapped = f.call1((i, item1, item2))?;
            results.push_value(&mapped, py)?;
        }

        // Return final array
        Ok(FSharpArray { storage: results })
    }

    #[pyo3(signature = (array2, cons=None))]
    pub fn append(
        &self,
        py: Python<'_>,
        array2: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let array2 = ensure_array(py, array2)?;

        // Verify that both arrays have the same type
        if self.storage.get_type() != array2.storage.get_type() {
            return Err(PyErr::new::<exceptions::PyValueError, _>(format!(
                "Cannot append arrays of different types: {:?} and {:?}",
                self.storage.get_type(),
                array2.storage.get_type()
            )));
        }

        let len1 = self.storage.len();
        let len2 = array2.storage.len();

        // Get constructor from cons parameter or use default
        let fs_cons = FSharpCons::extract(cons, self.storage.get_type());
        let mut builder = fs_cons.create(len1 + len2);

        // Copy elements from first array
        for i in 0..len1 {
            builder.push_from_storage(&self.storage, i, py);
        }

        // Copy elements from second array
        for i in 0..len2 {
            builder.push_from_storage(&array2.storage, i, py);
        }

        // Create the final array
        Ok(FSharpArray { storage: builder })
    }

    pub fn map_indexed3(
        &self,
        py: Python<'_>,
        f: &Bound<'_, PyAny>,
        source2: &FSharpArray,
        source3: &FSharpArray,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let len = self.storage.len();

        // Check lengths match
        if len != source2.storage.len() || source2.storage.len() != source3.storage.len() {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "Arrays had different lengths",
            ));
        }

        // Get constructor from cons parameter
        let fs_cons = FSharpCons::extract(cons, self.storage.get_type());
        let mut results = fs_cons.create(len);

        // Map each element with its index and counterparts in source2 and source3
        for i in 0..len {
            let item1 = self.get_item_at_index(i as isize, py)?;
            let item2 = source2.get_item_at_index(i as isize, py)?;
            let item3 = source3.get_item_at_index(i as isize, py)?;
            let mapped = f.call1((i, item1, item2, item3))?;
            results.push_value(&mapped, py)?;
        }

        // Return final array
        Ok(FSharpArray { storage: results })
    }

    pub fn index_of(
        &self,
        py: Python<'_>,
        item: &Bound<'_, PyAny>,
        start: Option<usize>,
        count: Option<usize>,
        eq: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<isize> {
        let start = start.unwrap_or(0);
        let end_idx = count
            .map(|c| start + c)
            .unwrap_or_else(|| self.storage.len());

        for i in start..end_idx {
            let current = self.get_item_at_index(i as isize, py)?;
            let is_equal = if let Some(eq) = eq {
                eq.call_method1("Equals", (current.bind(py), item))?
                    .is_truthy()?
            } else {
                current
                    .bind(py)
                    .rich_compare(item, CompareOp::Eq)?
                    .is_truthy()?
            };
            if is_equal {
                return Ok(i as isize);
            }
        }
        Ok(-1)
    }

    pub fn remove_in_place(&mut self, py: Python<'_>, item: &Bound<'_, PyAny>) -> PyResult<bool> {
        // Find the index of the item to remove using indexOf
        // Pass None for eq to use Python's default equality comparison
        let index = self.index_of(py, item, None, None, None)?;
        if index == -1 {
            return Ok(false);
        }
        self.remove_at(py, index)?;
        Ok(true)
    }

    pub fn copy_to(
        &self,
        py: Python<'_>,
        source_index: usize,
        target: &mut FSharpArray,
        target_index: usize,
        count: usize,
    ) -> PyResult<()> {
        // Try the fast path first - direct storage copy for same types
        match self
            .storage
            .copy_to(&mut target.storage, source_index, target_index, count, py)
        {
            Ok(()) => Ok(()),
            Err(_) => {
                // Fallback: manual copying with type conversion
                // This handles cases where source and target have different types
                for i in 0..count {
                    let source_item = self.get_item_at_index((source_index + i) as isize, py)?;
                    target.__setitem__((target_index + i) as isize, source_item.bind(py), py)?;
                }
                Ok(())
            }
        }
    }

    pub fn zip(&self, py: Python<'_>, array2: &Bound<'_, PyAny>) -> PyResult<FSharpArray> {
        let array2 = ensure_array(py, array2)?;

        if self.__len__() != array2.__len__() {
            return Err(PyErr::new::<pyo3::exceptions::PyValueError, _>(
                "Arrays had different lengths",
            ));
        }
        let mut result = FSharpArray::new(py, None, None)?;
        for i in 0..self.__len__() {
            let item1 = self.get_item_at_index(i as isize, py)?;
            let item2 = array2.get_item_at_index(i as isize, py)?;
            let tuple = PyTuple::new(py, &[item1, item2])?;
            result.storage.push_value(&tuple, py)?;
        }
        Ok(result)
    }

    pub fn for_all(&self, py: Python<'_>, predicate: &Bound<'_, PyAny>) -> PyResult<bool> {
        for i in 0..self.__len__() {
            let item = self.get_item_at_index(i as isize, py)?;
            let result = predicate.call1((item,))?.extract::<bool>()?;
            if !result {
                return Ok(false);
            }
        }
        Ok(true)
    }

    pub fn find(&self, py: Python<'_>, predicate: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        match self.try_find(py, predicate)? {
            Some(item) => Ok(item),
            None => Err(PyErr::new::<pyo3::exceptions::PyValueError, _>(
                "An element satisfying the predicate was not found in the collection.",
            )),
        }
    }

    pub fn try_find(
        &self,
        py: Python<'_>,
        predicate: &Bound<'_, PyAny>,
    ) -> PyResult<Option<Py<PyAny>>> {
        for i in 0..self.__len__() {
            let item = self.get_item_at_index(i as isize, py)?;
            let result = predicate.call1((item.clone_ref(py),))?.extract::<bool>()?;
            if result {
                return Ok(Some(item));
            }
        }
        Ok(None)
    }

    pub fn find_last_index(&self, py: Python<'_>, predicate: &Bound<'_, PyAny>) -> PyResult<usize> {
        match self.try_find_index_back(py, predicate)? {
            Some(index) => Ok(index),
            None => Err(PyErr::new::<pyo3::exceptions::PyValueError, _>(
                "An element satisfying the predicate was not found in the collection.",
            )),
        }
    }

    pub fn add_in_place(&mut self, py: Python<'_>, value: &Bound<'_, PyAny>) -> PyResult<()> {
        self.storage.push_value(value, py)
    }

    pub fn add_range_in_place(&mut self, py: Python<'_>, range: &Bound<'_, PyAny>) -> PyResult<()> {
        // Convert the range to an iterator
        let iter = range.try_iter()?;

        // Add each element from the range to the array
        for item in iter {
            let item = item?;
            self.add_in_place(py, &item)?;
        }

        Ok(())
    }

    pub fn insert_range_in_place(
        &mut self,
        py: Python<'_>,
        index: isize,
        range: &Bound<'_, PyAny>,
    ) -> PyResult<()> {
        let len = self.storage.len();
        let index = if index < 0 {
            len as isize + index
        } else {
            index
        };

        if index < 0 || index as usize > len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "index out of range",
            ));
        }

        // Convert the range to an iterator
        let iter = range.try_iter()?;
        let mut current_index = index;

        // Insert each element from the range at the current index
        for item in iter {
            let item = item?;
            self.storage.insert(current_index as usize, &item, py)?;
            current_index += 1;
        }

        Ok(())
    }

    pub fn get_sub_array(
        &self,
        py: Python<'_>,
        start_index: isize,
        count: usize,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let len = self.storage.len();
        let start_index = if start_index < 0 {
            len as isize + start_index
        } else {
            start_index
        };

        if start_index < 0 || start_index as usize >= len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "start_index out of range",
            ));
        }

        if count > len - start_index as usize {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "count exceeds array bounds",
            ));
        }

        // Get constructor from cons parameter or use default
        let fs_cons = FSharpCons::extract(cons, self.storage.get_type());
        let mut builder = fs_cons.create(count);

        // Copy elements from source array to new array
        for i in 0..count {
            builder.push_from_storage(&self.storage, start_index as usize + i, py);
        }

        Ok(FSharpArray { storage: builder })
    }

    pub fn contains(
        &self,
        py: Python<'_>,
        value: &Bound<'_, PyAny>,
        eq: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<bool> {
        let len = self.storage.len();
        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            let is_equal = if let Some(eq) = eq {
                eq.call_method1("Equals", (item.bind(py), value))?
                    .is_truthy()?
            } else {
                item.bind(py)
                    .rich_compare(value, CompareOp::Eq)?
                    .is_truthy()?
            };
            if is_equal {
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// Creates a shallow copy of the array
    pub fn copy(&self, _py: Python<'_>) -> PyResult<FSharpArray> {
        Ok(self.clone())
    }

    // let max (xs: 'a[]) ([<Inject>] comparer: IComparer<'a>) : 'a =
    pub fn max(&self, py: Python<'_>, comparer: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        reduce_impl(self, py, |acc, item, py| {
            let comparison =
                comparer.call_method1("Compare", (acc.clone_ref(py), item.clone_ref(py)))?;
            let is_gt = comparison.extract::<i32>()? > 0;
            Ok(if is_gt { acc } else { item })
        })
    }

    pub fn min(&self, py: Python<'_>, comparer: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        reduce_impl(self, py, |acc, item, py| {
            let comparison =
                comparer.call_method1("Compare", (acc.clone_ref(py), item.clone_ref(py)))?;
            let is_lt = comparison.extract::<i32>()? < 0;
            Ok(if is_lt { acc } else { item })
        })
    }

    /// Returns the element for which the key function returns the largest value.
    pub fn max_by(
        &self,
        py: Python<'_>,
        projection: &Bound<'_, PyAny>,
        comparer: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        reduce_impl(self, py, |acc, item, py| {
            let acc_key = projection.call1((acc.clone_ref(py),))?;
            let item_key = projection.call1((item.clone_ref(py),))?;
            let comparison = comparer.call_method1("Compare", (item_key, acc_key))?;
            let is_gt = comparison.extract::<i32>()? > 0;
            Ok(if is_gt { item } else { acc })
        })
    }

    /// Returns the element for which the key function returns the smallest value.
    pub fn min_by(
        &self,
        py: Python<'_>,
        projection: &Bound<'_, PyAny>,
        comparer: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        reduce_impl(self, py, |acc, item, py| {
            let acc_key = projection.call1((acc.clone_ref(py),))?;
            let item_key = projection.call1((item.clone_ref(py),))?;
            let comparison = comparer.call_method1("Compare", (item_key, acc_key))?;
            let is_lt = comparison.extract::<i32>()? < 0;
            Ok(if is_lt { item } else { acc })
        })
    }

    // Choose implementation (map + filter in one pass)
    #[pyo3(signature = (chooser, cons=None))]
    pub fn choose(
        &self,
        py: Python<'_>,
        chooser: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<Self> {
        let len = self.storage.len();
        let target_type = if let Some(cons) = cons {
            if let Ok(fs_cons) = cons.extract::<PyRef<'_, FSharpCons>>() {
                fs_cons.array_type.clone()
            } else {
                self.storage.get_type().clone()
            }
        } else {
            ArrayType::Generic
        };
        let mut results = NativeArray::new(&target_type, None);
        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            let chosen = chooser.call1((item.clone_ref(py),))?;
            if !chosen.is_none() {
                results.push_value(&chosen, py)?;
            }
        }
        Ok(FSharpArray { storage: results })
    }

    pub fn find_back(&self, py: Python<'_>, predicate: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let len = self.storage.len();
        for i in (0..len).rev() {
            let item = self.get_item_at_index(i as isize, py)?;
            let result = predicate.call1((item.clone_ref(py),))?.extract::<bool>()?;
            if result {
                return Ok(item);
            }
        }
        Err(PyErr::new::<pyo3::exceptions::PyValueError, _>(
            "An element satisfying the predicate was not found in the collection.",
        ))
    }

    pub fn try_pick(
        &self,
        py: Python<'_>,
        chooser: &Bound<'_, PyAny>,
    ) -> PyResult<Option<Py<PyAny>>> {
        let len = self.storage.len();
        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            let result = chooser.call1((item,))?;

            // Check if the result is not None
            if !result.is_none() {
                return Ok(Some(result.into()));
            }
        }

        Ok(None)
    }

    pub fn pick(&self, py: Python<'_>, chooser: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        match self.try_pick(py, chooser)? {
            Some(result) => Ok(result),
            None => Err(PyErr::new::<pyo3::exceptions::PyValueError, _>(
                "An index satisfying the predicate was not found in the collection.",
            )),
        }
    }

    pub fn remove_all_in_place(
        &mut self,
        py: Python<'_>,
        predicate: &Bound<'_, PyAny>,
    ) -> PyResult<usize> {
        let mut count = 0;
        let mut i = 0;
        let len = self.storage.len();

        while i < len {
            let item = self.get_item_at_index(i as isize, py)?;
            let should_remove = predicate.call1((item,))?.extract::<bool>()?;

            if should_remove {
                self.storage.remove_at_index(i);
                count += 1;
            } else {
                i += 1;
            }
        }

        Ok(count)
    }

    pub fn indexed(&self, py: Python<'_>) -> PyResult<FSharpArray> {
        let len = self.storage.len();
        let mut builder = NativeArray::new(&ArrayType::Generic, Some(len));

        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            let index = PyInt::new(py, i as i64);
            let tuple = PyTuple::new(py, &[index.into(), item])?;
            builder.push_value(&tuple, py)?;
        }

        Ok(FSharpArray { storage: builder })
    }

    pub fn try_last(&self, py: Python<'_>) -> PyResult<Option<Py<PyAny>>> {
        let len = self.storage.len();
        if len == 0 {
            Ok(None)
        } else {
            Ok(Some(self.get_item_at_index((len - 1) as isize, py)?))
        }
    }

    pub fn last(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        let len = self.storage.len();
        if len == 0 {
            Err(PyErr::new::<pyo3::exceptions::PyIndexError, _>(
                "Array is empty",
            ))
        } else {
            self.get_item_at_index((len - 1) as isize, py)
        }
    }

    pub fn find_index(&self, py: Python<'_>, predicate: &Bound<'_, PyAny>) -> PyResult<usize> {
        match self.try_find_index(py, predicate)? {
            Some(index) => Ok(index),
            None => Err(pyo3::exceptions::PyValueError::new_err(
                "No element matches the predicate",
            )),
        }
    }

    #[pyo3(signature = (projection, comparer=None))]
    pub fn sort_by(
        &self,
        py: Python<'_>,
        projection: &Bound<'_, PyAny>,
        comparer: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let mut result = self.clone();
        // Use provided comparer or create a default one
        let default_comparer;
        let comparer_ref = match comparer {
            Some(c) => c,
            None => {
                default_comparer = DefaultComparer::new()?.into_pyobject(py)?;
                &default_comparer
            }
        };
        result.storage = result
            .storage
            .sort_by_with_projection(py, projection, comparer_ref)?;
        Ok(result)
    }

    #[pyo3(signature = (comparer))]
    pub fn sort_with(&self, _py: Python<'_>, comparer: &Bound<'_, PyAny>) -> PyResult<FSharpArray> {
        let mut result = self.storage.clone();
        let result = result.sort_with(comparer)?;
        Ok(FSharpArray { storage: result })
    }

    #[pyo3(signature = (projection, adder))]
    pub fn sum_by(
        &self,
        py: Python<'_>,
        projection: &Bound<'_, PyAny>,
        adder: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        if self.is_empty() {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "The input array was empty",
            ));
        }

        let mut acc = adder.call_method0("GetZero")?;

        for i in 0..self.storage.len() {
            let item = self.storage.get(py, i)?;
            let projected = projection.call1((item,))?;
            acc = adder.call_method1("Add", (acc, projected))?;
        }

        Ok(acc.into())
    }

    pub fn unzip(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        let len = self.storage.len();

        // Create two new arrays for the results
        let mut res1 = NativeArray::new(&ArrayType::Generic, Some(len));
        let mut res2 = NativeArray::new(&ArrayType::Generic, Some(len));

        // Iterate through each element and split the tuples
        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;

            // Try to extract as a tuple
            if let Ok(tuple) = item.bind(py).cast::<PyTuple>() {
                if tuple.len() != 2 {
                    return Err(PyErr::new::<exceptions::PyValueError, _>(
                        "Expected tuples of length 2",
                    ));
                }

                let first = tuple.get_item(0)?;
                let second = tuple.get_item(1)?;

                res1.push_value(&first, py)?;
                res2.push_value(&second, py)?;
            } else {
                return Err(PyErr::new::<exceptions::PyTypeError, _>(
                    "Expected an array of tuples",
                ));
            }
        }

        // Create the result arrays
        let array1 = FSharpArray { storage: res1 };
        let array2 = FSharpArray { storage: res2 };

        // Return as a tuple
        let result_tuple = PyTuple::new(
            py,
            [Py::new(py, array1)?.bind(py), Py::new(py, array2)?.bind(py)],
        );
        Ok(result_tuple?.into())
    }

    #[pyo3(signature = (count, cons=None))]
    pub fn take(
        &self,
        py: Python<'_>,
        count: isize,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        if count < 0 {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "The input must be non-negative.",
            ));
        }

        let count = count as usize;
        let len = self.storage.len();

        if count > len {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "count is greater than array length",
            ));
        }

        if count == 0 {
            return FSharpArray::empty(py, cons);
        }

        // Use get_sub_array to get elements from 0 to count
        self.get_sub_array(py, 0, count, cons)
    }

    pub fn compare_to(
        &self,
        py: Python<'_>,
        comparer: &Bound<'_, PyAny>,
        other: &FSharpArray,
    ) -> PyResult<isize> {
        let len1 = self.storage.len();
        let len2 = other.storage.len();

        // First compare lengths (F# implementation behavior)
        if len1 > len2 {
            return Ok(1);
        } else if len1 < len2 {
            return Ok(-1);
        }

        // If lengths are equal, compare elements one by one
        for i in 0..len1 {
            let item1 = self.get_item_at_index(i as isize, py)?;
            let item2 = other.get_item_at_index(i as isize, py)?;

            let res = comparer.call1((item1, item2))?;
            let res: isize = res.extract()?;
            if res != 0 {
                return Ok(res);
            }
        }

        // All elements are equal
        Ok(0)
    }

    // Collect implementation (flatMap - map each element to an array then concatenate)
    #[pyo3(signature = (mapping, cons=None))]
    pub fn collect(
        &self,
        py: Python<'_>,
        mapping: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<Self> {
        let len = self.storage.len();

        // First, map each element to an array
        let mut mapped_arrays = Vec::with_capacity(len);
        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            let mapped_result = mapping.call1((item,))?;

            // Convert the result to a FSharpArray
            let mapped_array = ensure_array(py, &mapped_result)?;
            mapped_arrays.push(mapped_array);
        }

        // If no arrays to concatenate, return empty array
        if mapped_arrays.is_empty() {
            return FSharpArray::empty(py, cons);
        }

        // Now concatenate all the arrays
        let fs_cons = FSharpCons::extract(cons, &ArrayType::Generic);

        // Calculate total length needed
        let total_len: usize = mapped_arrays.iter().map(|arr| arr.storage.len()).sum();
        let mut result = fs_cons.create(total_len);

        // Copy all elements from all arrays
        for array in mapped_arrays {
            for i in 0..array.storage.len() {
                let item = array.get_item_at_index(i as isize, py)?;
                result.push_value(item.bind(py), py)?;
            }
        }

        Ok(FSharpArray { storage: result })
    }
}

// Loose functions that delegate to member functions
#[pyfunction]
#[pyo3(signature = (array1, array2, cons=None))]
pub fn append(
    py: Python<'_>,
    array1: &FSharpArray,
    array2: &Bound<'_, PyAny>,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    array1.append(py, array2, cons)
}

#[pyfunction]
#[pyo3(signature = (value, cons=None))]
pub fn singleton(
    py: Python<'_>,
    value: &Bound<'_, PyAny>,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    FSharpArray::singleton(py, value, cons)
}

#[pyfunction]
#[pyo3(signature = (cons=None))]
pub fn empty(py: Python<'_>, cons: Option<&Bound<'_, PyAny>>) -> PyResult<FSharpArray> {
    FSharpArray::empty(py, cons)
}

#[pyfunction]
pub fn create(py: Python<'_>, count: usize, value: &Bound<'_, PyAny>) -> PyResult<FSharpArray> {
    FSharpArray::create(py, count, value)
}

#[pyfunction]
#[pyo3(signature = (f, array, cons=None))]
pub fn map(
    py: Python<'_>,
    f: &Bound<'_, PyAny>,
    array: &FSharpArray,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    array.map(py, f, cons)
}

#[pyfunction]
pub fn map2(
    py: Python<'_>,
    f: &Bound<'_, PyAny>,
    array1: &FSharpArray,
    array2: &Bound<'_, PyAny>,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    array1.map2(py, f, array2, cons)
}

#[pyfunction]
pub fn map_indexed(
    py: Python<'_>,
    f: &Bound<'_, PyAny>,
    array: &FSharpArray,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    array.map_indexed(py, f, cons)
}

#[pyfunction]
pub fn filter(
    py: Python<'_>,
    predicate: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<FSharpArray> {
    array.filter(py, predicate)
}

#[pyfunction]
#[pyo3(signature = (count, array, cons=None))]
pub fn skip(
    py: Python<'_>,
    count: isize,
    array: &FSharpArray,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    array.skip(py, count, cons)
}

#[pyfunction]
#[pyo3(signature = (predicate, array, cons=None))]
pub fn skip_while(
    py: Python<'_>,
    predicate: &Bound<'_, PyAny>,
    array: &FSharpArray,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    array.skip_while(py, predicate, cons)
}

#[pyfunction]
#[pyo3(signature = (predicate, array, cons=None))]
pub fn take_while(
    py: Python<'_>,
    predicate: &Bound<'_, PyAny>,
    array: &FSharpArray,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    array.take_while(py, predicate, cons)
}

#[pyfunction]
pub fn chunk_by_size(
    py: Python<'_>,
    chunk_size: usize,
    array: &FSharpArray,
) -> PyResult<FSharpArray> {
    array.chunk_by_size(py, chunk_size)
}

#[pyfunction]
pub fn fill(
    py: Python<'_>,
    array: &mut FSharpArray,
    target_index: isize,
    count: usize,
    value: &Bound<'_, PyAny>,
) -> PyResult<()> {
    array.fill(target_index, count, value, py)
}

#[pyfunction]
pub fn fold(
    py: Python<'_>,
    folder: &Bound<'_, PyAny>,
    state: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<Py<PyAny>> {
    array.fold(py, folder, state)
}

#[pyfunction]
pub fn fold_indexed(
    py: Python<'_>,
    folder: &Bound<'_, PyAny>,
    state: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<Py<PyAny>> {
    array.fold_indexed(py, folder, state)
}

#[pyfunction]
pub fn fold_back(
    py: Python<'_>,
    folder: &Bound<'_, PyAny>,
    array: &FSharpArray,
    state: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    array.fold_back(py, folder, state)
}

#[pyfunction]
pub fn fold_back_indexed(
    py: Python<'_>,
    folder: &Bound<'_, PyAny>,
    array: &FSharpArray,
    state: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    array.fold_back_indexed(py, folder, state)
}

#[pyfunction]
pub fn sort_in_place(
    py: Python<'_>,
    array: &mut FSharpArray,
    comparer: &Bound<'_, PyAny>,
) -> PyResult<()> {
    array.sort_in_place(py, comparer)
}

#[pyfunction]
pub fn sort_in_place_with(
    py: Python<'_>,
    compare_func: &Bound<'_, PyAny>,
    array: &mut FSharpArray,
) -> PyResult<()> {
    array.sort_in_place_with(py, compare_func)
}

#[pyfunction]
pub fn sort_in_place_by(
    py: Python<'_>,
    projection: &Bound<'_, PyAny>,
    array: &mut FSharpArray,
    comparer: &Bound<'_, PyAny>,
) -> PyResult<()> {
    array.sort_in_place_by(py, projection, comparer)
}

#[pyfunction]
pub fn equals_with(
    py: Python<'_>,
    equals_func: &Bound<'_, PyAny>,
    array1: &FSharpArray,
    array2: &Bound<'_, PyAny>,
) -> PyResult<bool> {
    array1.equals_with(py, equals_func, array2)
}

#[pyfunction]
#[pyo3(signature = (array_ref, new_size, zero=None, cons=None))]
pub fn resize(
    py: Python<'_>,
    array_ref: &Bound<'_, PyAny>,
    new_size: usize,
    zero: Option<&Bound<'_, PyAny>>,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<()> {
    // Check if array_ref is a FSharpRef
    if let Ok(fs_ref) = array_ref.extract::<PyRef<'_, FSharpRef>>() {
        // Get current array using the getter
        let current_array_obj = fs_ref.get_contents(py)?;
        let current_array_bound = current_array_obj.bind(py);

        // Convert to FSharpArray (ensure_array now handles None as empty array)
        let mut current_array = ensure_array(py, current_array_bound)?;

        // Resize the array
        current_array.resize(py, new_size, zero, cons)?;

        // Set the new array using the setter
        let new_array_obj = current_array.into_pyobject(py)?;
        fs_ref.set_contents(py, new_array_obj.into())?;

        Ok(())
    } else {
        // Fallback: try to extract as FSharpArray directly (for backward compatibility)
        let mut array = array_ref.extract::<FSharpArray>()?;
        array.resize(py, new_size, zero, cons)
    }
}

#[pyfunction]
pub fn reduce(
    py: Python<'_>,
    reduction: &Bound<'_, PyAny>,
    array: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let array = ensure_array(py, array)?;
    array.reduce(py, reduction)
}

#[pyfunction]
pub fn reduce_back(
    py: Python<'_>,
    reduction: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<Py<PyAny>> {
    array.reduce_back(py, reduction)
}

#[pyfunction]
pub fn fold_back_indexed2(
    py: Python<'_>,
    folder: &Bound<'_, PyAny>,
    array1: &FSharpArray,
    array2: &Bound<'_, PyAny>,
    state: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    array1.fold_back_indexed2(py, folder, array2, state)
}

#[pyfunction]
pub fn fold_back2(
    py: Python<'_>,
    f: &Bound<'_, PyAny>,
    array1: &FSharpArray,
    array2: &Bound<'_, PyAny>,
    state: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    array1.fold_back2(py, f, array2, state)
}

#[pyfunction]
pub fn iterate(py: Python<'_>, action: &Bound<'_, PyAny>, array: &FSharpArray) -> PyResult<()> {
    array.iterate(py, action)
}

#[pyfunction]
pub fn iterate_indexed(
    py: Python<'_>,
    action: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<()> {
    array.iterate_indexed(py, action)
}

#[pyfunction]
pub fn sum(
    py: Python<'_>,
    array: &Bound<'_, PyAny>, // Take a PyAny instead of FSharpArray
    adder: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let array = ensure_array(py, array)?;

    // Now call the member function
    array.sum(py, adder)
}

#[pyfunction]
pub fn average(
    py: Python<'_>,
    array: &Bound<'_, PyAny>, // Take a PyAny instead of FSharpArray
    averager: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let array = ensure_array(py, array)?;

    // Now call the member function
    array.average(py, averager)
}

#[pyfunction]
pub fn average_by(
    py: Python<'_>,
    projection: &Bound<'_, PyAny>,
    array: &Bound<'_, PyAny>, // Take a PyAny instead of FSharpArray
    averager: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let array = ensure_array(py, array)?;

    // Now call the member function
    array.average_by(py, projection, averager)
}

#[pyfunction]
pub fn pairwise(py: Python<'_>, array: &FSharpArray) -> PyResult<FSharpArray> {
    array.pairwise(py)
}

#[pyfunction]
pub fn permute(
    py: Python<'_>,
    f: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<Py<FSharpArray>> {
    array.permute(py, f)
}

#[pyfunction]
#[pyo3(signature = (folder, state, array, cons=None))]
pub fn scan(
    py: Python<'_>,
    folder: &Bound<'_, PyAny>,
    state: &Bound<'_, PyAny>,
    array: &FSharpArray,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<Py<FSharpArray>> {
    array.scan(py, folder, state, cons)
}

#[pyfunction]
#[pyo3(signature = (folder, array, state, cons=None))]
pub fn scan_back(
    py: Python<'_>,
    folder: &Bound<'_, PyAny>,
    array: &FSharpArray,
    state: &Bound<'_, PyAny>,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<Py<FSharpArray>> {
    array.scan_back(py, folder, state, cons)
}

#[pyfunction]
pub fn split_into(py: Python<'_>, chunks: usize, array: &FSharpArray) -> PyResult<FSharpArray> {
    array.split_into(py, chunks)
}

#[pyfunction]
#[pyo3(signature = (array, cons=None))]
pub fn transpose(
    py: Python<'_>,
    array: &Bound<'_, PyAny>,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    let array = ensure_array(py, array)?;
    array.transpose(py, cons)
}

#[pyfunction]
pub fn try_find_back(
    py: Python<'_>,
    predicate: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<Option<Py<PyAny>>> {
    array.try_find_back(py, predicate)
}

#[pyfunction]
pub fn try_find_index_back(
    py: Python<'_>,
    predicate: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<Option<usize>> {
    array.try_find_index_back(py, predicate)
}

#[pyfunction]
pub fn windowed(py: Python<'_>, window_size: usize, array: &FSharpArray) -> PyResult<FSharpArray> {
    array.windowed(py, window_size)
}

#[pyfunction]
#[pyo3(signature = (mapping, state, array, cons=None))]
pub fn map_fold(
    py: Python<'_>,
    mapping: &Bound<'_, PyAny>,
    state: &Bound<'_, PyAny>,
    array: &FSharpArray,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<Py<PyAny>> {
    array.map_fold(py, mapping, state, cons)
}

#[pyfunction]
#[pyo3(signature = (mapping, array, state, cons=None))]
pub fn map_fold_back(
    py: Python<'_>,
    mapping: &Bound<'_, PyAny>,
    array: &Bound<'_, PyAny>,
    state: &Bound<'_, PyAny>,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<Py<PyAny>> {
    let array = ensure_array(py, array)?;
    array.map_fold_back(py, mapping, state, cons)
}

#[pyfunction]
pub fn head(py: Python<'_>, array: &FSharpArray) -> PyResult<Py<PyAny>> {
    array.head(py)
}

#[pyfunction]
pub fn try_head(py: Python<'_>, array: &FSharpArray) -> PyResult<Option<Py<PyAny>>> {
    array.try_head(py)
}

#[pyfunction]
#[pyo3(signature = (array, cons=None))]
pub fn tail(
    py: Python<'_>,
    array: &FSharpArray,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    array.tail(py, cons)
}

#[pyfunction]
pub fn item(py: Python<'_>, index: isize, array: &FSharpArray) -> PyResult<Py<PyAny>> {
    array.item(py, index)
}

#[pyfunction]
pub fn try_item(py: Python<'_>, index: isize, array: &FSharpArray) -> PyResult<Option<Py<PyAny>>> {
    array.try_item(py, index)
}

#[pyfunction]
pub fn reverse(py: Python<'_>, array: &FSharpArray) -> PyResult<FSharpArray> {
    array.reverse(py)
}

#[pyfunction]
#[pyo3(signature = (count, initializer, cons=None))]
pub fn initialize(
    py: Python<'_>,
    count: usize,
    initializer: &Bound<'_, PyAny>,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    FSharpArray::initialize(py, count, initializer, cons)
}

#[pyfunction]
pub fn compare_with(
    py: Python<'_>,
    comparer: &Bound<'_, PyAny>,
    array1: &FSharpArray,
    array2: &FSharpArray,
) -> PyResult<isize> {
    array1.compare_with(py, comparer, array2)
}

#[pyfunction]
pub fn exists_offset(
    py: Python<'_>,
    predicate: &Bound<'_, PyAny>,
    array: &FSharpArray,
    index: usize,
) -> PyResult<bool> {
    array.exists_offset(py, predicate, index)
}

#[pyfunction]
pub fn exists(py: Python<'_>, predicate: &Bound<'_, PyAny>, array: &FSharpArray) -> PyResult<bool> {
    array.exists(py, predicate)
}

#[pyfunction]
#[pyo3(signature = (index, value, array, cons=None))]
pub fn update_at(
    py: Python<'_>,
    index: usize,
    value: &Bound<'_, PyAny>,
    array: &FSharpArray,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    array.update_at(py, index, value, cons)
}

#[pyfunction]
pub fn set_slice(
    py: Python<'_>,
    target: &mut FSharpArray,
    lower: Option<usize>,
    upper: Option<usize>,
    source: &FSharpArray,
) -> PyResult<()> {
    source.set_slice(py, target, lower, upper)
}

#[pyfunction]
pub fn insert_at(
    py: Python<'_>,
    index: usize,
    value: &Bound<'_, PyAny>,
    array: &FSharpArray,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    array.insert_at(py, index, value, cons)
}

#[pyfunction]
pub fn insert_many_at(
    py: Python<'_>,
    index: usize,
    ys: &Bound<'_, PyAny>,
    array: &FSharpArray,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    array.insert_many_at(py, index, ys, cons)
}

#[pyfunction]
#[pyo3(signature = (f, array1, array2, array3, cons=None))]
pub fn map3(
    py: Python<'_>,
    f: &Bound<'_, PyAny>,
    array1: &FSharpArray,
    array2: &FSharpArray,
    array3: &FSharpArray,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    array1.map3(py, f, array2, array3, cons)
}

#[pyfunction]
#[pyo3(signature = (f, array1, array2, cons=None))]
pub fn map_indexed2(
    py: Python<'_>,
    f: &Bound<'_, PyAny>,
    array1: &FSharpArray,
    array2: &FSharpArray,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    array1.map_indexed2(py, f, array2, cons)
}

#[pyfunction]
#[pyo3(signature = (f, array1, array2, array3, cons=None))]
pub fn map_indexed3(
    py: Python<'_>,
    f: &Bound<'_, PyAny>,
    array1: &FSharpArray,
    array2: &FSharpArray,
    array3: &FSharpArray,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    array1.map_indexed3(py, f, array2, array3, cons)
}

#[pyfunction]
#[pyo3(signature = (index, array))]
pub fn remove_at(py: Python<'_>, index: isize, array: &mut FSharpArray) -> PyResult<FSharpArray> {
    array.remove_at(py, index)
}

#[pyfunction]
#[pyo3(signature = (index, count, array))]
pub fn remove_many_at(
    py: Python<'_>,
    index: isize,
    count: usize,
    array: &mut FSharpArray,
) -> PyResult<FSharpArray> {
    array.remove_many_at(py, index, count)
}

#[pyfunction]
pub fn remove_in_place(
    py: Python<'_>,
    array: &mut FSharpArray,
    item: &Bound<'_, PyAny>,
) -> PyResult<bool> {
    array.remove_in_place(py, item)
}

#[pyfunction]
pub fn index_of(
    py: Python<'_>,
    array: &FSharpArray,
    item: &Bound<'_, PyAny>,
    start: Option<usize>,
    count: Option<usize>,
    eq: Option<&Bound<'_, PyAny>>,
) -> PyResult<isize> {
    array.index_of(py, item, start, count, eq)
}

#[pyfunction]
pub fn copy_to(
    py: Python<'_>,
    source: &FSharpArray,
    source_index: usize,
    target: &mut FSharpArray,
    target_index: usize,
    count: usize,
) -> PyResult<()> {
    source.copy_to(py, source_index, target, target_index, count)
}

#[pyfunction]
pub fn zip(
    py: Python<'_>,
    array1: &FSharpArray,
    array2: &Bound<'_, PyAny>,
) -> PyResult<FSharpArray> {
    array1.zip(py, array2)
}

#[pyfunction]
pub fn for_all(
    py: Python<'_>,
    predicate: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<bool> {
    array.for_all(py, predicate)
}

#[pyfunction]
pub fn find(
    py: Python<'_>,
    predicate: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<Py<PyAny>> {
    array.find(py, predicate)
}

#[pyfunction]
pub fn try_find(
    py: Python<'_>,
    predicate: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<Option<Py<PyAny>>> {
    array.try_find(py, predicate)
}

#[pyfunction]
pub fn find_last_index(
    py: Python<'_>,
    predicate: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<usize> {
    array.find_last_index(py, predicate)
}

#[pyfunction]
pub fn add_in_place(
    py: Python<'_>,
    array: &mut FSharpArray,
    value: &Bound<'_, PyAny>,
) -> PyResult<()> {
    array.add_in_place(py, value)
}

#[pyfunction]
pub fn add_range_in_place(
    py: Python<'_>,
    array: &mut FSharpArray,
    range: &Bound<'_, PyAny>,
) -> PyResult<()> {
    array.add_range_in_place(py, range)
}

#[pyfunction]
pub fn insert_range_in_place(
    py: Python<'_>,
    array: &mut FSharpArray,
    index: isize,
    range: &Bound<'_, PyAny>,
) -> PyResult<()> {
    array.insert_range_in_place(py, index, range)
}

#[pyfunction]
#[pyo3(signature = (array, start_index, count, cons=None))]
pub fn get_sub_array(
    py: Python<'_>,
    array: &FSharpArray,
    start_index: isize,
    count: usize,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    array.get_sub_array(py, start_index, count, cons)
}

#[pyfunction]
#[pyo3(signature = (value, array, eq=None))]
// let contains<'T> (value: 'T) (array: 'T[]) ([<Inject>] eq: IEqualityComparer<'T>) =
pub fn contains(
    py: Python<'_>,
    value: &Bound<'_, PyAny>,
    array: &FSharpArray,
    eq: Option<&Bound<'_, PyAny>>,
) -> PyResult<bool> {
    array.contains(py, value, eq)
}

#[pyfunction]
pub fn copy(py: Python<'_>, array: &Bound<'_, PyAny>) -> PyResult<FSharpArray> {
    let array = ensure_array(py, array)?;
    array.copy(py)
}

#[pyfunction]
#[pyo3(signature = (array, comparer))]
pub fn max(
    py: Python<'_>,
    array: &Bound<'_, PyAny>,
    comparer: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let array = ensure_array(py, array)?;
    array.max(py, comparer)
}

#[pyfunction]
#[pyo3(signature = (array, comparer))]
pub fn min(
    py: Python<'_>,
    array: &Bound<'_, PyAny>,
    comparer: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let array = ensure_array(py, array)?;
    array.min(py, comparer)
}

#[pyfunction]
#[pyo3(signature = (chooser, array, cons=None))]
pub fn choose(
    py: Python<'_>,
    chooser: &Bound<'_, PyAny>,
    array: &FSharpArray,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    array.choose(py, chooser, cons)
}

#[pyfunction]
#[pyo3(signature = (mapping, array, cons=None))]
pub fn collect(
    py: Python<'_>,
    mapping: &Bound<'_, PyAny>,
    array: &FSharpArray,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    array.collect(py, mapping, cons)
}

#[pyfunction]
#[pyo3(signature = (key, array, comparer))]
pub fn max_by(
    py: Python<'_>,
    key: &Bound<'_, PyAny>,
    array: &Bound<'_, PyAny>,
    comparer: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let array = ensure_array(py, array)?;
    array.max_by(py, key, comparer)
}

#[pyfunction]
#[pyo3(signature = (key, array, comparer))]
pub fn min_by(
    py: Python<'_>,
    key: &Bound<'_, PyAny>,
    array: &Bound<'_, PyAny>,
    comparer: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let array = ensure_array(py, array)?;
    array.min_by(py, key, comparer)
}

#[pyfunction]
pub fn find_back(
    py: Python<'_>,
    predicate: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<Py<PyAny>> {
    array.find_back(py, predicate)
}

#[pyfunction]
pub fn pick(
    py: Python<'_>,
    chooser: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<Py<PyAny>> {
    array.pick(py, chooser)
}

#[pyfunction]
pub fn try_pick(
    py: Python<'_>,
    chooser: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<Option<Py<PyAny>>> {
    array.try_pick(py, chooser)
}

#[pyfunction]
pub fn remove_all_in_place(
    py: Python<'_>,
    predicate: &Bound<'_, PyAny>,
    array: &mut FSharpArray,
) -> PyResult<usize> {
    array.remove_all_in_place(py, predicate)
}

#[pyfunction]
pub fn indexed(py: Python<'_>, array: &FSharpArray) -> PyResult<FSharpArray> {
    array.indexed(py)
}

#[pyfunction]
pub fn try_find_index(
    py: Python<'_>,
    predicate: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<Option<usize>> {
    array.try_find_index(py, predicate)
}

#[pyfunction]
pub fn try_last(py: Python<'_>, array: &FSharpArray) -> PyResult<Option<Py<PyAny>>> {
    array.try_last(py)
}

#[pyfunction]
pub fn last(py: Python<'_>, array: &FSharpArray) -> PyResult<Py<PyAny>> {
    array.last(py)
}

#[pyfunction]
pub fn truncate(py: Python<'_>, count: isize, array: &FSharpArray) -> PyResult<FSharpArray> {
    array.truncate(py, count)
}

#[pyfunction]
#[pyo3(signature = (f, array, cons=None))]
pub fn partition(
    py: Python<'_>,
    f: &Bound<'_, PyAny>,
    array: &FSharpArray,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    array.partition(py, f, cons)
}

#[pyfunction]
#[pyo3(signature = (arrays, cons=None))]
pub fn concat(
    py: Python<'_>,
    arrays: &Bound<'_, PyAny>,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    // First pass: collect all arrays and calculate total length
    let iter = arrays.try_iter()?;
    let mut collected_arrays: Vec<FSharpArray> = Vec::new();
    let mut total_len = 0usize;

    for item in iter {
        let array = item?.extract::<FSharpArray>()?;
        total_len += array.__len__();
        collected_arrays.push(array);
    }

    // Handle empty input
    if collected_arrays.is_empty() {
        let fs_cons = FSharpCons::extract(cons, &ArrayType::Generic);
        return fs_cons.allocate(py, 0);
    }

    // Determine the target type from cons or first array
    let target_type = if let Some(cons) = cons {
        if let Ok(fs_cons) = cons.extract::<PyRef<'_, FSharpCons>>() {
            fs_cons.array_type.clone()
        } else {
            collected_arrays[0].storage.get_type().clone()
        }
    } else {
        collected_arrays[0].storage.get_type().clone()
    };

    // Allocate result array with exact capacity needed
    let mut result_storage = NativeArray::new(&target_type, Some(total_len));

    // Copy all elements from all arrays in a single pass
    for array in &collected_arrays {
        for i in 0..array.__len__() {
            result_storage.push_from_storage(&array.storage, i, py);
        }
    }

    Ok(FSharpArray {
        storage: result_storage,
    })
}

#[pyfunction]
pub fn find_index(
    py: Python<'_>,
    predicate: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<usize> {
    array.find_index(py, predicate)
}

#[pyfunction]
pub fn find_index_back(
    py: Python<'_>,
    predicate: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<usize> {
    array.find_index_back(py, predicate)
}

#[pyfunction]
pub fn sort(
    py: Python<'_>,
    array: &FSharpArray,
    comparer: &Bound<'_, PyAny>,
) -> PyResult<FSharpArray> {
    array.sort(py, comparer)
}

#[pyfunction]
#[pyo3(signature = (projection, array, comparer=None))]
pub fn sort_by(
    py: Python<'_>,
    projection: &Bound<'_, PyAny>,
    array: &FSharpArray,
    comparer: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    array.sort_by(py, projection, comparer)
}

#[pyfunction]
pub fn sort_with(
    py: Python<'_>,
    comparer: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<FSharpArray> {
    array.sort_with(py, comparer)
}

#[pyfunction]
#[pyo3(signature = (projection, array, adder))]
pub fn sum_by(
    py: Python<'_>,
    projection: &Bound<'_, PyAny>,
    array: &Bound<'_, PyAny>,
    adder: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let array = ensure_array(py, array)?;
    array.sum_by(py, projection, adder)
}

#[pyfunction]
pub fn unzip(py: Python<'_>, array: &FSharpArray) -> PyResult<Py<PyAny>> {
    array.unzip(py)
}

#[pyfunction]
#[pyo3(signature = (count, array, cons=None))]
pub fn take(
    py: Python<'_>,
    count: isize,
    array: &Bound<'_, PyAny>,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    let array = ensure_array(py, array)?;
    array.take(py, count, cons)
}

#[pyfunction]
pub fn compare_to(
    py: Python<'_>,
    comparer: &Bound<'_, PyAny>,
    source1: &FSharpArray,
    source2: &FSharpArray,
) -> PyResult<isize> {
    source1.compare_to(py, comparer, source2)
}

#[pyfunction]
#[pyo3(signature = (seq, cons=None))]
pub fn of_seq(
    py: Python<'_>,
    seq: &Bound<'_, PyAny>,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    // Extract array type from constructor if provided
    let array_type = if let Some(cons) = cons {
        if let Ok(fsharp_cons) = cons.extract::<PyRef<'_, FSharpCons>>() {
            Some(fsharp_cons.array_type.as_str())
        } else {
            None
        }
    } else {
        None
    };

    // Create array from the sequence
    FSharpArray::new(py, Some(seq), array_type)
}

// Constructor class for array allocation
#[pyclass(module = "fable")]
#[derive(Clone)]
struct FSharpCons {
    #[pyo3(get, set)]
    array_type: ArrayType,
}

#[pymethods]
impl FSharpCons {
    #[new]
    pub fn new(array_type: &str) -> PyResult<Self> {
        let array_type = match array_type {
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
        };

        Ok(FSharpCons { array_type })
    }

    // Allocate method implementing the Cons interface
    pub fn allocate(&self, _py: Python<'_>, length: usize) -> PyResult<FSharpArray> {
        let builder = NativeArray::new(&self.array_type, Some(length));
        let array = FSharpArray { storage: builder };
        Ok(array)
    }

    // Allow calling the constructor directly
    fn __call__(&self, py: Python<'_>, length: usize) -> PyResult<FSharpArray> {
        self.allocate(py, length)
    }

    #[classmethod]
    fn __class_getitem__(cls: &Bound<'_, PyType>, _item: &Bound<'_, PyAny>) -> Py<PyAny> {
        cls.clone().unbind().into()
    }
}

#[pymethods]
impl Int8Array {
    #[new]
    #[pyo3(signature = (elements=None))]
    fn new(py: Python<'_>, elements: Option<&Bound<'_, PyAny>>) -> PyResult<(Self, FSharpArray)> {
        Ok((Int8Array {}, FSharpArray::new(py, elements, Some("Int8"))?))
    }
}

#[pymethods]
impl UInt8Array {
    #[new]
    #[pyo3(signature = (elements=None))]
    fn new(py: Python<'_>, elements: Option<&Bound<'_, PyAny>>) -> PyResult<(Self, FSharpArray)> {
        Ok((
            UInt8Array {},
            FSharpArray::new(py, elements, Some("UInt8"))?,
        ))
    }
}

#[pymethods]
impl Int16Array {
    #[new]
    #[pyo3(signature = (elements=None))]
    fn new(py: Python<'_>, elements: Option<&Bound<'_, PyAny>>) -> PyResult<(Self, FSharpArray)> {
        Ok((
            Int16Array {},
            FSharpArray::new(py, elements, Some("Int16"))?,
        ))
    }
}

#[pymethods]
impl UInt16Array {
    #[new]
    #[pyo3(signature = (elements=None))]
    fn new(py: Python<'_>, elements: Option<&Bound<'_, PyAny>>) -> PyResult<(Self, FSharpArray)> {
        Ok((
            UInt16Array {},
            FSharpArray::new(py, elements, Some("UInt16"))?,
        ))
    }
}

#[pymethods]
impl Int32Array {
    #[new]
    #[pyo3(signature = (elements=None))]
    fn new(py: Python<'_>, elements: Option<&Bound<'_, PyAny>>) -> PyResult<(Self, FSharpArray)> {
        Ok((
            Int32Array {},
            FSharpArray::new(py, elements, Some("Int32"))?,
        ))
    }
}

#[pymethods]
impl UInt32Array {
    #[new]
    #[pyo3(signature = (elements=None))]
    fn new(py: Python<'_>, elements: Option<&Bound<'_, PyAny>>) -> PyResult<(Self, FSharpArray)> {
        Ok((
            UInt32Array {},
            FSharpArray::new(py, elements, Some("UInt32"))?,
        ))
    }
}

#[pymethods]
impl Int64Array {
    #[new]
    #[pyo3(signature = (elements=None))]
    fn new(py: Python<'_>, elements: Option<&Bound<'_, PyAny>>) -> PyResult<(Self, FSharpArray)> {
        Ok((
            Int64Array {},
            FSharpArray::new(py, elements, Some("Int64"))?,
        ))
    }
}

#[pymethods]
impl UInt64Array {
    #[new]
    #[pyo3(signature = (elements=None))]
    fn new(py: Python<'_>, elements: Option<&Bound<'_, PyAny>>) -> PyResult<(Self, FSharpArray)> {
        Ok((
            UInt64Array {},
            FSharpArray::new(py, elements, Some("UInt64"))?,
        ))
    }
}

#[pymethods]
impl Float32Array {
    #[new]
    #[pyo3(signature = (elements=None))]
    fn new(py: Python<'_>, elements: Option<&Bound<'_, PyAny>>) -> PyResult<(Self, FSharpArray)> {
        Ok((
            Float32Array {},
            FSharpArray::new(py, elements, Some("Float32"))?,
        ))
    }
}

#[pymethods]
impl Float64Array {
    #[new]
    #[pyo3(signature = (elements=None))]
    fn new(py: Python<'_>, elements: Option<&Bound<'_, PyAny>>) -> PyResult<(Self, FSharpArray)> {
        Ok((
            Float64Array {},
            FSharpArray::new(py, elements, Some("Float64"))?,
        ))
    }
}

#[pymethods]
impl BoolArray {
    #[new]
    #[pyo3(signature = (elements=None))]
    fn new(py: Python<'_>, elements: Option<&Bound<'_, PyAny>>) -> PyResult<(Self, FSharpArray)> {
        Ok((
            BoolArray {},
            FSharpArray::new(py, elements, Some("Bool"))?,
        ))
    }
}

#[pymethods]
impl GenericArray {
    #[new]
    #[pyo3(signature = (elements=None))]
    fn new(py: Python<'_>, elements: Option<&Bound<'_, PyAny>>) -> PyResult<(Self, FSharpArray)> {
        Ok((
            GenericArray {},
            FSharpArray::new(py, elements, Some("generic"))?,
        ))
    }
}

impl FSharpCons {
    fn create(&self, length: usize) -> NativeArray {
        NativeArray::new(&self.array_type, Some(length))
    }

    // Helper function to extract the constructor
    fn extract(cons: Option<&Bound<'_, PyAny>>, default_type: &ArrayType) -> FSharpCons {
        if let Some(cons) = cons {
            // Try to extract the constructor
            match cons.extract::<PyRef<'_, FSharpCons>>() {
                Ok(fs_cons) => fs_cons.clone(),
                Err(_) => FSharpCons {
                    array_type: default_type.clone(),
                },
            }
        } else {
            // Default to generic type if no constructor is provided
            FSharpCons {
                array_type: default_type.clone(),
            }
        }
    }
}

// Helper function for allocating arrays
#[pyfunction]
pub fn allocate_array_from_cons(
    py: Python<'_>,
    cons: Option<&Bound<'_, PyAny>>,
    length: usize,
) -> PyResult<Py<FSharpArray>> {
    let cons = FSharpCons::extract(cons, &ArrayType::Generic);
    let array = cons.allocate(py, length)?;
    Py::new(py, array)
}

// Utility function to extract typed vectors from any iterable
fn extract_typed_vec_from_iterable<U>(elements: &Bound<'_, PyAny>) -> PyResult<Vec<U>>
where
    U: for<'a, 'py> pyo3::FromPyObject<'a, 'py>,
{
    // Get the length. Most Python objects have a len method, but some don't.
    // like sequences.
    let len = elements.len().unwrap_or(0);
    let mut vec = Vec::with_capacity(len);

    for item in elements.try_iter()? {
        let bound_item = item?;
        let typed_item: U = bound_item.extract().map_err(|_| {
            PyErr::new::<pyo3::exceptions::PyValueError, _>("Failed to extract item from iterable")
        })?;
        vec.push(typed_item);
    }

    Ok(vec)
}

#[pyclass(module = "fable")]
struct FSharpArrayIter {
    array: Py<FSharpArray>,
    index: usize,
    len: usize,
}

#[pymethods]
impl FSharpArrayIter {
    fn __iter__(slf: PyRef<'_, Self>) -> PyRef<'_, Self> {
        slf
    }

    fn __next__(mut slf: PyRefMut<'_, Self>, py: Python<'_>) -> PyResult<Option<Py<PyAny>>> {
        if slf.index >= slf.len {
            return Ok(None);
        }
        let array = slf.array.bind(py);
        let array_ref = array.borrow();
        let item = array_ref.get_item_at_index(slf.index as isize, py)?;
        slf.index += 1;
        Ok(Some(item))
    }
}

// Internal reduce implementation for Rust closures (not exposed to Python)
fn reduce_impl<F>(array: &FSharpArray, py: Python<'_>, mut f: F) -> PyResult<Py<PyAny>>
where
    F: FnMut(Py<PyAny>, Py<PyAny>, Python<'_>) -> PyResult<Py<PyAny>>,
{
    let len = array.storage.len();
    if len == 0 {
        return Err(PyErr::new::<exceptions::PyValueError, _>(
            "Cannot reduce an empty array.",
        ));
    }
    let mut acc = array.get_item_at_index(0, py)?;
    for i in 1..len {
        let item = array.get_item_at_index(i as isize, py)?;
        acc = f(acc, item, py)?;
    }
    Ok(acc)
}

/// A module for array operations
pub fn register_array_module(parent_module: &Bound<'_, PyModule>) -> PyResult<()> {
    let m = PyModule::new(parent_module.py(), "array")?;

    m.add_class::<FSharpArray>()?;
    m.add_class::<FSharpCons>()?;

    m.add_function(wrap_pyfunction!(add_in_place, &m)?)?;
    m.add_function(wrap_pyfunction!(add_range_in_place, &m)?)?;
    m.add_function(wrap_pyfunction!(append, &m)?)?;
    m.add_function(wrap_pyfunction!(average, &m)?)?;
    m.add_function(wrap_pyfunction!(average_by, &m)?)?;
    m.add_function(wrap_pyfunction!(chunk_by_size, &m)?)?;
    m.add_function(wrap_pyfunction!(choose, &m)?)?;
    m.add_function(wrap_pyfunction!(collect, &m)?)?;
    m.add_function(wrap_pyfunction!(compare_with, &m)?)?;
    m.add_function(wrap_pyfunction!(concat, &m)?)?;
    m.add_function(wrap_pyfunction!(contains, &m)?)?;
    m.add_function(wrap_pyfunction!(copy, &m)?)?;
    m.add_function(wrap_pyfunction!(copy_to, &m)?)?;
    m.add_function(wrap_pyfunction!(create, &m)?)?;
    m.add_function(wrap_pyfunction!(empty, &m)?)?;
    m.add_function(wrap_pyfunction!(equals_with, &m)?)?;
    m.add_function(wrap_pyfunction!(exists, &m)?)?;
    m.add_function(wrap_pyfunction!(exists_offset, &m)?)?;
    m.add_function(wrap_pyfunction!(fill, &m)?)?;
    m.add_function(wrap_pyfunction!(filter, &m)?)?;
    m.add_function(wrap_pyfunction!(find, &m)?)?;
    m.add_function(wrap_pyfunction!(find_back, &m)?)?;
    m.add_function(wrap_pyfunction!(find_index, &m)?)?;
    m.add_function(wrap_pyfunction!(find_index_back, &m)?)?;
    m.add_function(wrap_pyfunction!(find_last_index, &m)?)?;
    m.add_function(wrap_pyfunction!(fold, &m)?)?;
    m.add_function(wrap_pyfunction!(fold_back, &m)?)?;
    m.add_function(wrap_pyfunction!(fold_back2, &m)?)?;
    m.add_function(wrap_pyfunction!(fold_back_indexed, &m)?)?;
    m.add_function(wrap_pyfunction!(fold_back_indexed2, &m)?)?;
    m.add_function(wrap_pyfunction!(fold_indexed, &m)?)?;
    m.add_function(wrap_pyfunction!(for_all, &m)?)?;
    m.add_function(wrap_pyfunction!(get_sub_array, &m)?)?;
    m.add_function(wrap_pyfunction!(head, &m)?)?;
    m.add_function(wrap_pyfunction!(indexed, &m)?)?;
    m.add_function(wrap_pyfunction!(initialize, &m)?)?;
    m.add_function(wrap_pyfunction!(insert_at, &m)?)?;
    m.add_function(wrap_pyfunction!(insert_many_at, &m)?)?;
    m.add_function(wrap_pyfunction!(insert_range_in_place, &m)?)?;
    m.add_function(wrap_pyfunction!(index_of, &m)?)?;
    m.add_function(wrap_pyfunction!(item, &m)?)?;
    m.add_function(wrap_pyfunction!(iterate, &m)?)?;
    m.add_function(wrap_pyfunction!(iterate_indexed, &m)?)?;
    m.add_function(wrap_pyfunction!(last, &m)?)?;
    m.add_function(wrap_pyfunction!(map, &m)?)?;
    m.add_function(wrap_pyfunction!(map2, &m)?)?;
    m.add_function(wrap_pyfunction!(map3, &m)?)?;
    m.add_function(wrap_pyfunction!(map_fold, &m)?)?;
    m.add_function(wrap_pyfunction!(map_fold_back, &m)?)?;
    m.add_function(wrap_pyfunction!(map_indexed, &m)?)?;
    m.add_function(wrap_pyfunction!(map_indexed2, &m)?)?;
    m.add_function(wrap_pyfunction!(map_indexed3, &m)?)?;
    m.add_function(wrap_pyfunction!(max, &m)?)?;
    m.add_function(wrap_pyfunction!(max_by, &m)?)?;
    m.add_function(wrap_pyfunction!(min, &m)?)?;
    m.add_function(wrap_pyfunction!(min_by, &m)?)?;
    m.add_function(wrap_pyfunction!(of_seq, &m)?)?;
    m.add_function(wrap_pyfunction!(pairwise, &m)?)?;
    m.add_function(wrap_pyfunction!(partition, &m)?)?;
    m.add_function(wrap_pyfunction!(permute, &m)?)?;
    m.add_function(wrap_pyfunction!(pick, &m)?)?;
    m.add_function(wrap_pyfunction!(reduce, &m)?)?;
    m.add_function(wrap_pyfunction!(reduce_back, &m)?)?;
    m.add_function(wrap_pyfunction!(remove_all_in_place, &m)?)?;
    m.add_function(wrap_pyfunction!(remove_at, &m)?)?;
    m.add_function(wrap_pyfunction!(remove_in_place, &m)?)?;
    m.add_function(wrap_pyfunction!(remove_many_at, &m)?)?;
    m.add_function(wrap_pyfunction!(resize, &m)?)?;
    m.add_function(wrap_pyfunction!(reverse, &m)?)?;
    m.add_function(wrap_pyfunction!(scan, &m)?)?;
    m.add_function(wrap_pyfunction!(scan_back, &m)?)?;
    m.add_function(wrap_pyfunction!(set_slice, &m)?)?;
    m.add_function(wrap_pyfunction!(singleton, &m)?)?;
    m.add_function(wrap_pyfunction!(skip, &m)?)?;
    m.add_function(wrap_pyfunction!(skip_while, &m)?)?;
    m.add_function(wrap_pyfunction!(sort, &m)?)?;
    m.add_function(wrap_pyfunction!(sort_by, &m)?)?;
    m.add_function(wrap_pyfunction!(sort_in_place, &m)?)?;
    m.add_function(wrap_pyfunction!(sort_in_place_by, &m)?)?;
    m.add_function(wrap_pyfunction!(sort_in_place_with, &m)?)?;
    m.add_function(wrap_pyfunction!(sort_with, &m)?)?;
    m.add_function(wrap_pyfunction!(split_into, &m)?)?;
    m.add_function(wrap_pyfunction!(sum, &m)?)?;
    m.add_function(wrap_pyfunction!(sum_by, &m)?)?;
    m.add_function(wrap_pyfunction!(tail, &m)?)?;
    m.add_function(wrap_pyfunction!(take, &m)?)?;
    m.add_function(wrap_pyfunction!(take_while, &m)?)?;
    m.add_function(wrap_pyfunction!(transpose, &m)?)?;
    m.add_function(wrap_pyfunction!(try_find, &m)?)?;
    m.add_function(wrap_pyfunction!(try_find_back, &m)?)?;
    m.add_function(wrap_pyfunction!(try_find_index, &m)?)?;
    m.add_function(wrap_pyfunction!(try_find_index_back, &m)?)?;
    m.add_function(wrap_pyfunction!(try_head, &m)?)?;
    m.add_function(wrap_pyfunction!(try_item, &m)?)?;
    m.add_function(wrap_pyfunction!(try_last, &m)?)?;
    m.add_function(wrap_pyfunction!(try_pick, &m)?)?;
    m.add_function(wrap_pyfunction!(truncate, &m)?)?;
    m.add_function(wrap_pyfunction!(unzip, &m)?)?;
    m.add_function(wrap_pyfunction!(update_at, &m)?)?;
    m.add_function(wrap_pyfunction!(windowed, &m)?)?;
    m.add_function(wrap_pyfunction!(zip, &m)?)?;
    m.add_function(wrap_pyfunction!(compare_to, &m)?)?;

    m.add_function(wrap_pyfunction!(allocate_array_from_cons, &m)?)?;

    parent_module.add_submodule(&m)?;
    Ok(())
}
