use crate::floats::{Float32, Float64};
use crate::ints::{Int16, Int32, Int64, Int8, UInt16, UInt32, UInt64, UInt8};
use crate::native_array::{ArrayType, NativeArray};
use pyo3::class::basic::CompareOp;
use pyo3::types::{PyBytes, PyTuple, PyType};
use pyo3::{exceptions, IntoPyObjectExt, PyTypeInfo};
use pyo3::{
    prelude::*,
    types::{PyAnyMethods, PyList},
};
use std::sync::{Arc, Mutex};

/// A module for array operations
pub fn register_array_module(parent_module: &Bound<'_, PyModule>) -> PyResult<()> {
    let m = PyModule::new(parent_module.py(), "array")?;

    m.add_class::<FSharpArray>()?;

    m.add_function(wrap_pyfunction!(allocate_array_from_cons, &m)?)?;
    m.add_function(wrap_pyfunction!(append, &m)?)?;
    m.add_function(wrap_pyfunction!(chunk_by_size, &m)?)?;
    m.add_function(wrap_pyfunction!(compare_with, &m)?)?;
    m.add_function(wrap_pyfunction!(create, &m)?)?;
    m.add_function(wrap_pyfunction!(empty, &m)?)?;
    m.add_function(wrap_pyfunction!(equals_with, &m)?)?;
    m.add_function(wrap_pyfunction!(exists, &m)?)?;
    m.add_function(wrap_pyfunction!(exists_offset, &m)?)?;
    m.add_function(wrap_pyfunction!(fill, &m)?)?;
    m.add_function(wrap_pyfunction!(filter, &m)?)?;
    m.add_function(wrap_pyfunction!(fold, &m)?)?;
    m.add_function(wrap_pyfunction!(fold_back, &m)?)?;
    m.add_function(wrap_pyfunction!(fold_back2, &m)?)?;
    m.add_function(wrap_pyfunction!(fold_back_indexed, &m)?)?;
    m.add_function(wrap_pyfunction!(fold_back_indexed2, &m)?)?;
    m.add_function(wrap_pyfunction!(fold_indexed, &m)?)?;
    m.add_function(wrap_pyfunction!(head, &m)?)?;
    m.add_function(wrap_pyfunction!(initialize, &m)?)?;
    m.add_function(wrap_pyfunction!(insert_at, &m)?)?;
    m.add_function(wrap_pyfunction!(insert_many_at, &m)?)?;
    m.add_function(wrap_pyfunction!(item, &m)?)?;
    m.add_function(wrap_pyfunction!(iterate, &m)?)?;
    m.add_function(wrap_pyfunction!(iterate_indexed, &m)?)?;
    m.add_function(wrap_pyfunction!(map, &m)?)?;
    m.add_function(wrap_pyfunction!(map2, &m)?)?;
    m.add_function(wrap_pyfunction!(map3, &m)?)?;
    m.add_function(wrap_pyfunction!(map_fold, &m)?)?;
    m.add_function(wrap_pyfunction!(map_fold_back, &m)?)?;
    m.add_function(wrap_pyfunction!(map_indexed, &m)?)?;
    m.add_function(wrap_pyfunction!(map_indexed2, &m)?)?;
    m.add_function(wrap_pyfunction!(map_indexed3, &m)?)?;
    m.add_function(wrap_pyfunction!(pairwise, &m)?)?;
    m.add_function(wrap_pyfunction!(permute, &m)?)?;
    m.add_function(wrap_pyfunction!(reduce, &m)?)?;
    m.add_function(wrap_pyfunction!(reduce_back, &m)?)?;
    m.add_function(wrap_pyfunction!(remove_at, &m)?)?;
    m.add_function(wrap_pyfunction!(remove_many_at, &m)?)?;
    m.add_function(wrap_pyfunction!(remove_in_place, &m)?)?;
    m.add_function(wrap_pyfunction!(index_of, &m)?)?;
    m.add_function(wrap_pyfunction!(reverse, &m)?)?;
    m.add_function(wrap_pyfunction!(scan, &m)?)?;
    m.add_function(wrap_pyfunction!(scan_back, &m)?)?;
    m.add_function(wrap_pyfunction!(set_slice, &m)?)?;
    m.add_function(wrap_pyfunction!(singleton, &m)?)?;
    m.add_function(wrap_pyfunction!(skip, &m)?)?;
    m.add_function(wrap_pyfunction!(sort_in_place, &m)?)?;
    m.add_function(wrap_pyfunction!(sort_in_place_with, &m)?)?;
    m.add_function(wrap_pyfunction!(split_into, &m)?)?;
    m.add_function(wrap_pyfunction!(sum, &m)?)?;
    m.add_function(wrap_pyfunction!(tail, &m)?)?;
    m.add_function(wrap_pyfunction!(transpose, &m)?)?;
    m.add_function(wrap_pyfunction!(try_find_back, &m)?)?;
    m.add_function(wrap_pyfunction!(try_find_index_back, &m)?)?;
    m.add_function(wrap_pyfunction!(try_head, &m)?)?;
    m.add_function(wrap_pyfunction!(try_item, &m)?)?;
    m.add_function(wrap_pyfunction!(update_at, &m)?)?;
    m.add_function(wrap_pyfunction!(windowed, &m)?)?;
    m.add_function(wrap_pyfunction!(copy_to, &m)?)?;
    m.add_function(wrap_pyfunction!(zip, &m)?)?;
    m.add_function(wrap_pyfunction!(for_all, &m)?)?;
    m.add_function(wrap_pyfunction!(find, &m)?)?;
    m.add_function(wrap_pyfunction!(try_find, &m)?)?;
    m.add_function(wrap_pyfunction!(find_last_index, &m)?)?;

    m.add_class::<FSharpCons>()?;
    m.add_function(wrap_pyfunction!(allocate_array_from_cons, &m)?)?;

    parent_module.add_submodule(&m)
}

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
struct StringArray {}

#[pyclass(module="fable", extends=FSharpArray)]
struct GenericArray {}

// Utility function to convert Python objects to FSharpArray
fn ensure_array(py: Python<'_>, ob: &Bound<'_, PyAny>) -> PyResult<FSharpArray> {
    // If it's already a FSharpArray, just extract it
    if let Ok(array) = ob.extract::<PyRef<'_, FSharpArray>>() {
        return Ok(array.clone());
    }

    // Check if the object is iterable
    if let Ok(iter) = ob.try_iter() {
        // Convert iterable directly to FSharpArray
        return FSharpArray::new(py, Some(iter.as_any()), None);
    }

    // If it's a single item, create a singleton array
    let singleton_list = PyList::new(py, &[ob])?;
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
        let nominal_type = if let Some(type_str) = array_type {
            match type_str {
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
                "String" => ArrayType::String,
                _ => ArrayType::Generic,
            }
        } else {
            ArrayType::Generic
        };

        if let Some(elements) = elements {
            // Try to create specialized storage if possible
            match &nominal_type {
                ArrayType::Int8 => {
                    if let Ok(vec) = extract_typed_vec_from_iterable::<Int8, i8>(elements, |x| {
                        Ok(*Int8::new(x)?)
                    }) {
                        return Ok(FSharpArray {
                            storage: NativeArray::Int8(vec),
                        });
                    }
                }
                ArrayType::UInt8 => {
                    if let Ok(vec) = extract_typed_vec_from_iterable::<UInt8, u8>(elements, |x| {
                        Ok(*UInt8::new(x)?)
                    }) {
                        return Ok(FSharpArray {
                            storage: NativeArray::UInt8(vec),
                        });
                    }
                }
                ArrayType::Int16 => {
                    if let Ok(vec) = extract_typed_vec_from_iterable::<Int16, i16>(elements, |x| {
                        Ok(*Int16::new(x)?)
                    }) {
                        return Ok(FSharpArray {
                            storage: NativeArray::Int16(vec),
                        });
                    }
                }
                ArrayType::UInt16 => {
                    if let Ok(vec) = extract_typed_vec_from_iterable::<UInt16, u16>(elements, |x| {
                        Ok(*UInt16::new(x)?)
                    }) {
                        return Ok(FSharpArray {
                            storage: NativeArray::UInt16(vec),
                        });
                    }
                }
                ArrayType::Int32 => {
                    // println!("FSharpArray::Int32");
                    if let Ok(vec) = extract_typed_vec_from_iterable::<Int32, i32>(elements, |x| {
                        Ok(*Int32::new(x)?)
                    }) {
                        return Ok(FSharpArray {
                            storage: NativeArray::Int32(vec),
                        });
                    }
                }
                ArrayType::UInt32 => {
                    if let Ok(vec) = extract_typed_vec_from_iterable::<UInt32, u32>(elements, |x| {
                        Ok(*UInt32::new(x)?)
                    }) {
                        return Ok(FSharpArray {
                            storage: NativeArray::UInt32(vec),
                        });
                    }
                }
                ArrayType::Int64 => {
                    if let Ok(vec) = extract_typed_vec_from_iterable::<Int64, i64>(elements, |x| {
                        Ok(*Int64::new(x)?)
                    }) {
                        return Ok(FSharpArray {
                            storage: NativeArray::Int64(vec),
                        });
                    }
                }
                ArrayType::UInt64 => {
                    if let Ok(vec) = extract_typed_vec_from_iterable::<UInt64, u64>(elements, |x| {
                        Ok(*UInt64::new(x)?)
                    }) {
                        return Ok(FSharpArray {
                            storage: NativeArray::UInt64(vec),
                        });
                    }
                }
                ArrayType::Float32 => {
                    if let Ok(vec) =
                        extract_typed_vec_from_iterable::<Float32, f32>(elements, |x| {
                            Ok(*x.extract::<Float32>()?)
                        })
                    {
                        return Ok(FSharpArray {
                            storage: NativeArray::Float32(vec),
                        });
                    }
                }
                ArrayType::Float64 => {
                    if let Ok(vec) =
                        extract_typed_vec_from_iterable::<Float64, f64>(elements, |x| {
                            Ok(*x.extract::<Float64>()?)
                        })
                    {
                        return Ok(FSharpArray {
                            storage: NativeArray::Float64(vec),
                        });
                    }
                }
                ArrayType::String => {
                    if let Ok(vec) =
                        extract_typed_vec_from_iterable::<String, String>(elements, |x| {
                            Ok(x.extract::<String>()?)
                        })
                    {
                        return Ok(FSharpArray {
                            storage: NativeArray::String(vec),
                        });
                    }
                }
                _ => {}
            }

            // Fallback to PyObject storage
            // println!("Fallback to PyObject storage");
            let len = elements.len();
            let mut vec = match len {
                Ok(len) => Vec::with_capacity(len),
                Err(_) => Vec::new(),
            };
            if let Ok(iter) = elements.try_iter() {
                for item in iter {
                    // Process item
                    vec.push(item?.into_pyobject(py)?.into());
                }
            }

            Ok(FSharpArray {
                storage: NativeArray::PyObject(Arc::new(Mutex::new(vec))),
            })
        } else {
            // Empty array - create with the right type but no elements
            Ok(FSharpArray {
                storage: NativeArray::create_empty_storage(&nominal_type),
            })
        }
    }

    #[classmethod]
    fn __class_getitem__(
        _cls: &Bound<'_, PyType>,
        item: &Bound<'_, PyAny>,
        py: Python<'_>,
    ) -> PyResult<PyObject> {
        // Get type name - either from string or from type.__name__
        let type_name: Option<String> = if let Ok(s) = item.extract::<String>() {
            Some(s)
        } else if let Ok(py_type) = item.downcast::<PyType>() {
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
            Some("string") | Some("str") => StringArray::type_object(py),
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
            println!("Creating UInt8 array");
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
        } else if let Ok(string) = value.extract::<String>() {
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, string);
            return Ok(FSharpArray {
                storage: NativeArray::String(vec),
            });
        }

        println!("Creating PyObject array");
        // Fallback to generic PyObject storage
        let mut vec = Vec::with_capacity(count);
        for _ in 0..count {
            vec.push(value.clone().into());
        }

        Ok(FSharpArray {
            storage: NativeArray::PyObject(Arc::new(Mutex::new(vec))),
        })
    }

    #[staticmethod]
    pub fn initialize(
        py: Python<'_>,
        count: usize,
        initializer: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        if count == 0 {
            return Ok(FSharpArray::empty(py, cons)?);
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

    pub fn __len__(&self) -> usize {
        self.storage.len()
    }

    pub fn __iter__(&self, py: Python<'_>) -> PyResult<PyObject> {
        let iter = FSharpArrayIter {
            array: Py::new(py, self.clone())?,
            index: 0,
            len: self.storage.len(),
        };
        iter.into_py_any(py)
    }

    pub fn __bytes__(&self, py: Python<'_>) -> PyResult<PyObject> {
        println!("Converting to bytes: {:?}", self.storage.type_name());
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
            _ => {
                println!("Cannot convert to bytes for this type");
                Ok(py.NotImplemented())
            }
        }
    }

    // Separate function to handle slice access
    fn get_item_slice(
        &self,
        slice: &Bound<'_, pyo3::types::PySlice>,
        py: Python<'_>,
    ) -> PyResult<PyObject> {
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
                result.__setitem__(result_idx, &item.bind(py), py)?;
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
        let mut builder = NativeArray::new(&self.storage.get_type(), Some(slice_len));

        // Add each element from the slice range
        for i in 0..slice_len {
            builder.push_from_storage(&self.storage, start + i, py);
        }

        // Create the result array
        let result = FSharpArray { storage: builder };
        Ok(Py::new(py, result)?.into())
    }

    pub fn __getitem__(&self, idx: &Bound<'_, PyAny>, py: Python<'_>) -> PyResult<PyObject> {
        // Try to downcast to a slice first
        if let Ok(slice) = idx.downcast::<pyo3::types::PySlice>() {
            // println!("Slice: {:?}", slice);
            return self.get_item_slice(&slice, py);
        }
        // Then try to extract as an integer
        else if let Ok(i) = idx.extract::<isize>() {
            // println!("Integer: {:?}", i);
            return self.get_item_at_index(i, py);
        }
        // If neither works, raise TypeError
        else {
            return Err(PyErr::new::<exceptions::PyTypeError, _>(
                "indices must be integers or slices",
            ));
        }
    }

    // Helper method to get an item at a specific index
    fn get_item_at_index(&self, idx: isize, py: Python<'_>) -> PyResult<PyObject> {
        let len = self.storage.len();
        let idx = if idx < 0 { len as isize + idx } else { idx };

        if idx < 0 || idx as usize >= len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "index out of range",
            ));
        }

        match &self.storage {
            NativeArray::Int8(vec) => {
                let value = vec[idx as usize];
                // Construct the Fable wrapper and convert to PyObject
                Ok(Int8(value).into_pyobject(py)?.into())
            }
            NativeArray::UInt8(vec) => {
                let value = vec[idx as usize];
                Ok(UInt8(value).into_pyobject(py)?.into())
            }
            NativeArray::Int16(vec) => {
                let value = vec[idx as usize];
                Ok(Int16(value).into_pyobject(py)?.into())
            }
            NativeArray::UInt16(vec) => {
                let value = vec[idx as usize];
                Ok(UInt16(value).into_pyobject(py)?.into())
            }
            NativeArray::Int32(vec) => {
                let value = vec[idx as usize];
                Ok(Int32(value).into_pyobject(py)?.into())
            }
            NativeArray::UInt32(vec) => {
                let value = vec[idx as usize];
                Ok(UInt32(value).into_pyobject(py)?.into())
            }
            NativeArray::Int64(vec) => {
                let value = vec[idx as usize];
                Ok(Int64(value).into_pyobject(py)?.into())
            }
            NativeArray::UInt64(vec) => {
                let value = vec[idx as usize];
                Ok(UInt64(value).into_pyobject(py)?.into())
            }
            NativeArray::Float32(vec) => {
                let value = vec[idx as usize];
                Ok(Float32(value).into_pyobject(py)?.into())
            }
            NativeArray::Float64(vec) => {
                let value = vec[idx as usize];
                Ok(Float64(value).into_pyobject(py)?.into())
            }
            NativeArray::String(vec) => Ok(vec[idx as usize].clone().into_pyobject(py)?.into()),
            NativeArray::PyObject(vec) => Ok(vec.lock().unwrap()[idx as usize].clone_ref(py)),
        }
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
            NativeArray::String(vec) => {
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
        Ok(self.storage.insert(idx as usize, value, py)?)
    }

    #[staticmethod]
    pub fn empty(py: Python<'_>, cons: Option<&Bound<'_, PyAny>>) -> PyResult<FSharpArray> {
        // Determine the type from constructor
        let fs_cons = FSharpCons::extract(cons, &ArrayType::Generic);

        // Create an empty array with this type
        let array = fs_cons.allocate(py, 0)?;
        Ok(array)
    }

    pub fn remove_at(&mut self, py: Python<'_>, index: isize) -> PyResult<FSharpArray> {
        let len = self.storage.len();
        let index = if index < 0 {
            len as isize + index
        } else {
            index
        };

        if index < 0 || index as usize >= len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "index out of range",
            ));
        }

        // Create a new array with the same type and size - 1
        let mut builder = NativeArray::new(&self.storage.get_type(), Some(len - 1));

        // Copy all elements except the one at the specified index
        for i in 0..len {
            if i as isize != index {
                builder.push_from_storage(&self.storage, i, py);
            }
        }

        // Create the result array
        let result = FSharpArray { storage: builder };
        Ok(result)
    }

    pub fn remove_many_at(
        &mut self,
        py: Python<'_>,
        index: isize,
        count: usize,
    ) -> PyResult<FSharpArray> {
        let len = self.storage.len();
        let index = if index < 0 {
            len as isize + index
        } else {
            index
        };

        if index < 0 || index as usize >= len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "index out of range",
            ));
        }

        // Create a new array with the same type and size - count
        let mut builder = NativeArray::new(&self.storage.get_type(), Some(len - count));

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
        let fs_cons = FSharpCons::extract(cons, &self.storage.get_type());
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
        count: usize,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let len = self.storage.len();
        let fs_cons = FSharpCons::extract(cons, &ArrayType::Generic);
        if len == 0 {
            return Ok(fs_cons.allocate(py, 0)?);
        }
        // Create the builder for results
        let mut results = fs_cons.create(len.saturating_sub(count));

        // Add the remaining elements (after skipping)
        let actual_count = std::cmp::min(count, len);
        for i in actual_count..len {
            // We can use push_original since we're just taking elements from the original array
            results.push_from_storage(&self.storage, i, py);
        }

        // Construct the result array
        Ok(FSharpArray { storage: results })
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

        // Determine target type from cons or preserve source type
        let target_type = self.storage.get_type().clone();

        // Create the builder for results
        let mut results = NativeArray::new(&target_type, None); // No initial capacity needed

        // Add each chunk to the result
        for x in 0..((len + chunk_size - 1) / chunk_size) {
            let start = x * chunk_size;
            let end = std::cmp::min(start + chunk_size, len);
            for i in start..end {
                results.push_from_storage(&self.storage, i, py);
            }
        }

        // Construct the result array
        Ok(FSharpArray { storage: results })
    }

    pub fn fill(
        &mut self,
        target_index: isize,
        count: usize,
        value: &Bound<'_, PyAny>,
        py: Python<'_>,
    ) -> PyResult<Py<Self>> {
        // Validate input parameters
        let len = self.storage.len();
        let target_index = if target_index < 0 {
            len as isize + target_index
        } else {
            target_index
        };

        if target_index < 0 || target_index as usize >= len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "target_index out of range",
            ));
        }

        // Make sure we don't go beyond array bounds
        let available_slots = len - target_index as usize;
        let actual_count = std::cmp::min(count, available_slots);

        match &mut self.storage {
            NativeArray::PyObject(arc_vec) => {
                // Create a new vector with the same contents
                let mut new_vec = Vec::with_capacity(arc_vec.lock().unwrap().len());

                // Copy all elements to the new vector, replacing values in the target range
                for (i, obj) in arc_vec.lock().unwrap().iter().enumerate() {
                    if i >= target_index as usize && i < target_index as usize + actual_count {
                        // Insert the fill value
                        let item: PyObject = value.clone().into();
                        new_vec.push(item);
                    } else {
                        // Keep the existing element
                        new_vec.push(obj.clone_ref(py));
                    }
                }

                // Replace the old Arc with a new one
                self.storage = NativeArray::PyObject(Arc::new(Mutex::new(new_vec)));
            }
            _ => {
                // For all other types, use the helper
                NativeArray::fill_storage(
                    &mut self.storage,
                    target_index as usize,
                    actual_count,
                    value,
                )?;
            }
        }
        Ok(Py::new(py, self.clone())?)
    }

    pub fn fold(
        &self,
        py: Python<'_>,
        folder: &Bound<'_, PyAny>,
        state: &Bound<'_, PyAny>,
    ) -> PyResult<PyObject> {
        let len = self.storage.len();
        let mut acc = state.clone();

        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            acc = folder.call1((item, acc))?;
        }

        Ok(acc.into())
    }

    pub fn fold_indexed(
        &self,
        py: Python<'_>,
        folder: &Bound<'_, PyAny>,
        state: &Bound<'_, PyAny>,
    ) -> PyResult<PyObject> {
        let len = self.storage.len();
        let mut acc = state.clone();

        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            acc = folder.call1((i, item, acc))?;
        }

        Ok(acc.into())
    }

    pub fn fold_back(
        &self,
        py: Python<'_>,
        folder: &Bound<'_, PyAny>,
        state: &Bound<'_, PyAny>,
    ) -> PyResult<PyObject> {
        let len = self.storage.len();
        let mut acc = state.clone();

        for i in (0..len).rev() {
            let item = self.get_item_at_index(i as isize, py)?;
            acc = folder.call1((item, acc))?;
        }

        Ok(acc.into())
    }

    pub fn fold_back_indexed(
        &self,
        py: Python<'_>,
        folder: &Bound<'_, PyAny>,
        state: &Bound<'_, PyAny>,
    ) -> PyResult<PyObject> {
        let len = self.storage.len();
        let mut acc = state.clone(); // Updated to use clone()

        for i in (0..len).rev() {
            let item = self.get_item_at_index(i as isize, py)?;
            acc = folder.call1((i, item, acc))?;
        }

        Ok(acc.into())
    }

    pub fn sort_in_place(&mut self, py: Python<'_>) -> PyResult<()> {
        match &mut self.storage {
            NativeArray::Int8(vec) => vec.sort(),
            NativeArray::UInt8(vec) => vec.sort(),
            NativeArray::Int16(vec) => vec.sort(),
            NativeArray::UInt16(vec) => vec.sort(),
            NativeArray::Int32(vec) => vec.sort(),
            NativeArray::UInt32(vec) => vec.sort(),
            NativeArray::Int64(vec) => vec.sort(),
            NativeArray::UInt64(vec) => vec.sort(),
            NativeArray::Float32(vec) => vec.sort_by(|a, b| a.partial_cmp(b).unwrap()),
            NativeArray::Float64(vec) => vec.sort_by(|a, b| a.partial_cmp(b).unwrap()),
            NativeArray::String(vec) => vec.sort(),
            NativeArray::PyObject(arc_vec) => {
                let mut new_vec: Vec<Py<PyAny>> = Vec::with_capacity(arc_vec.lock().unwrap().len());
                for obj in arc_vec.lock().unwrap().iter() {
                    new_vec.push(obj.clone_ref(py));
                }

                // Use sort_by with Python's rich comparison protocol
                new_vec.sort_by(|a, b| {
                    // Compare a < b using Python's __lt__ method
                    match a.bind(py).rich_compare(b.bind(py), CompareOp::Lt) {
                        Ok(result) => {
                            if result.is_truthy().unwrap_or(false) {
                                std::cmp::Ordering::Less
                            } else {
                                // If a is not less than b, check if b < a
                                match b.bind(py).rich_compare(a.bind(py), CompareOp::Lt) {
                                    Ok(result) => {
                                        if result.is_truthy().unwrap_or(false) {
                                            std::cmp::Ordering::Greater
                                        } else {
                                            std::cmp::Ordering::Equal
                                        }
                                    }
                                    Err(_) => std::cmp::Ordering::Equal, // Default to Equal on error
                                }
                            }
                        }
                        Err(_) => std::cmp::Ordering::Equal, // Default to Equal on error
                    }
                });

                self.storage = NativeArray::PyObject(Arc::new(Mutex::new(new_vec)));
            }
        }
        Ok(())
    }

    pub fn sort_in_place_with(
        &mut self,
        py: Python<'_>,
        compare_func: &Bound<'_, PyAny>,
    ) -> PyResult<()> {
        match &mut self.storage {
            NativeArray::PyObject(arc_vec) => {
                let mut new_vec: Vec<Py<PyAny>> = Vec::with_capacity(arc_vec.lock().unwrap().len());
                for obj in arc_vec.lock().unwrap().iter() {
                    new_vec.push(obj.clone_ref(py));
                }

                // Use sort_by with
                new_vec.sort_by(|a, b| {
                    // Compare a < b using Python's __lt__ method
                    match compare_func.call1((a, b)) {
                        Ok(result) => {
                            if result.is_truthy().unwrap_or(false) {
                                std::cmp::Ordering::Less
                            } else {
                                // If a is not less than b, check if b < a
                                match compare_func.call1((b, a)) {
                                    Ok(result) => {
                                        if result.is_truthy().unwrap_or(false) {
                                            std::cmp::Ordering::Greater
                                        } else {
                                            std::cmp::Ordering::Equal
                                        }
                                    }
                                    Err(_) => std::cmp::Ordering::Equal, // Default to Equal on error
                                }
                            }
                        }
                        Err(_) => std::cmp::Ordering::Equal, // Default to Equal on error
                    }
                });
                self.storage = NativeArray::PyObject(Arc::new(Mutex::new(new_vec)));
            }
            _ => {
                // For all other types, use the helper
                NativeArray::sort_storage_with(&mut self.storage, compare_func)?;
            }
        }
        Ok(())
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

    pub fn reduce(&self, py: Python<'_>, reduction: &Bound<'_, PyAny>) -> PyResult<PyObject> {
        let len = self.storage.len();
        if len == 0 {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "Cannot reduce an empty array.",
            ));
        }

        // Initialize the accumulator with the first element
        let mut acc = self.get_item_at_index(0, py)?;

        for i in 1..len {
            let item = self.get_item_at_index(i as isize, py)?;
            acc = reduction.call1((acc, item))?.into();
        }

        Ok(acc.into())
    }

    pub fn reduce_back(&self, py: Python<'_>, reduction: &Bound<'_, PyAny>) -> PyResult<PyObject> {
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
    ) -> PyResult<PyObject> {
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
    ) -> PyResult<PyObject> {
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

    pub fn sum(&self, py: Python<'_>, adder: &Bound<'_, PyAny>) -> PyResult<PyObject> {
        let len = self.storage.len();
        let mut acc = adder.call_method0("GetZero")?;

        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            acc = adder.call_method1("Add", (acc, item))?;
        }

        Ok(acc.into())
    }

    pub fn pairwise(&self, py: Python<'_>) -> PyResult<FSharpArray> {
        let len = self.storage.len();
        if len < 2 {
            return Ok(FSharpArray::empty(py, None)?);
        }

        let count = len - 1;
        let builder = NativeArray::new(&self.storage.get_type(), Some(count));
        let mut result = FSharpArray { storage: builder };

        for i in 0..count {
            let item1 = self.get_item_at_index(i as isize, py)?;
            let item2 = self.get_item_at_index((i + 1) as isize, py)?;
            let tuple = PyTuple::new(py, &[item1, item2])?;
            result.__setitem__(i as isize, &tuple, py)?;
        }

        Ok(result)
    }

    pub fn permute(&self, py: Python<'_>, f: &Bound<'_, PyAny>) -> PyResult<Py<FSharpArray>> {
        let len = self.storage.len();
        let builder = NativeArray::new(&self.storage.get_type(), Some(len));
        let mut result = FSharpArray { storage: builder };

        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            let new_index = f.call1((&item,))?.extract::<usize>()?;
            let item_bound = item.bind(py);
            result.__setitem__(new_index as isize, &item_bound, py)?;
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
        let fs_cons = FSharpCons::extract(cons, &self.storage.get_type());
        let mut results = fs_cons.create(len + 1);
        results.push_value(state, py)?;

        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            let new_state = folder.call1((item, state))?;
            results.push_value(&new_state, py)?;
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
        let mut current_state = state.clone();
        let fs_cons = FSharpCons::extract(cons, &self.storage.get_type());
        let mut results = fs_cons.create(self.storage.len() + 1);
        results.push_value(&current_state, py)?;

        for i in (0..self.storage.len()).rev() {
            let x = self.get_item_at_index(i as isize, py)?;
            current_state = folder.call1((current_state, x))?;
            results.insert(0, &current_state, py)?;
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
                storage: NativeArray::PyObject(Arc::new(Mutex::new(vec![]))),
            });
        }

        // Create the builder for results
        let mut results = NativeArray::new(&self.storage.get_type(), None); // No initial capacity needed

        // Add each chunk to the result
        for x in 0..((len + chunks - 1) / chunks) {
            let start = x * chunks;
            let end = std::cmp::min(start + chunks, len);
            for i in start..end {
                results.push_from_storage(&self.storage, i, py);
            }
        }

        // Construct the result array
        Ok(FSharpArray { storage: results })
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
        let fs_cons = FSharpCons::extract(cons, &self.storage.get_type());
        let mut results = fs_cons.create(len_inner);

        // Fill the result array
        for _i in 0..len_inner {
            let mut inner_array = fs_cons.create(len);
            for j in 0..len {
                let item = self.get_item_at_index(j as isize, py)?;
                inner_array.push_value(&item.bind(py), py)?;
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
    ) -> PyResult<Option<PyObject>> {
        let len = self.storage.len();
        for i in (0..len).rev() {
            let item = self.get_item_at_index(i as isize, py)?;
            if predicate.call1((&item,))?.is_truthy()? {
                return Ok(Some(item));
            }
        }
        Ok(None)
    }

    pub fn try_find_index_back(
        &self,
        py: Python<'_>,
        predicate: &Bound<'_, PyAny>,
    ) -> PyResult<Option<usize>> {
        let len = self.storage.len();
        for i in (0..len).rev() {
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
        let builder = NativeArray::new(&self.storage.get_type(), Some(count));
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
                window.__setitem__(j as isize, &item.bind(py), py)?;
            }

            // Add the window to the result array
            result.__setitem__(i as isize, &Py::new(py, window)?.bind(py), py)?;
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
    ) -> PyResult<PyObject> {
        let len = self.storage.len();

        // Get the constructor or use default
        let fs_cons = FSharpCons::extract(cons, &self.storage.get_type());

        if len == 0 {
            // Return empty array and original state
            let empty_array = fs_cons.create(0);
            let result_array = FSharpArray {
                storage: empty_array,
            };
            let result_tuple =
                PyTuple::new(py, &[Py::new(py, result_array)?.bind(py), &state.clone()]);
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
        let result_tuple = PyTuple::new(py, &[Py::new(py, result_array)?.bind(py), &current_state]);
        Ok(result_tuple?.into())
    }

    #[pyo3(signature = (mapping, state, cons=None))]
    pub fn map_fold_back(
        &self,
        py: Python<'_>,
        mapping: &Bound<'_, PyAny>,
        state: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<PyObject> {
        let len = self.storage.len();

        // Get the constructor or use default
        let fs_cons = FSharpCons::extract(cons, &self.storage.get_type());

        if len == 0 {
            // Return empty array and original state
            let empty_array = fs_cons.create(0);
            let result_array = FSharpArray {
                storage: empty_array,
            };
            let result_tuple =
                PyTuple::new(py, &[Py::new(py, result_array)?.bind(py), &state.clone()]);
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
        let result_tuple = PyTuple::new(py, &[Py::new(py, result_array)?.bind(py), &current_state]);
        Ok(result_tuple?.into())
    }

    // Get the first element of the array
    pub fn head(&self, py: Python<'_>) -> PyResult<PyObject> {
        if self.storage.len() == 0 {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "The input array was empty",
            ));
        }
        self.get_item_at_index(0, py)
    }

    // Try to get the first element, returning None if array is empty
    pub fn try_head(&self, py: Python<'_>) -> PyResult<Option<PyObject>> {
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
    pub fn item(&self, py: Python<'_>, index: isize) -> PyResult<PyObject> {
        self.get_item_at_index(index, py)
    }

    // Try to get an item at a specific index, returning None if out of bounds
    pub fn try_item(&self, py: Python<'_>, index: isize) -> PyResult<Option<PyObject>> {
        let len = self.storage.len();
        let idx = if index < 0 {
            len as isize + index
        } else {
            index
        };

        if idx < 0 || idx as usize >= len {
            Ok(None)
        } else {
            Ok(Some(self.get_item_at_index(idx, py)?))
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
    ) -> PyResult<bool> {
        // Check if the other object is a FSharpArray
        if self.storage.len() != other.storage.len() {
            return Ok(false);
        }

        // Compare elements using the provided comparer function
        for i in 0..self.storage.len() {
            let item1 = self.get_item_at_index(i as isize, py)?;
            let item2 = other.get_item_at_index(i as isize, py)?;

            let result = comparer.call1((item1, item2))?;
            if result.extract::<i32>()? != 0 {
                return Ok(false);
            }
        }
        Ok(true)
    }

    pub fn exists_offset(
        &self,
        py: Python<'_>,
        predicate: &Bound<'_, PyAny>,
        index: usize,
    ) -> PyResult<bool> {
        let len = self.storage.len();
        if index >= len {
            return Ok(false);
        }

        let item = self.get_item_at_index(index as isize, py)?;
        if predicate.call1((item,))?.is_truthy()? {
            return Ok(true);
        }

        // Recursively check the next index
        self.exists_offset(py, predicate, index + 1)
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
        let fs_cons = FSharpCons::extract(cons, &self.storage.get_type());
        let mut target = fs_cons.allocate(py, len)?;

        // Fill the new array with values from the original array
        for i in 0..len {
            if i == index {
                target.__setitem__(i as isize, &value, py)?;
            } else {
                let item = self.get_item_at_index(i as isize, py)?;
                target.__setitem__(i as isize, &item.bind(py), py)?;
            }
        }

        Ok(target)
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
            target.__setitem__((i + lower) as isize, &item.bind(py), py)?;
        }

        Ok(())
    }

    // Format array as F# style string: [1; 2; 3] or [1; 2; 3; ... ] for longer arrays
    pub fn __str__(&self, py: Python<'_>) -> PyResult<String> {
        let len = self.storage.len();

        // Empty array case
        if len == 0 {
            return Ok("[]".to_string());
        }

        // Define max elements to show before truncating
        const MAX_DISPLAY_ELEMENTS: usize = 3;
        let show_ellipsis = len > MAX_DISPLAY_ELEMENTS;
        let elements_to_show = if show_ellipsis {
            MAX_DISPLAY_ELEMENTS
        } else {
            len
        };

        let mut result = String::from("[");

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

        result.push(']');
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
            NativeArray::String(_) => "String",
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
        _cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let len = self.storage.len();
        if index > len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "Index out of bounds",
            ));
        }

        // Create a new array using the constructor
        let target_storage = NativeArray::create_from_storage(&self.storage, py);

        let mut target = FSharpArray {
            storage: target_storage,
        };

        // Set the new value at the specified index
        target.__setitem__(index as isize, &value, py)?;

        Ok(target)
    }

    pub fn insert_many_at(
        &self,
        py: Python<'_>,
        index: usize,
        ys: &Bound<'_, PyAny>,
        _cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let len = self.storage.len();
        if index > len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "Index out of bounds",
            ));
        }

        // Create a new array using the constructor
        let target_storage = NativeArray::create_from_storage(&self.storage, py);

        let mut target = FSharpArray {
            storage: target_storage,
        };

        // Set the new value at the specified index
        target.__setitem__(index as isize, &ys, py)?;

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
        let fs_cons = FSharpCons::extract(cons, &self.storage.get_type());
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
        let fs_cons = FSharpCons::extract(cons, &self.storage.get_type());
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
        let fs_cons = FSharpCons::extract(cons, &self.storage.get_type());
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
        let fs_cons = FSharpCons::extract(cons, &self.storage.get_type());
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

    pub fn index_of(&self, py: Python<'_>, item: &Bound<'_, PyAny>) -> PyResult<Option<usize>> {
        let len = self.storage.len();
        for i in 0..len {
            let current = self.get_item_at_index(i as isize, py)?;
            if current
                .bind(py)
                .rich_compare(item, CompareOp::Eq)?
                .is_truthy()?
            {
                return Ok(Some(i));
            }
        }
        Ok(None)
    }

    pub fn remove_in_place(&mut self, py: Python<'_>, item: &Bound<'_, PyAny>) -> PyResult<bool> {
        // Find the index of the item to remove using indexOf
        let index = match self.index_of(py, item)? {
            Some(idx) => idx,
            None => return Ok(false),
        };

        self.storage.remove_at_index(index);
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
        self.storage
            .copy_to(&mut target.storage, source_index, target_index, count, py)
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

    pub fn find(&self, py: Python<'_>, predicate: &Bound<'_, PyAny>) -> PyResult<PyObject> {
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
    ) -> PyResult<Option<PyObject>> {
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
pub fn singleton(
    py: Python<'_>,
    value: &Bound<'_, PyAny>,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    FSharpArray::singleton(py, value, cons)
}

#[pyfunction]
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
    count: usize,
    array: &FSharpArray,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    array.skip(py, count, cons)
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
    array: &Bound<'_, PyAny>, // Take a PyAny instead of FSharpArray
    target_index: isize,
    count: usize,
    value: &Bound<'_, PyAny>,
) -> PyResult<Py<FSharpArray>> {
    let mut array = ensure_array(py, array)?;
    array.fill(target_index, count, value, py)
}

#[pyfunction]
pub fn fold(
    py: Python<'_>,
    folder: &Bound<'_, PyAny>,
    state: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<PyObject> {
    array.fold(py, folder, state)
}

#[pyfunction]
pub fn fold_indexed(
    py: Python<'_>,
    folder: &Bound<'_, PyAny>,
    state: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<PyObject> {
    array.fold_indexed(py, folder, state)
}

#[pyfunction]
pub fn fold_back(
    py: Python<'_>,
    folder: &Bound<'_, PyAny>,
    array: &FSharpArray,
    state: &Bound<'_, PyAny>,
) -> PyResult<PyObject> {
    array.fold_back(py, folder, state)
}

#[pyfunction]
pub fn fold_back_indexed(
    py: Python<'_>,
    folder: &Bound<'_, PyAny>,
    array: &FSharpArray,
    state: &Bound<'_, PyAny>,
) -> PyResult<PyObject> {
    array.fold_back_indexed(py, folder, state)
}

#[pyfunction]
pub fn sort_in_place(py: Python<'_>, array: &mut FSharpArray) -> PyResult<()> {
    array.sort_in_place(py)
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
pub fn equals_with(
    py: Python<'_>,
    equals_func: &Bound<'_, PyAny>,
    array1: &FSharpArray,
    array2: &Bound<'_, PyAny>,
) -> PyResult<bool> {
    array1.equals_with(py, equals_func, array2)
}

#[pyfunction]
pub fn reduce(
    py: Python<'_>,
    reduction: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<PyObject> {
    array.reduce(py, reduction)
}

#[pyfunction]
pub fn reduce_back(
    py: Python<'_>,
    reduction: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<PyObject> {
    array.reduce_back(py, reduction)
}

#[pyfunction]
pub fn fold_back_indexed2(
    py: Python<'_>,
    folder: &Bound<'_, PyAny>,
    array1: &FSharpArray,
    array2: &Bound<'_, PyAny>,
    state: &Bound<'_, PyAny>,
) -> PyResult<PyObject> {
    array1.fold_back_indexed2(py, folder, array2, state)
}

#[pyfunction]
pub fn fold_back2(
    py: Python<'_>,
    f: &Bound<'_, PyAny>,
    array1: &FSharpArray,
    array2: &Bound<'_, PyAny>,
    state: &Bound<'_, PyAny>,
) -> PyResult<PyObject> {
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
) -> PyResult<PyObject> {
    let array = ensure_array(py, array)?;

    // Now call the member function
    array.sum(py, adder)
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
    array: &FSharpArray,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    array.transpose(py, cons)
}

#[pyfunction]
pub fn try_find_back(
    py: Python<'_>,
    predicate: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<Option<PyObject>> {
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
) -> PyResult<PyObject> {
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
) -> PyResult<PyObject> {
    let array = ensure_array(py, array)?;
    array.map_fold_back(py, mapping, state, cons)
}

#[pyfunction]
pub fn head(py: Python<'_>, array: &FSharpArray) -> PyResult<PyObject> {
    array.head(py)
}

#[pyfunction]
pub fn try_head(py: Python<'_>, array: &FSharpArray) -> PyResult<Option<PyObject>> {
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
pub fn item(py: Python<'_>, index: isize, array: &FSharpArray) -> PyResult<PyObject> {
    array.item(py, index)
}

#[pyfunction]
pub fn try_item(py: Python<'_>, index: isize, array: &FSharpArray) -> PyResult<Option<PyObject>> {
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
) -> PyResult<bool> {
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
) -> PyResult<Option<usize>> {
    array.index_of(py, item)
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
) -> PyResult<PyObject> {
    array.find(py, predicate)
}

#[pyfunction]
pub fn try_find(
    py: Python<'_>,
    predicate: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<Option<PyObject>> {
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

// Constructor class for array allocation
#[pyclass()]
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
            "String" => ArrayType::String,
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
impl StringArray {
    #[new]
    #[pyo3(signature = (elements=None))]
    fn new(py: Python<'_>, elements: Option<&Bound<'_, PyAny>>) -> PyResult<(Self, FSharpArray)> {
        Ok((
            StringArray {},
            FSharpArray::new(py, elements, Some("String"))?,
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
fn extract_typed_vec_from_iterable<T, U>(
    elements: &Bound<'_, PyAny>,
    extractor: impl Fn(&Bound<'_, PyAny>) -> PyResult<U>,
) -> PyResult<Vec<U>>
where
    T: for<'a> pyo3::FromPyObject<'a>,
{
    // Check if the object is iterable
    if let Err(err) = elements.try_iter() {
        // println!("Error: {:?}", err);
        return Err(err);
    }

    let len = elements.len();
    let mut vec = match len {
        Ok(len) => Vec::with_capacity(len),
        Err(_) => Vec::new(),
    };

    for item in elements.try_iter()? {
        // println!("Item: {:?}", item);
        let typed_item = extractor(&item?)?;
        // println!("Got here");
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

    fn __next__(mut slf: PyRefMut<'_, Self>, py: Python<'_>) -> PyResult<Option<PyObject>> {
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
