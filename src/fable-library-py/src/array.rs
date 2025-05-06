use std::sync::Arc;
use pyo3::class::basic::CompareOp;
use pyo3::types::{PyBytes, PyTuple, PyType};
use crate::floats::{Float32, Float64};
use crate::ints::{Int16, Int32, Int64, Int8, UInt16, UInt32, UInt64, UInt8};
use pyo3::{exceptions, IntoPyObjectExt};
use pyo3::{
    prelude::*,
    types::{PyAnyMethods, PyList},
};

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

    m.add_class::<FSharpCons>()?;
    m.add_function(wrap_pyfunction!(allocate_array_from_cons, &m)?)?;

    parent_module.add_submodule(&m)
}

// Enum to track the declared/nominal type of an array
#[derive(Clone, Debug, PartialEq)]
enum ArrayType {
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

// The actual storage can be different from the nominal type
#[derive(Debug, Clone)]
enum ArrayStorage {
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
    PyObject(Arc<Vec<PyObject>>),
}

#[pyclass(module = "fable")]
#[derive(Clone)]
pub struct FSharpArray {
    storage: ArrayStorage,
    nominal_type: ArrayType,
}

// Utility function to convert Python objects to FSharpArray
fn ensure_array(py: Python<'_>, ob: &Bound<'_, PyAny>) -> PyResult<FSharpArray> {
    // If it's already a FSharpArray, just extract it
    if let Ok(array) = ob.extract::<PyRef<'_, FSharpArray>>() {
        return Ok(array.clone());
    }

    // Check if the object is iterable
    if let Ok(iter) = ob.try_iter() {
        // Convert iterable directly to FSharpArray
        return FSharpArray::new(py, None, Some(iter.as_any()));
    }

    // If it's a single item, create a singleton array
    let singleton_list = PyList::new(py, &[ob])?;
    FSharpArray::new(py, None, Some(&singleton_list))
}

#[pymethods]
impl FSharpArray {
    #[new]
    #[pyo3(signature = (array_type=None, elements=None))]
    pub fn new(
        py: Python<'_>,
        array_type: Option<&str>,
        elements: Option<&Bound<'_, PyAny>>,
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
                    if let Ok(vec) = extract_typed_vec_from_iterable::<Int8, i8>(elements, |x|  Ok(*Int8::new(x)?)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::Int8(vec),
                            nominal_type,
                        });
                    }
                }
                ArrayType::UInt8 => {
                    if let Ok(vec) = extract_typed_vec_from_iterable::<UInt8, u8>(elements, |x| Ok(*UInt8::new(x)?)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::UInt8(vec),
                            nominal_type,
                        });
                    }
                }
                ArrayType::Int16 => {
                    if let Ok(vec) = extract_typed_vec_from_iterable::<Int16, i16>(elements, |x| Ok(*Int16::new(x)?)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::Int16(vec),
                            nominal_type,
                        });
                    }
                }
                ArrayType::UInt16 => {
                    if let Ok(vec) = extract_typed_vec_from_iterable::<UInt16, u16>(elements, |x| Ok(*UInt16::new(x)?)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::UInt16(vec),
                            nominal_type,
                        });
                    }
                }
                ArrayType::Int32 => {
                    // println!("FSharpArray::Int32");
                    if let Ok(vec) = extract_typed_vec_from_iterable::<Int32, i32>(elements, |x| Ok(*Int32::new(x)?)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::Int32(vec),
                            nominal_type,
                        });
                    }
                }
                ArrayType::UInt32 => {
                    if let Ok(vec) = extract_typed_vec_from_iterable::<UInt32, u32>(elements, |x| Ok(*UInt32::new(x)?)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::UInt32(vec),
                            nominal_type,
                        });
                    }
                }
                ArrayType::Int64 => {
                    if let Ok(vec) = extract_typed_vec_from_iterable::<Int64, i64>(elements, |x| Ok(*Int64::new(x)?)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::Int64(vec),
                            nominal_type,
                        });
                    }
                }
                ArrayType::UInt64 => {
                    if let Ok(vec) = extract_typed_vec_from_iterable::<UInt64, u64>(elements, |x| Ok(*UInt64::new(x)?)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::UInt64(vec),
                            nominal_type,
                        });
                    }
                }
                ArrayType::Float32 => {
                    if let Ok(vec) = extract_typed_vec_from_iterable::<Float32, f32>(elements, |x| Ok(*x.extract::<Float32>()?)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::Float32(vec),
                            nominal_type,
                        });
                    }
                }
                ArrayType::Float64 => {
                    if let Ok(vec) = extract_typed_vec_from_iterable::<Float64, f64>(elements, |x| Ok(*x.extract::<Float64>()?)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::Float64(vec),
                            nominal_type,
                        });
                    }
                }
                ArrayType::String => {
                    if let Ok(vec) = extract_typed_vec_from_iterable::<String, String>(elements, |x| Ok(x.extract::<String>()?)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::String(vec),
                            nominal_type,
                        });
                    }
                }
                _ => {}
            }

            // Fallback to PyObject storage
            // println!("Fallback to PyObject storage");
            let len = elements.len();
            let mut vec =
                match len {
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
                storage: ArrayStorage::PyObject(Arc::new(vec)),
                nominal_type,
            })
        } else {
            // Empty array - create with the right type but no elements
            Ok(ArrayBuilder::create_empty_array(&nominal_type))
        }
    }

    #[classmethod]
    fn __class_getitem__(cls: &Bound<'_, PyType>, _item: &Bound<'_, PyAny>, py: Python<'_>) -> PyResult<PyObject> {
        // This just returns the class itself, making the type hints work
        // without changing runtime behavior
        Ok(cls.into_py_any(py)?)
    }

    // Creates an array whose elements are all initially the given value.
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
                storage: ArrayStorage::Int8(vec),
                nominal_type: ArrayType::Int8,
            });
        } else if let Ok(uint8) = value.extract::<UInt8>() {
            println!("Creating UInt8 array");
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, *uint8);
            return Ok(FSharpArray {
                storage: ArrayStorage::UInt8(vec),
                nominal_type: ArrayType::UInt8,
            });
        } else if let Ok(int16) = value.extract::<Int16>() {
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, *int16);
            return Ok(FSharpArray {
                storage: ArrayStorage::Int16(vec),
                nominal_type: ArrayType::Int16,
            });
        } else if let Ok(uint16) = value.extract::<UInt16>() {
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, *uint16);
            return Ok(FSharpArray {
                storage: ArrayStorage::UInt16(vec),
                nominal_type: ArrayType::UInt16,
            });
        } else if let Ok(int32) = value.extract::<Int32>() {
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, *int32);
            return Ok(FSharpArray {
                storage: ArrayStorage::Int32(vec),
                nominal_type: ArrayType::Int32,
            });
        } else if let Ok(uint32) = value.extract::<UInt32>() {
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, *uint32);
            return Ok(FSharpArray {
                storage: ArrayStorage::UInt32(vec),
                nominal_type: ArrayType::UInt32,
            });
        } else if let Ok(int64) = value.extract::<Int64>() {
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, *int64);
            return Ok(FSharpArray {
                storage: ArrayStorage::Int64(vec),
                nominal_type: ArrayType::Int64,
            });
        } else if let Ok(uint64) = value.extract::<UInt64>() {
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, *uint64);
            return Ok(FSharpArray {
                storage: ArrayStorage::UInt64(vec),
                nominal_type: ArrayType::UInt64,
            });
        } else if let Ok(float32) = value.extract::<Float32>() {
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, *float32);
            return Ok(FSharpArray {
                storage: ArrayStorage::Float32(vec),
                nominal_type: ArrayType::Float32,
            });
        } else if let Ok(float64) = value.extract::<Float64>() {
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, *float64);
            return Ok(FSharpArray {
                storage: ArrayStorage::Float64(vec),
                nominal_type: ArrayType::Float64,
            });
        } else if let Ok(string) = value.extract::<String>() {
            let mut vec = Vec::with_capacity(count);
            vec.resize(count, string);
            return Ok(FSharpArray {
                storage: ArrayStorage::String(vec),
                nominal_type: ArrayType::String,
            });
        }

        println!("Creating PyObject array");
        // Fallback to generic PyObject storage
        let mut vec = Vec::with_capacity(count);
        for _ in 0..count {
            vec.push(value.clone().into());
        }

        Ok(FSharpArray {
            storage: ArrayStorage::PyObject(Arc::new(vec)),
            nominal_type: ArrayType::Generic,
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
        let mut results = fs_cons.create_builder(count);

        // Initialize each element using the provided initializer function
        for i in 0..count {
            let item = initializer.call1((i,))?;
            results.push_value(&item, py)?;
        }

        // Construct the result array
        Ok(ArrayBuilder::create_array(results, &fs_cons.array_type))
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

        // Create an array with capacity of 1
        let mut array = fs_cons.allocate(py, 1)?;

        // Set the single element
        array.__setitem__(0, value, py)?;

        Ok(array)
    }

    pub fn __len__(&self) -> usize {
        match &self.storage {
            ArrayStorage::Int8(vec) => vec.len(),
            ArrayStorage::UInt8(vec) => vec.len(),
            ArrayStorage::Int16(vec) => vec.len(),
            ArrayStorage::UInt16(vec) => vec.len(),
            ArrayStorage::Int32(vec) => vec.len(),
            ArrayStorage::UInt32(vec) => vec.len(),
            ArrayStorage::Int64(vec) => vec.len(),
            ArrayStorage::UInt64(vec) => vec.len(),
            ArrayStorage::Float32(vec) => vec.len(),
            ArrayStorage::Float64(vec) => vec.len(),
            ArrayStorage::String(vec) => vec.len(),
            ArrayStorage::PyObject(vec) => vec.len(),
        }
    }

    pub fn __bytes__(&self, py: Python<'_>) -> PyResult<PyObject> {
        match &self.storage {
            // For UInt8/Int8 arrays, we can create bytes directly
            ArrayStorage::UInt8(vec) => {
                let bytes = PyBytes::new(py, vec.as_slice());
                Ok(bytes.into())
            },
            ArrayStorage::Int8(vec) => {
                // Convert i8 slice to u8 slice with unsafe transmute
                // This is safe because we're just reinterpreting the bits
                let bytes = PyBytes::new(py, unsafe {
                    std::slice::from_raw_parts(
                        vec.as_ptr() as *const u8,
                        vec.len()
                    )
                });
                Ok(bytes.into())
            },
            // For other numeric types, create a bytearray from their raw memory
            ArrayStorage::Int16(vec) => {
                let bytes = PyBytes::new(py, unsafe {
                    std::slice::from_raw_parts(
                        vec.as_ptr() as *const u8,
                        vec.len() * std::mem::size_of::<i16>()
                    )
                });
                Ok(bytes.into())
            },
            ArrayStorage::UInt16(vec) => {
                let bytes = PyBytes::new(py, unsafe {
                    std::slice::from_raw_parts(
                        vec.as_ptr() as *const u8,
                        vec.len() * std::mem::size_of::<u16>()
                    )
                });
                Ok(bytes.into())
            },
            // Similar patterns for other numeric types
            ArrayStorage::Int32(vec) => {
                let bytes = PyBytes::new(py, unsafe {
                    std::slice::from_raw_parts(
                        vec.as_ptr() as *const u8,
                        vec.len() * std::mem::size_of::<i32>()
                    )
                });
                Ok(bytes.into())
            },
            ArrayStorage::UInt32(vec) => {
                let bytes = PyBytes::new(py, unsafe {
                    std::slice::from_raw_parts(
                        vec.as_ptr() as *const u8,
                        vec.len() * std::mem::size_of::<u32>()
                    )
                });
                Ok(bytes.into())
            },
            ArrayStorage::Int64(vec) => {
                let bytes = PyBytes::new(py, unsafe {
                    std::slice::from_raw_parts(
                        vec.as_ptr() as *const u8,
                        vec.len() * std::mem::size_of::<i64>()
                    )
                });
                Ok(bytes.into())
            },
            ArrayStorage::UInt64(vec) => {
                let bytes = PyBytes::new(py, unsafe {
                    std::slice::from_raw_parts(
                        vec.as_ptr() as *const u8,
                        vec.len() * std::mem::size_of::<u64>()
                    )
                });
                Ok(bytes.into())
            },
            ArrayStorage::Float32(vec) => {
                let bytes = PyBytes::new(py, unsafe {
                    std::slice::from_raw_parts(
                        vec.as_ptr() as *const u8,
                        vec.len() * std::mem::size_of::<f32>()
                    )
                });
                Ok(bytes.into())
            },
            ArrayStorage::Float64(vec) => {
                let bytes = PyBytes::new(py, unsafe {
                    std::slice::from_raw_parts(
                        vec.as_ptr() as *const u8,
                        vec.len() * std::mem::size_of::<f64>()
                    )
                });
                Ok(bytes.into())
            },
            // For non-numeric types, return NotImplemented
            _ => {
                Ok(py.NotImplemented())
            }
        }
    }

    // Separate function to handle slice access
    fn get_item_slice(&self, slice: &Bound<'_, pyo3::types::PySlice>, py: Python<'_>) -> PyResult<PyObject> {
        let len = self.__len__();
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
            let fs_cons = FSharpCons::new(&self.nominal_type.clone().into_pyobject(py)?.extract::<String>()?)?;
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
        let mut builder = ArrayBuilder::new(&self.nominal_type, Some(slice_len));

        // Add each element from the slice range
        for i in 0..slice_len {
            builder.push_from_storage(&self.storage, start + i, py);
        }

        // Create the result array
        let result = ArrayBuilder::create_array(builder, &self.nominal_type);
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
                "indices must be integers or slices"
            ));
        }
    }

    // Helper method to get an item at a specific index
    fn get_item_at_index(&self, idx: isize, py: Python<'_>) -> PyResult<PyObject> {
        let len = self.__len__();
        let idx = if idx < 0 { len as isize + idx } else { idx };


        if idx < 0 || idx as usize >= len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "index out of range",
            ));
        }

        match &self.storage {
            ArrayStorage::Int8(vec) => {
                let value = vec[idx as usize];
                // Construct the Fable wrapper and convert to PyObject
                Ok(Int8(value).into_pyobject(py)?.into())
            }
            ArrayStorage::UInt8(vec) => {
                let value = vec[idx as usize];
                Ok(UInt8(value).into_pyobject(py)?.into())
            }
            ArrayStorage::Int16(vec) => {
                let value = vec[idx as usize];
                Ok(Int16(value).into_pyobject(py)?.into())
            }
            ArrayStorage::UInt16(vec) => {
                let value = vec[idx as usize];
                Ok(UInt16(value).into_pyobject(py)?.into())
            }
            ArrayStorage::Int32(vec) => {
                let value = vec[idx as usize];
                Ok(Int32(value).into_pyobject(py)?.into())
            }
            ArrayStorage::UInt32(vec) => {
                let value = vec[idx as usize];
                Ok(UInt32(value).into_pyobject(py)?.into())
            }
            ArrayStorage::Int64(vec) => {
                let value = vec[idx as usize];
                Ok(Int64(value).into_pyobject(py)?.into())
            }
            ArrayStorage::UInt64(vec) => {
                let value = vec[idx as usize];
                Ok(UInt64(value).into_pyobject(py)?.into())
            }
            ArrayStorage::Float32(vec) => {
                let value = vec[idx as usize];
                Ok(Float32(value).into_pyobject(py)?.into())
            }
            ArrayStorage::Float64(vec) => {
                let value = vec[idx as usize];
                Ok(Float64(value).into_pyobject(py)?.into())
            }
            ArrayStorage::String(vec) => Ok(vec[idx as usize].clone().into_pyobject(py)?.into()),
            ArrayStorage::PyObject(vec) => Ok(vec[idx as usize].clone_ref(py)),
        }
    }

    pub fn __setitem__(
        &mut self,
        idx: isize,
        value: &Bound<'_, PyAny>,
        py: Python<'_>,
    ) -> PyResult<()> {
        let len = self.__len__();
        let idx = if idx < 0 { len as isize + idx } else { idx };

        if idx < 0 || idx as usize >= len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "index out of range",
            ));
        }

        match &mut self.storage {
            ArrayStorage::Int8(vec) => {
                if let Ok(i_val) = value.extract::<i8>() {
                    vec[idx as usize] = i_val;
                    return Ok(());
                }
            }
            ArrayStorage::UInt8(vec) => {
                if let Ok(u_val) = value.extract::<u8>() {
                    vec[idx as usize] = u_val;
                    return Ok(());
                }
            }
            ArrayStorage::Int16(vec) => {
                if let Ok(i_val) = value.extract::<i16>() {
                    vec[idx as usize] = i_val;
                    return Ok(());
                }
            }
            ArrayStorage::UInt16(vec) => {
                if let Ok(u_val) = value.extract::<u16>() {
                    vec[idx as usize] = u_val;
                    return Ok(());
                }
            }
            ArrayStorage::Int32(vec) => {
                if let Ok(i_val) = value.extract::<i32>() {
                    vec[idx as usize] = i_val;
                    return Ok(());
                }
            }
            ArrayStorage::UInt32(vec) => {
                if let Ok(u_val) = value.extract::<u32>() {
                    vec[idx as usize] = u_val;
                    return Ok(());
                }
            }
            ArrayStorage::Int64(vec) => {
                if let Ok(i_val) = value.extract::<i64>() {
                    vec[idx as usize] = i_val;
                    return Ok(());
                }
            }
            ArrayStorage::UInt64(vec) => {
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
            ArrayStorage::Float32(vec) => {
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
            ArrayStorage::Float64(vec) => {
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
            ArrayStorage::String(vec) => {
                vec[idx as usize] = value.extract()?;
            }
            ArrayStorage::PyObject(arc_vec) => {
                // Try to get mutable access to the vector without cloning
                if let Some(vec_mut) = Arc::get_mut(arc_vec) {
                    // If we have unique ownership, update the element directly
                    vec_mut[idx as usize] = value.clone().into();
                } else {
                    println!("Need to copya and clone PyObject array");
                    // If the Arc is shared, we need to create a new Vec and replace the Arc
                    let mut new_vec = Vec::with_capacity(arc_vec.len());

                    // Copy all elements, cloning each Py<PyAny> reference
                    for (i, obj) in arc_vec.iter().enumerate() {
                        if i == idx as usize {
                            // For the element we're updating, use the new value
                            new_vec.push(value.clone().into());
                        } else {
                            // For other elements, clone the existing reference
                            new_vec.push(obj.clone_ref(py));
                        }
                    }

                    // Replace the old Arc with a new one containing the updated Vec
                    *arc_vec = Arc::new(new_vec);
                }
            }
        }

        Ok(())
    }

    pub fn __delitem__(&mut self, idx: isize, py: Python<'_>) -> PyResult<()> {
        let len = self.__len__();
        let idx = if idx < 0 { len as isize + idx } else { idx };

        if idx < 0 || idx as usize >= len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "index out of range",
            ));
        }

        match &mut self.storage {
            ArrayStorage::Int8(vec) => {
                vec.remove(idx as usize);
            }
            ArrayStorage::UInt8(vec) => {
                vec.remove(idx as usize);
            }
            ArrayStorage::Int16(vec) => {
                vec.remove(idx as usize);
            }
            ArrayStorage::UInt16(vec) => {
                vec.remove(idx as usize);
            }
            ArrayStorage::Int32(vec) => {
                vec.remove(idx as usize);
            }
            ArrayStorage::UInt32(vec) => {
                vec.remove(idx as usize);
            }
            ArrayStorage::Int64(vec) => {
                vec.remove(idx as usize);
            }
            ArrayStorage::UInt64(vec) => {
                vec.remove(idx as usize);
            }
            ArrayStorage::Float32(vec) => {
                vec.remove(idx as usize);
            }
            ArrayStorage::Float64(vec) => {
                vec.remove(idx as usize);
            }
            ArrayStorage::String(vec) => {
                vec.remove(idx as usize);
            }
            ArrayStorage::PyObject(arc_vec) => {
                // Create a new vector and copy all elements
                let mut new_vec: Vec<Py<PyAny>> = Vec::with_capacity(arc_vec.len());
                for (i, obj) in arc_vec.iter().enumerate() {
                    if i != idx as usize {
                        // Clone the existing element
                        let item: PyObject = obj.clone_ref(py);
                        new_vec.push(item);
                    }
                }
                // Replace the old Arc with a new one containing the updated vector
                self.storage = ArrayStorage::PyObject(Arc::new(new_vec));
            }
        }

        Ok(())
    }

    pub fn insert(&mut self, idx: isize, value: &Bound<'_, PyAny>, py: Python<'_>) -> PyResult<()> {
        let len = self.__len__();
        let idx = if idx < 0 { len as isize + idx } else { idx };

        if idx < 0 || idx as usize > len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "index out of range",
            ));
        }

        match &mut self.storage {
            ArrayStorage::Int8(vec) => {
                let int8: Int8 = value.extract()?;
                vec.insert(idx as usize, *int8);
            }
            ArrayStorage::UInt8(vec) => {
                let uint8: UInt8 = value.extract()?;
                vec.insert(idx as usize, *uint8);
            }
            ArrayStorage::Int16(vec) => {
                let int16: Int16 = value.extract()?;
                vec.insert(idx as usize, *int16);
            }
            ArrayStorage::UInt16(vec) => {
                let uint16: UInt16 = value.extract()?;
                vec.insert(idx as usize, *uint16);
            }
            ArrayStorage::Int32(vec) => {
                let int32: Int32 = value.extract()?;
                vec.insert(idx as usize, *int32);
            }
            ArrayStorage::UInt32(vec) => {
                let uint32: UInt32 = value.extract()?;
                vec.insert(idx as usize, *uint32);
            }
            ArrayStorage::Int64(vec) => {
                let int64: Int64 = value.extract()?;
                vec.insert(idx as usize, *int64);
            }
            ArrayStorage::UInt64(vec) => {
                let uint64: UInt64 = value.extract()?;
                vec.insert(idx as usize, *uint64);
            }
            ArrayStorage::Float32(vec) => {
                let float32: Float32 = value.extract()?;
                vec.insert(idx as usize, *float32);
            }
            ArrayStorage::Float64(vec) => {
                let float64: Float64 = value.extract()?;
                vec.insert(idx as usize, *float64);
            }
            ArrayStorage::String(vec) => {
                let string_value: String = value.extract()?;
                vec.insert(idx as usize, string_value);
            }
            ArrayStorage::PyObject(arc_vec) => {
                // Create a new vector and copy all elements
                let mut new_vec: Vec<Py<PyAny>> = Vec::with_capacity(arc_vec.len() + 1);
                for (i, obj) in arc_vec.iter().enumerate() {
                    if i == idx as usize {
                        // Insert the new element at the specified index
                        let item: PyObject = value.clone().into();
                        new_vec.push(item);
                    }
                    // Clone the existing element
                    let item: PyObject = obj.clone_ref(py);
                    new_vec.push(item);
                }
                // Replace the old Arc with a new one containing the updated vector
                self.storage = ArrayStorage::PyObject(Arc::new(new_vec));
            }
        }
        Ok(())
    }

    #[staticmethod]
    pub fn empty(py: Python<'_>, cons: Option<&Bound<'_, PyAny>>) -> PyResult<FSharpArray> {
        // Determine the type from constructor
        let fs_cons = FSharpCons::extract(cons, &ArrayType::Generic);

        // Create an empty array with this type
        let array = fs_cons.allocate(py, 0)?;
        Ok(array)
    }

    pub fn remove_at(
        &mut self,
        py: Python<'_>,
        index: isize,
    ) -> PyResult<FSharpArray> {
        let len = self.__len__();
        let index = if index < 0 { len as isize + index } else { index };

        if index < 0 || index as usize >= len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "index out of range",
            ));
        }

        // Create a new array with the same type and size - 1
        let mut builder = ArrayBuilder::new(&self.nominal_type, Some(len - 1));

        // Copy all elements except the one at the specified index
        for i in 0..len {
            if i as isize != index {
                builder.push_from_storage(&self.storage, i, py);
            }
        }

        // Create the result array
        let result = ArrayBuilder::create_array(builder, &self.nominal_type);
        Ok(result)
    }

    pub fn remove_many_at(
        &mut self,
        py: Python<'_>,
        index: isize,
        count: usize,
    ) -> PyResult<FSharpArray> {
        let len = self.__len__();
        let index = if index < 0 { len as isize + index } else { index };

        if index < 0 || index as usize >= len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "index out of range",
            ));
        }

        // Create a new array with the same type and size - count
        let mut builder = ArrayBuilder::new(&self.nominal_type, Some(len - count));

        // Copy all elements except the ones at the specified indices
        for i in 0..len {
            if i < index as usize || i >= index as usize + count {
                builder.push_from_storage(&self.storage, i, py);
            }
        }

        // Create the result array
        let result = ArrayBuilder::create_array(builder, &self.nominal_type);
        Ok(result)
    }

    // Map implementation (Refactored with ArrayBuilder)
    #[pyo3(signature = (f, cons=None))]
    pub fn map(
        &self,
        py: Python<'_>,
        f: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<Self> {
        let len = self.__len__();

        // Determine target type from cons or preserve source type
        let target_type = if let Some(cons) = cons {
            if let Ok(fs_cons) = cons.extract::<PyRef<'_, FSharpCons>>() {
                fs_cons.array_type.clone()
            } else {
                // Fallback if cons is not a FSharpCons
                self.nominal_type.clone()
            }
        } else {
            // If no constructor is provided, assume the type might change and default to Generic
            ArrayType::Generic
        };

        // Create a helper to collect results based on the target type
        let mut results = ArrayBuilder::new(&target_type, Some(len));

        // Map each element
        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            let mapped = f.call1((item,))?;
            // Push the mapped item into the results collector
            results.push_value(&mapped, py)?;
        }

        // Convert the collected results into the final storage
        Ok(ArrayBuilder::create_array(results, &target_type))
    }

    pub fn map2(
        &self,
        py: Python<'_>,
        f: &Bound<'_, PyAny>,
        source2: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<Self> {
        let len = self.__len__();
        let source2 = ensure_array(py, source2)?;

        // Check lengths match
        if len != source2.__len__() {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "Arrays had different lengths",
            ));
        }

        // Get constructor from cons parameter
        let fs_cons = FSharpCons::extract(cons, &self.nominal_type);
        let mut results = fs_cons.create_builder(len);

        // Map each element with counterpart in source2
        for i in 0..len {
            let item1 = self.get_item_at_index(i as isize, py)?;
            let item2 = source2.get_item_at_index(i as isize, py)?;
            let mapped = f.call1((item1, item2))?;
            results.push_value(&mapped, py)?;
        }

        // Return final array
        Ok(ArrayBuilder::create_array(results, &fs_cons.array_type))
    }

    pub fn map_indexed(
        &self,
        py: Python<'_>,
        f: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<Self> {
        let len = self.__len__();

        // Determine target type from cons or preserve source type
        let target_type = if let Some(cons) = cons {
            if let Ok(fs_cons) = cons.extract::<PyRef<'_, FSharpCons>>() {
                fs_cons.array_type.clone()
            } else {
                // Fallback if cons is not a FSharpCons
                self.nominal_type.clone()
            }
        } else {
            // If no constructor is provided, assume the type might change and default to Generic
            ArrayType::Generic
        };

        // Create a helper to collect results based on the target type
        let mut results = ArrayBuilder::new(&target_type, Some(len));

        // Map each element with its index
        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            let mapped = f.call1((i, item))?;
            // Push the mapped item into the results collector
            results.push_value(&mapped, py)?;
        }

        // Convert the collected results into the final storage
        Ok(ArrayBuilder::create_array(results, &target_type))
    }

    // Filter implementation (Refactored with ArrayBuilder)
    // Expose this method to Python
    #[pyo3(signature = (predicate))]
    pub fn filter(&self, py: Python<'_>, predicate: &Bound<'_, PyAny>) -> PyResult<Self> {
        let len = self.__len__();
        let original_type = self.nominal_type.clone();

        // Create a helper to collect results based on the original type
        let mut results = ArrayBuilder::new(&original_type, None); // No initial capacity needed

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
        Ok(ArrayBuilder::create_array(results, &original_type))
    }

    // Skip implementation using our refined ArrayBuilder
    #[pyo3(signature = (count, cons=None))]
    pub fn skip(
        &self,
        py: Python<'_>,
        count: usize,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let len = self.__len__();
        let fs_cons = FSharpCons::extract(cons, &ArrayType::Generic);
        if len == 0 {
            return Ok(fs_cons.allocate(py, 0)?)
        }
        // Create the builder for results
        let mut results = fs_cons.create_builder(len.saturating_sub(count));

        // Add the remaining elements (after skipping)
        let actual_count = std::cmp::min(count, len);
        for i in actual_count..len {
            // We can use push_original since we're just taking elements from the original array
            results.push_from_storage(&self.storage, i, py);
        }

        // Construct the result array
        Ok(FSharpArray {
            storage: results.into_storage(),
            nominal_type: fs_cons.array_type
        })
    }

    pub fn chunk_by_size(
        &self,
        py: Python<'_>,
        chunk_size: usize,
    ) -> PyResult<Self> {
        if chunk_size < 1 {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "The input must be positive.",
            ));
        }

        let len = self.__len__();
        if len == 0 {
            // Return an empty array
            return Ok(FSharpArray {
                storage: ArrayStorage::PyObject(Arc::new(vec![])),
                nominal_type: ArrayType::Generic,
            });
        }

        // Determine target type from cons or preserve source type
        let target_type= self.nominal_type.clone();

        // Create the builder for results
        let mut results = ArrayBuilder::new(&target_type, None); // No initial capacity needed

        // Add each chunk to the result
        for x in 0..((len + chunk_size - 1) / chunk_size) {
            let start = x * chunk_size;
            let end = std::cmp::min(start + chunk_size, len);
            for i in start..end {
                results.push_from_storage(&self.storage, i, py);
            }
        }

        // Construct the result array
        Ok(FSharpArray {
            storage: results.into_storage(),
            nominal_type: target_type,
        })
    }

    pub fn fill(
        &mut self,
        target_index: isize,
        count: usize,
        value: &Bound<'_, PyAny>,
        py: Python<'_>,
    ) -> PyResult<Py<Self>> {
        // Validate input parameters
        let len = self.__len__();
        let target_index = if target_index < 0 { len as isize + target_index } else { target_index };

        if target_index < 0 || target_index as usize >= len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "target_index out of range",
            ));
        }

        // Make sure we don't go beyond array bounds
        let available_slots = len - target_index as usize;
        let actual_count = std::cmp::min(count, available_slots);

        match &mut self.storage {
            ArrayStorage::PyObject(arc_vec) => {
                // Create a new vector with the same contents
                let mut new_vec = Vec::with_capacity(arc_vec.len());

                // Copy all elements to the new vector, replacing values in the target range
                for (i, obj) in arc_vec.iter().enumerate() {
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
                self.storage = ArrayStorage::PyObject(Arc::new(new_vec));
            }
            _ => {
                // For all other types, use the helper
                ArrayBuilder::fill_storage(&mut self.storage, target_index as usize, actual_count, value)?;
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
        let len = self.__len__();
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
        let len = self.__len__();
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
        let len = self.__len__();
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
        let len = self.__len__();
        let mut acc = state.clone(); // Updated to use clone()

        for i in (0..len).rev() {
            let item = self.get_item_at_index(i as isize, py)?;
            acc = folder.call1((i, item, acc))?;
        }

        Ok(acc.into())
    }

    pub fn sort_in_place(&mut self, py: Python<'_>) -> PyResult<()> {
        match &mut self.storage {
            ArrayStorage::Int8(vec) => vec.sort(),
            ArrayStorage::UInt8(vec) => vec.sort(),
            ArrayStorage::Int16(vec) => vec.sort(),
            ArrayStorage::UInt16(vec) => vec.sort(),
            ArrayStorage::Int32(vec) => vec.sort(),
            ArrayStorage::UInt32(vec) => vec.sort(),
            ArrayStorage::Int64(vec) => vec.sort(),
            ArrayStorage::UInt64(vec) => vec.sort(),
            ArrayStorage::Float32(vec) => vec.sort_by(|a, b| a.partial_cmp(b).unwrap()),
            ArrayStorage::Float64(vec) => vec.sort_by(|a, b| a.partial_cmp(b).unwrap()),
            ArrayStorage::String(vec) => vec.sort(),
            ArrayStorage::PyObject(arc_vec) => {
                let mut new_vec: Vec<Py<PyAny>> = Vec::with_capacity(arc_vec.len());
                for obj in arc_vec.iter() {
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
                                    },
                                    Err(_) => std::cmp::Ordering::Equal // Default to Equal on error
                                }
                            }
                        },
                        Err(_) => std::cmp::Ordering::Equal // Default to Equal on error
                    }
                });

                self.storage = ArrayStorage::PyObject(Arc::new(new_vec));
            }
        }
        Ok(())
    }

    // let sortInPlaceWith (comparer: 'T -> 'T -> int) (xs: 'T[]) =
    //   sortInPlaceWithImpl comparer xs
    //   xs
    pub fn sort_in_place_with(
        &mut self,
        py: Python<'_>,
        compare_func: &Bound<'_, PyAny>,
    ) -> PyResult<()> {
        match &mut self.storage {
            ArrayStorage::PyObject(arc_vec) => {
                let mut new_vec: Vec<Py<PyAny>> = Vec::with_capacity(arc_vec.len());
                for obj in arc_vec.iter() {
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
                                    },
                                    Err(_) => std::cmp::Ordering::Equal // Default to Equal on error
                                }
                            }
                        },
                        Err(_) => std::cmp::Ordering::Equal // Default to Equal on error
                    }
                });
                self.storage = ArrayStorage::PyObject(Arc::new(new_vec));
            }
            _ => {
                // For all other types, use the helper
                ArrayBuilder::sort_storage_with(&mut self.storage, compare_func)?;
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
            if self.__len__() != other_array.__len__() {
                return Ok(false);
            }

            // Compare elements using the provided equals function
            for i in 0..self.__len__() {
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

    pub fn reduce(
        &self,
        py: Python<'_>,
        reduction: &Bound<'_, PyAny>,
    ) -> PyResult<PyObject> {
        let len = self.__len__();
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

    pub fn reduce_back(
        &self,
        py: Python<'_>,
        reduction: &Bound<'_, PyAny>,
    ) -> PyResult<PyObject> {
        let len = self.__len__();
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
            if self.__len__() != other_array.__len__() {
                return Err(PyErr::new::<exceptions::PyValueError, _>(
                    "Arrays must have the same length.",
                ));
            }

            let len = self.__len__();
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
            if self.__len__() != other_array.__len__() {
                return Err(PyErr::new::<exceptions::PyValueError, _>(
                    "Arrays must have the same length.",
                ));
            }

            let len = self.__len__();
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

    pub fn iterate(
        &self,
        py: Python<'_>,
        action: &Bound<'_, PyAny>,
    ) -> PyResult<()> {
        let len = self.__len__();
        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            action.call1((item,))?;
        }
        Ok(())
    }
    pub fn iterate_indexed(
        &self,
        py: Python<'_>,
        action: &Bound<'_, PyAny>,
    ) -> PyResult<()> {
        let len = self.__len__();
        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            action.call1((i, item))?;
        }
        Ok(())
    }

    pub fn sum(
        &self,
        py: Python<'_>,
        adder: &Bound<'_, PyAny>,
    ) -> PyResult<PyObject> {
        let len = self.__len__();
        let mut acc = adder.call_method0("GetZero")?;

        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            acc = adder.call_method1("Add", (acc, item))?;
        }

        Ok(acc.into())
    }

    pub fn pairwise(
        &self,
        py: Python<'_>,
    ) -> PyResult<FSharpArray> {
        let len = self.__len__();
        if len < 2 {
            return Ok(FSharpArray::empty(py, None)?);
        }

        let count = len - 1;
        let builder = ArrayBuilder::new(&self.nominal_type, Some(count));
        let mut result = ArrayBuilder::create_array(builder, &self.nominal_type);

        for i in 0..count {
            let item1 = self.get_item_at_index(i as isize, py)?;
            let item2 = self.get_item_at_index((i + 1) as isize, py)?;
            let tuple = PyTuple::new(py, &[item1, item2])?;
            result.__setitem__(i as isize, &tuple, py)?;
        }

        Ok(result)
    }

    pub fn permute(
        &self,
        py: Python<'_>,
        f: &Bound<'_, PyAny>,
    ) -> PyResult<Py<FSharpArray>> {
        let len = self.__len__();
        let builder = ArrayBuilder::new(&self.nominal_type, Some(len));
        let mut result = ArrayBuilder::create_array(builder, &self.nominal_type);

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
        let len = self.__len__();
        let fs_cons = FSharpCons::extract(cons, &self.nominal_type);
        let mut results = fs_cons.create_builder(len + 1);
        results.push_value(state, py)?;

        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;
            let new_state = folder.call1((item, state))?;
            results.push_value(&new_state, py)?;
        }

        Ok(ArrayBuilder::create_array(results, &self.nominal_type).into_pyobject(py)?.into())
    }

    pub fn scan_back(
        &self,
        py: Python<'_>,
        folder: &Bound<'_, PyAny>,
        state: &Bound<'_, PyAny>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<Py<FSharpArray>> {
        let len = self.__len__();
        let fs_cons = FSharpCons::extract(cons, &self.nominal_type);
        let mut results = fs_cons.create_builder(len + 1);
        results.push_value(state, py)?;

        for i in (0..len).rev() {
            let item = self.get_item_at_index(i as isize, py)?;
            let new_state = folder.call1((item, state))?;
            results.push_value(&new_state, py)?;
        }

        Ok(ArrayBuilder::create_array(results, &self.nominal_type).into_pyobject(py)?.into())
    }

    pub fn split_into(
        &self,
        py: Python<'_>,
        chunks: usize,
    ) -> PyResult<FSharpArray> {
        if chunks < 1 {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "The input must be positive.",
            ));
        }

        let len = self.__len__();
        if len == 0 {
            // Return an empty array
            return Ok(FSharpArray {
                storage: ArrayStorage::PyObject(Arc::new(vec![])),
                nominal_type: ArrayType::Generic,
            });
        }

        // Create the builder for results
        let mut results = ArrayBuilder::new(&self.nominal_type, None); // No initial capacity needed

        // Add each chunk to the result
        for x in 0..((len + chunks - 1) / chunks) {
            let start = x * chunks;
            let end = std::cmp::min(start + chunks, len);
            for i in start..end {
                results.push_from_storage(&self.storage, i, py);
            }
        }

        // Construct the result array
        Ok(FSharpArray {
            storage: results.into_storage(),
            nominal_type: self.nominal_type.clone(),
        })
    }

    pub fn transpose(
        &self,
        py: Python<'_>,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let len = self.__len__();
        if len == 0 {
            return Ok(FSharpArray {
                storage: ArrayStorage::PyObject(Arc::new(vec![])),
                nominal_type: ArrayType::Generic,
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
        let fs_cons = FSharpCons::extract(cons, &self.nominal_type);
        let mut results = fs_cons.create_builder(len_inner);

        // Fill the result array
        for i in 0..len_inner {
            let mut inner_array = fs_cons.allocate(py, len)?;
            for j in 0..len {
                let item = self.get_item_at_index(j as isize, py)?;
                inner_array.__setitem__(i as isize, &item.bind(py), py)?;
            }
            results.push_value(Py::new(py, inner_array)?.bind(py), py)?;
        }

        Ok(ArrayBuilder::create_array(results, &self.nominal_type))
    }

    pub fn try_find_back(
        &self,
        py: Python<'_>,
        predicate: &Bound<'_, PyAny>,
    ) -> PyResult<Option<PyObject>> {
        let len = self.__len__();
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
        let len = self.__len__();
        for i in (0..len).rev() {
            let item = self.get_item_at_index(i as isize, py)?;
            if predicate.call1((item,))?.is_truthy()? {
                return Ok(Some(i));
            }
        }
        Ok(None)
    }

    pub fn windowed(
        &self,
        py: Python<'_>,
        window_size: usize,
    ) -> PyResult<FSharpArray> {
        if window_size == 0 {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "windowSize must be positive.",
            ));
        }

        let len = self.__len__();
        let count = std::cmp::max(0, len as isize - window_size as isize + 1) as usize;

        // Create an array of arrays
        let builder = ArrayBuilder::new(&ArrayType::Generic, Some(count));
        let mut result = ArrayBuilder::create_array(builder, &ArrayType::Generic);

        for i in 0..count {
            // For each window position, create a new array containing window_size elements
            let fs_cons = FSharpCons::new(&self.nominal_type.clone().into_pyobject(py)?.extract::<String>()?)?;
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
        let len = self.__len__();

        // Get the constructor or use default
        let fs_cons = FSharpCons::extract(cons, &self.nominal_type);

        if len == 0 {
            // Return empty array and original state
            let empty_array = fs_cons.allocate(py, 0)?;
            let result_tuple = PyTuple::new(py, &[Py::new(py, empty_array)?.bind(py), &state.clone()]);
            return Ok(result_tuple?.into());
        }

        let mut result_array = fs_cons.allocate(py, len)?;
        let mut current_state = state.clone();

        // Process each element
        for i in 0..len {
            let item = self.get_item_at_index(i as isize, py)?;

            // Call the mapping function: mapping(state, item) -> (mapped_item, new_state)
            let result = mapping.call1((current_state, item))?;

            // Get the first item (mapped result) and second item (new state)
            let mapped_item = result.get_item(0)?;
            current_state = result.get_item(1)?;

            // Set the mapped item in the result array
            result_array.__setitem__(i as isize, &mapped_item, py)?;
        }

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
        let len = self.__len__();

        // Get the constructor or use default
        let fs_cons = FSharpCons::extract(cons, &self.nominal_type);

        if len == 0 {
            // Return empty array and original state
            let empty_array = fs_cons.allocate(py, 0)?;
            let result_tuple = PyTuple::new(py, &[Py::new(py, empty_array)?.bind(py), &state.clone()]);
            return Ok(result_tuple?.into());
        }

        let mut result_array = fs_cons.allocate(py, len)?;
        let mut current_state = state.clone();

        // Process each element in reverse order
        for i in (0..len).rev() {
            let item = self.get_item_at_index(i as isize, py)?;

            // Call the mapping function: mapping(item, state) -> (mapped_item, new_state)
            let result = mapping.call1((item, current_state))?;

            // Get the first item (mapped result) and second item (new state)
            let mapped_item = result.get_item(0)?;
            current_state = result.get_item(1)?;

            // Set the mapped item in the result array
            result_array.__setitem__(i as isize, &mapped_item, py)?;
        }

        // Return tuple of (result_array, final_state)
        let result_tuple = PyTuple::new(py, &[Py::new(py, result_array)?.bind(py), &current_state]);
        Ok(result_tuple?.into())
    }

    // Get the first element of the array
    pub fn head(&self, py: Python<'_>) -> PyResult<PyObject> {
        if self.__len__() == 0 {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "The input array was empty"
            ));
        }
        self.get_item_at_index(0, py)
    }

    // Try to get the first element, returning None if array is empty
    pub fn try_head(&self, py: Python<'_>) -> PyResult<Option<PyObject>> {
        if self.__len__() == 0 {
            Ok(None)
        } else {
            Ok(Some(self.get_item_at_index(0, py)?))
        }
    }

    // Return a new array with all elements except the first one
    pub fn tail(&self, py: Python<'_>, cons: Option<&Bound<'_, PyAny>>) -> PyResult<FSharpArray> {
        if self.__len__() == 0 {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "Not enough elements"
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
        let len = self.__len__();
        let idx = if index < 0 { len as isize + index } else { index };

        if idx < 0 || idx as usize >= len {
            Ok(None)
        } else {
            Ok(Some(self.get_item_at_index(idx, py)?))
        }
    }

    // Then simplify the FSharpArray method
    pub fn reverse(&self, py: Python<'_>) -> PyResult<FSharpArray> {
        // Use the helper method from ArrayBuilder
        let reversed_storage = ArrayBuilder::reverse_storage(&self.storage, py);

        Ok(FSharpArray {
            storage: reversed_storage,
            nominal_type: self.nominal_type.clone(),
        })
    }

    pub fn compare_with(
        &self,
        py: Python<'_>,
        comparer: &Bound<'_, PyAny>,
        other: &FSharpArray,
    ) -> PyResult<bool> {
        // Check if the other object is a FSharpArray
        if self.__len__() != other.__len__() {
            return Ok(false);
        }

        // Compare elements using the provided comparer function
        for i in 0..self.__len__() {
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
        let len = self.__len__();
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

    pub fn exists(
        &self,
        py: Python<'_>,
        predicate: &Bound<'_, PyAny>,
    ) -> PyResult<bool> {
        let len = self.__len__();
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
        let len = self.__len__();
        if index >= len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "Index out of bounds",
            ));
        }

        // Create a new array using the constructor
        let fs_cons = FSharpCons::extract(cons, &self.nominal_type);
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
        &self,  // self is the source array
        py: Python<'_>,
        target: &mut FSharpArray,  // Target array to copy to
        lower: Option<usize>,  // Starting index in target
        upper: Option<usize>,  // Upper bound in target
    ) -> PyResult<()> {
        let lower = lower.unwrap_or(0);
        let upper = upper.unwrap_or(0);

        // Calculate length to copy (following F# logic)
        let length = if upper > 0 {
            upper
        } else {
            target.__len__() - 1
        } - lower;

        // Check if target has enough elements
        // println!("Target length: {}", target.__len__());
        // println!("Length to copy: {}", length);
        if target.__len__() < length {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "Target array is not large enough to hold the copied elements",
            ));
        }

        // Copy elements from source (self) to target
        for i in 0..=length {
            let item = self.get_item_at_index((i+lower) as isize, py)?;
            target.__setitem__((i+lower) as isize, &item.bind(py), py)?;
        }

        Ok(())
    }

    // Format array as F# style string: [1; 2; 3] or [1; 2; 3; ... ] for longer arrays
    pub fn __str__(&self, py: Python<'_>) -> PyResult<String> {
        let len = self.__len__();

        // Empty array case
        if len == 0 {
            return Ok("[]".to_string());
        }

        // Define max elements to show before truncating
        const MAX_DISPLAY_ELEMENTS: usize = 3;
        let show_ellipsis = len > MAX_DISPLAY_ELEMENTS;
        let elements_to_show = if show_ellipsis { MAX_DISPLAY_ELEMENTS } else { len };

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
        let str_result = self.__str__(py)?;
        Ok(format!("'{}'", str_result))
    }

    pub fn insert_at(
        &self,
        py: Python<'_>,
        index: usize,
        value: &Bound<'_, PyAny>,
        _cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let len = self.__len__();
        if index > len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "Index out of bounds",
            ));
        }

        // Create a new array using the constructor
        let target_storage=ArrayBuilder::create_from_storage(&self.storage, py);

        let mut target = FSharpArray {
            storage: target_storage,
            nominal_type: self.nominal_type.clone(),
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
        let len = self.__len__();
        if index > len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>(
                "Index out of bounds",
            ));
        }

        // Create a new array using the constructor
        let target_storage=ArrayBuilder::create_from_storage(&self.storage, py);

        let mut target = FSharpArray {
            storage: target_storage,
            nominal_type: self.nominal_type.clone(),
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
        let len = self.__len__();

        // Check lengths match
        if len != source2.__len__() || source2.__len__() != source3.__len__() {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "Arrays had different lengths",
            ));
        }

        // Get constructor from cons parameter
        let fs_cons = FSharpCons::extract(cons, &self.nominal_type);
        let mut results = fs_cons.create_builder(len);

        // Map each element with counterparts in source2 and source3
        for i in 0..len {
            let item1 = self.get_item_at_index(i as isize, py)?;
            let item2 = source2.get_item_at_index(i as isize, py)?;
            let item3 = source3.get_item_at_index(i as isize, py)?;
            let mapped = f.call1((item1, item2, item3))?;
            results.push_value(&mapped, py)?;
        }

        // Return final array
        Ok(ArrayBuilder::create_array(results, &fs_cons.array_type))
    }

    pub fn map_indexed2(
        &self,
        py: Python<'_>,
        f: &Bound<'_, PyAny>,
        source2: &FSharpArray,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let len = self.__len__();

        // Check lengths match
        if len != source2.__len__() {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "Arrays had different lengths",
            ));
        }

        // Get constructor from cons parameter
        let fs_cons = FSharpCons::extract(cons, &self.nominal_type);
        let mut results = fs_cons.create_builder(len);

        // Map each element with its index and counterpart in source2
        for i in 0..len {
            let item1 = self.get_item_at_index(i as isize, py)?;
            let item2 = source2.get_item_at_index(i as isize, py)?;
            let mapped = f.call1((i, item1, item2))?;
            results.push_value(&mapped, py)?;
        }

        // Return final array
        Ok(ArrayBuilder::create_array(results, &fs_cons.array_type))
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
        if self.nominal_type != array2.nominal_type {
            return Err(PyErr::new::<exceptions::PyValueError, _>(format!(
                "Cannot append arrays of different types: {:?} and {:?}",
                self.nominal_type, array2.nominal_type
            )));
        }

        let len1 = self.__len__();
        let len2 = array2.__len__();

        // Get constructor from cons parameter or use default
        let fs_cons = FSharpCons::extract(cons, &self.nominal_type);
        let mut builder = fs_cons.create_builder(len1 + len2);

        // Copy elements from first array
        for i in 0..len1 {
            builder.push_from_storage(&self.storage, i, py);
        }

        // Copy elements from second array
        for i in 0..len2 {
            builder.push_from_storage(&array2.storage, i, py);
        }

        // Create the final array
        Ok(ArrayBuilder::create_array(builder, &fs_cons.array_type))
    }

    pub fn map_indexed3(
        &self,
        py: Python<'_>,
        f: &Bound<'_, PyAny>,
        source2: &FSharpArray,
        source3: &FSharpArray,
        cons: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<FSharpArray> {
        let len = self.__len__();

        // Check lengths match
        if len != source2.__len__() || source2.__len__() != source3.__len__() {
            return Err(PyErr::new::<exceptions::PyValueError, _>(
                "Arrays had different lengths",
            ));
        }

        // Get constructor from cons parameter
        let fs_cons = FSharpCons::extract(cons, &self.nominal_type);
        let mut results = fs_cons.create_builder(len);

        // Map each element with its index and counterparts in source2 and source3
        for i in 0..len {
            let item1 = self.get_item_at_index(i as isize, py)?;
            let item2 = source2.get_item_at_index(i as isize, py)?;
            let item3 = source3.get_item_at_index(i as isize, py)?;
            let mapped = f.call1((i, item1, item2, item3))?;
            results.push_value(&mapped, py)?;
        }

        // Return final array
        Ok(ArrayBuilder::create_array(results, &fs_cons.array_type))
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
pub fn empty(
    py: Python<'_>,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<FSharpArray> {
    FSharpArray::empty(py, cons)
}

#[pyfunction]
pub fn create(
    py: Python<'_>,
    count: usize,
    value: &Bound<'_, PyAny>
) -> PyResult<FSharpArray> {
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
    array: &Bound<'_, PyAny>,  // Take a PyAny instead of FSharpArray
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
pub fn sort_in_place(
    py: Python<'_>,
    array: &mut FSharpArray,
) -> PyResult<()> {
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
pub fn iterate(
    py: Python<'_>,
    action: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<()> {
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
    array: &Bound<'_, PyAny>,  // Take a PyAny instead of FSharpArray
    adder: &Bound<'_, PyAny>,
) -> PyResult<PyObject> {
    let array = ensure_array(py, array)?;

    // Now call the member function
    array.sum(py, adder)
}

#[pyfunction]
pub fn pairwise(
    py: Python<'_>,
    array: &FSharpArray,
) -> PyResult<FSharpArray> {
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
#[pyo3(signature = (folder, state, array, cons=None))]
pub fn scan_back(
    py: Python<'_>,
    folder: &Bound<'_, PyAny>,
    state: &Bound<'_, PyAny>,
    array: &FSharpArray,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<Py<FSharpArray>> {
    array.scan_back(py, folder, state, cons)
}

#[pyfunction]
pub fn split_into(
    py: Python<'_>,
    chunks: usize,
    array: &FSharpArray,
) -> PyResult<FSharpArray> {
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
pub fn windowed(
    py: Python<'_>,
    window_size: usize,
    array: &FSharpArray,
) -> PyResult<FSharpArray> {
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
#[pyo3(signature = (mapping, state, array, cons=None))]
pub fn map_fold_back(
    py: Python<'_>,
    mapping: &Bound<'_, PyAny>,
    state: &Bound<'_, PyAny>,
    array: &Bound<'_, PyAny>,
    cons: Option<&Bound<'_, PyAny>>,
) -> PyResult<PyObject> {
    let array = ensure_array(py, array)?;
    array.map_fold_back(py, mapping, state, cons)
}

#[pyfunction]
pub fn head(
    py: Python<'_>,
    array: &FSharpArray,
) -> PyResult<PyObject> {
    array.head(py)
}

#[pyfunction]
pub fn try_head(
    py: Python<'_>,
    array: &FSharpArray,
) -> PyResult<Option<PyObject>> {
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
pub fn item(
    py: Python<'_>,
    index: isize,
    array: &FSharpArray,
) -> PyResult<PyObject> {
    array.item(py, index)
}

#[pyfunction]
pub fn try_item(
    py: Python<'_>,
    index: isize,
    array: &FSharpArray,
) -> PyResult<Option<PyObject>> {
    array.try_item(py, index)
}

#[pyfunction]
pub fn reverse(
    py: Python<'_>,
    array: &FSharpArray,
) -> PyResult<FSharpArray> {
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
pub fn exists(
    py: Python<'_>,
    predicate: &Bound<'_, PyAny>,
    array: &FSharpArray,
) -> PyResult<bool> {
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
#[pyo3(signature = (array, index))]
pub fn remove_at(
    py: Python<'_>,
    array: &mut FSharpArray,
    index: isize,
) -> PyResult<FSharpArray> {
    array.remove_at(py, index)
}

#[pyfunction]
#[pyo3(signature = (array, index, count))]
pub fn remove_many_at(
    py: Python<'_>,
    array: &mut FSharpArray,
    index: isize,
    count: usize,
) -> PyResult<FSharpArray> {
    array.remove_many_at(py, index, count)
}

// Constructor class for array allocation
#[pyclass]
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
        let builder = ArrayBuilder::new(&self.array_type, Some(length));
        let array = ArrayBuilder::create_array(builder, &self.array_type);
        Ok(array)
    }

    // Allow calling the constructor directly
    fn __call__(&self, py: Python<'_>, length: usize) -> PyResult<FSharpArray> {
        self.allocate(py, length)
    }
}

impl FSharpCons {
    fn create_builder(
        &self,
        length: usize,
    ) -> ArrayBuilder {
        let builder = ArrayBuilder::new(&self.array_type, Some(length));
        builder
    }

    // Helper function to extract the constructor
    fn extract(
        cons: Option<&Bound<'_, PyAny>>,
        default_type: &ArrayType,
    ) -> FSharpCons {
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
    let mut vec =
        match len {
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

// Buffer protocol implementation removed

// Helper enum for building result arrays efficiently for map/filter
#[derive(Debug)]
enum ArrayBuilder {
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
    Generic(Vec<PyObject>),
}

// Helper macro for sorting with a comparator
macro_rules! sort_with_comparator {
    ($vec:expr, $compare_func:expr) => {
        $vec.sort_by(|a, b| {
            match $compare_func.call1((a, b)).unwrap().extract::<i32>().unwrap() {
                n if n < 0 => std::cmp::Ordering::Less,
                n if n > 0 => std::cmp::Ordering::Greater,
                _ => std::cmp::Ordering::Equal,
            }
        })
    };
}

// Helper macro for filling storage
macro_rules! fill_typed_vec {
    ($vec:expr, $value:expr, $target_index:expr, $count:expr, $type:ty) => {
        {
            let typed_value: $type = $value.extract()?;
            for i in 0..$count {
                $vec[$target_index + i] = *typed_value;
            }
        }
    };
}

// Helper macro for reversing a vector
macro_rules! reverse_vec {
    ($vec:expr) => {
        {
            let mut new_vec = $vec.clone();
            new_vec.reverse();
            new_vec
        }
    };
}

impl ArrayBuilder {
    // Create a new builder for the target type, optionally with capacity
    fn new(array_type: &ArrayType, capacity: Option<usize>) -> Self {
        let cap = capacity.unwrap_or(0); // Default capacity 0 if not specified
        match array_type {
            ArrayType::Int8 => ArrayBuilder::Int8(Vec::with_capacity(cap)),
            ArrayType::UInt8 => ArrayBuilder::UInt8(Vec::with_capacity(cap)),
            ArrayType::Int16 => ArrayBuilder::Int16(Vec::with_capacity(cap)),
            ArrayType::UInt16 => ArrayBuilder::UInt16(Vec::with_capacity(cap)),
            ArrayType::Int32 => ArrayBuilder::Int32(Vec::with_capacity(cap)),
            ArrayType::UInt32 => ArrayBuilder::UInt32(Vec::with_capacity(cap)),
            ArrayType::Int64 => ArrayBuilder::Int64(Vec::with_capacity(cap)),
            ArrayType::UInt64 => ArrayBuilder::UInt64(Vec::with_capacity(cap)),
            ArrayType::Float32 => ArrayBuilder::Float32(Vec::with_capacity(cap)),
            ArrayType::Float64 => ArrayBuilder::Float64(Vec::with_capacity(cap)),
            ArrayType::String => ArrayBuilder::String(Vec::with_capacity(cap)),
            ArrayType::Generic => ArrayBuilder::Generic(Vec::with_capacity(cap)),
        }
    }

    // Create an empty ArrayStorage of the specified type
    fn create_empty_storage(array_type: &ArrayType) -> ArrayStorage {
        match array_type {
            ArrayType::Int8 => ArrayStorage::Int8(Vec::new()),
            ArrayType::UInt8 => ArrayStorage::UInt8(Vec::new()),
            ArrayType::Int16 => ArrayStorage::Int16(Vec::new()),
            ArrayType::UInt16 => ArrayStorage::UInt16(Vec::new()),
            ArrayType::Int32 => ArrayStorage::Int32(Vec::new()),
            ArrayType::UInt32 => ArrayStorage::UInt32(Vec::new()),
            ArrayType::Int64 => ArrayStorage::Int64(Vec::new()),
            ArrayType::UInt64 => ArrayStorage::UInt64(Vec::new()),
            ArrayType::Float32 => ArrayStorage::Float32(Vec::new()),
            ArrayType::Float64 => ArrayStorage::Float64(Vec::new()),
            ArrayType::String => ArrayStorage::String(Vec::new()),
            ArrayType::Generic => ArrayStorage::PyObject(Arc::new(Vec::new())),
        }
    }

    fn create_from_storage(
        storage: &ArrayStorage,
        py: Python<'_>,
    ) -> ArrayStorage {
        match storage {
            ArrayStorage::Int8(vec) => ArrayStorage::Int8(vec.clone()),
            ArrayStorage::UInt8(vec) => ArrayStorage::UInt8(vec.clone()),
            ArrayStorage::Int16(vec) => ArrayStorage::Int16(vec.clone()),
            ArrayStorage::UInt16(vec) => ArrayStorage::UInt16(vec.clone()),
            ArrayStorage::Int32(vec) => ArrayStorage::Int32(vec.clone()),
            ArrayStorage::UInt32(vec) => ArrayStorage::UInt32(vec.clone()),
            ArrayStorage::Int64(vec) => ArrayStorage::Int64(vec.clone()),
            ArrayStorage::UInt64(vec) => ArrayStorage::UInt64(vec.clone()),
            ArrayStorage::Float32(vec) => ArrayStorage::Float32(vec.clone()),
            ArrayStorage::Float64(vec) => ArrayStorage::Float64(vec.clone()),
            ArrayStorage::String(vec) => ArrayStorage::String(vec.clone()),
            // Clone the PyObject references
            ArrayStorage::PyObject(arc_vec) => {
                let new_vec = arc_vec.iter().map(|item| item.clone_ref(py)).collect();
                ArrayStorage::PyObject(Arc::new(new_vec))
            }
        }
    }

    fn create_array(results: Self, array_type: &ArrayType) -> FSharpArray {
        let final_storage = results.into_storage();

        // Construct the final FSharpArray
        FSharpArray {
            storage: final_storage,
            nominal_type: array_type.clone(),
        }
    }

    fn create_empty_array(array_type: &ArrayType) -> FSharpArray {
        FSharpArray {
            storage: Self::create_empty_storage(array_type),
            nominal_type: array_type.clone(),
        }
    }

    // Pushes the original element at `index` from `source_storage` into the builder.
    // Used by `filter`. Assumes `self` matches `source_storage` type.
    fn push_from_storage(&mut self, source_storage: &ArrayStorage, index: usize, py: Python<'_>) {
        // Get type names before the match to avoid borrow-after-move in panic!
        let builder_type_name = std::any::type_name_of_val(self);
        let storage_type_name = std::any::type_name_of_val(source_storage);
        println!("Builder: {:?}", self);
        println!("Storage: {:?}", source_storage);

        match (self, source_storage) {
            (ArrayBuilder::Int8(res), ArrayStorage::Int8(src)) => res.push(src[index]),
            (ArrayBuilder::UInt8(res), ArrayStorage::UInt8(src)) => res.push(src[index]),
            (ArrayBuilder::Int16(res), ArrayStorage::Int16(src)) => res.push(src[index]),
            (ArrayBuilder::UInt16(res), ArrayStorage::UInt16(src)) => res.push(src[index]),
            (ArrayBuilder::Int32(res), ArrayStorage::Int32(src)) => res.push(src[index]),
            (ArrayBuilder::UInt32(res), ArrayStorage::UInt32(src)) => res.push(src[index]),
            (ArrayBuilder::Int64(res), ArrayStorage::Int64(src)) => res.push(src[index]),
            (ArrayBuilder::UInt64(res), ArrayStorage::UInt64(src)) => res.push(src[index]),
            (ArrayBuilder::Float32(res), ArrayStorage::Float32(src)) => res.push(src[index]),
            (ArrayBuilder::Float64(res), ArrayStorage::Float64(src)) => res.push(src[index]),
            (ArrayBuilder::String(res), ArrayStorage::String(src)) => res.push(src[index].clone()),
            (ArrayBuilder::Generic(res), ArrayStorage::PyObject(src)) => res.push(src[index].clone_ref(py)),
            // Add a catch-all for safety, although it shouldn't be reached if types match
            _ => panic!("Mismatched ArrayBuilder and ArrayStorage types in push_from_storage. Builder: {:?}, Storage: {:?}", builder_type_name, storage_type_name),
        }
    }

    // Pushes a mapped item into the builder, performing necessary extraction/conversion.
    // Used by `map`. Assumes `self` matches the target type of the mapped item.
    fn push_value(&mut self, value: &Bound<'_, PyAny>, _py: Python<'_>) -> PyResult<()> {
        match self {
            ArrayBuilder::Int8(vec) => vec.push(value.extract::<Int8>()?.0),
            ArrayBuilder::UInt8(vec) => vec.push(value.extract::<UInt8>()?.0),
            ArrayBuilder::Int16(vec) => vec.push(value.extract::<Int16>()?.0),
            ArrayBuilder::UInt16(vec) => vec.push(value.extract::<UInt16>()?.0),
            ArrayBuilder::Int32(vec) => vec.push(value.extract::<Int32>()?.0),
            ArrayBuilder::UInt32(vec) => vec.push(value.extract::<UInt32>()?.0),
            ArrayBuilder::Int64(vec) => vec.push(value.extract::<Int64>()?.0),
            ArrayBuilder::UInt64(vec) => vec.push(value.extract::<UInt64>()?.0),
            ArrayBuilder::Float32(vec) => vec.push(value.extract::<Float32>()?.0),
            ArrayBuilder::Float64(vec) => vec.push(value.extract::<Float64>()?.0),
            ArrayBuilder::String(vec) => vec.push(value.extract()?),
            // Clone the Bound, then use .into() as Py<PyAny> implements From<Bound<'_, T>>
            ArrayBuilder::Generic(vec) => vec.push(value.clone().into()),
        };
        Ok(())
    }

    // Consumes the builder and returns the final ArrayStorage
    fn into_storage(self) -> ArrayStorage {
        match self {
            ArrayBuilder::Int8(vec) => ArrayStorage::Int8(vec),
            ArrayBuilder::UInt8(vec) => ArrayStorage::UInt8(vec),
            ArrayBuilder::Int16(vec) => ArrayStorage::Int16(vec),
            ArrayBuilder::UInt16(vec) => ArrayStorage::UInt16(vec),
            ArrayBuilder::Int32(vec) => ArrayStorage::Int32(vec),
            ArrayBuilder::UInt32(vec) => ArrayStorage::UInt32(vec),
            ArrayBuilder::Int64(vec) => ArrayStorage::Int64(vec),
            ArrayBuilder::UInt64(vec) => ArrayStorage::UInt64(vec),
            ArrayBuilder::Float32(vec) => ArrayStorage::Float32(vec),
            ArrayBuilder::Float64(vec) => ArrayStorage::Float64(vec),
            ArrayBuilder::String(vec) => ArrayStorage::String(vec),
            ArrayBuilder::Generic(vec) => ArrayStorage::PyObject(Arc::new(vec)),
        }
    }

    fn sort_storage_with(
        storage: &mut ArrayStorage,
        compare_func: &Bound<'_, PyAny>
    ) -> PyResult<()> {
        match storage {
            ArrayStorage::Int8(vec) => sort_with_comparator!(vec, compare_func),
            ArrayStorage::UInt8(vec) => sort_with_comparator!(vec, compare_func),
            ArrayStorage::Int16(vec) => sort_with_comparator!(vec, compare_func),
            ArrayStorage::UInt16(vec) => sort_with_comparator!(vec, compare_func),
            ArrayStorage::Int32(vec) => sort_with_comparator!(vec, compare_func),
            ArrayStorage::UInt32(vec) => sort_with_comparator!(vec, compare_func),
            ArrayStorage::Int64(vec) => sort_with_comparator!(vec, compare_func),
            ArrayStorage::UInt64(vec) => sort_with_comparator!(vec, compare_func),
            ArrayStorage::Float32(vec) => sort_with_comparator!(vec, compare_func),
            ArrayStorage::Float64(vec) => sort_with_comparator!(vec, compare_func),
            ArrayStorage::String(vec) => sort_with_comparator!(vec, compare_func),
            ArrayStorage::PyObject(_) => {
                return Err(PyErr::new::<exceptions::PyValueError, _>(
                    "PyObject storage must be handled separately"
                ));
            }
        }
        Ok(())
    }

    fn fill_storage(
        storage: &mut ArrayStorage,
        target_index: usize,
        count: usize,
        value: &Bound<'_, PyAny>
    ) -> PyResult<()> {
        match storage {
            ArrayStorage::Int8(vec) => fill_typed_vec!(vec, value, target_index, count, Int8),
            ArrayStorage::UInt8(vec) => fill_typed_vec!(vec, value, target_index, count, UInt8),
            ArrayStorage::Int16(vec) => fill_typed_vec!(vec, value, target_index, count, Int16),
            ArrayStorage::UInt16(vec) => fill_typed_vec!(vec, value, target_index, count, UInt16),
            ArrayStorage::Int32(vec) => fill_typed_vec!(vec, value, target_index, count, Int32),
            ArrayStorage::UInt32(vec) => fill_typed_vec!(vec, value, target_index, count, UInt32),
            ArrayStorage::Int64(vec) => fill_typed_vec!(vec, value, target_index, count, Int64),
            ArrayStorage::UInt64(vec) => fill_typed_vec!(vec, value, target_index, count, UInt64),
            ArrayStorage::Float32(vec) => fill_typed_vec!(vec, value, target_index, count, Float32),
            ArrayStorage::Float64(vec) => fill_typed_vec!(vec, value, target_index, count, Float64),
            ArrayStorage::String(vec) => {
                let string_value: String = value.extract()?;
                for i in 0..count {
                    vec[target_index + i] = string_value.clone();
                }
            },
            // We handle PyObject case separately in the main method
            ArrayStorage::PyObject(_) => {
                return Err(PyErr::new::<exceptions::PyValueError, _>(
                    "PyObject storage must be handled separately"
                ));
            }
        }
        Ok(())
    }

    fn reverse_storage(storage: &ArrayStorage, py: Python<'_>) -> ArrayStorage {
        match storage {
            ArrayStorage::Int8(vec) => ArrayStorage::Int8(reverse_vec!(vec)),
            ArrayStorage::UInt8(vec) => ArrayStorage::UInt8(reverse_vec!(vec)),
            ArrayStorage::Int16(vec) => ArrayStorage::Int16(reverse_vec!(vec)),
            ArrayStorage::UInt16(vec) => ArrayStorage::UInt16(reverse_vec!(vec)),
            ArrayStorage::Int32(vec) => ArrayStorage::Int32(reverse_vec!(vec)),
            ArrayStorage::UInt32(vec) => ArrayStorage::UInt32(reverse_vec!(vec)),
            ArrayStorage::Int64(vec) => ArrayStorage::Int64(reverse_vec!(vec)),
            ArrayStorage::UInt64(vec) => ArrayStorage::UInt64(reverse_vec!(vec)),
            ArrayStorage::Float32(vec) => ArrayStorage::Float32(reverse_vec!(vec)),
            ArrayStorage::Float64(vec) => ArrayStorage::Float64(reverse_vec!(vec)),
            ArrayStorage::String(vec) => ArrayStorage::String(reverse_vec!(vec)),
            ArrayStorage::PyObject(arc_vec) => {
                let mut new_vec = Vec::with_capacity(arc_vec.len());
                for i in (0..arc_vec.len()).rev() {
                    new_vec.push(arc_vec[i].clone_ref(py));
                }
                ArrayStorage::PyObject(Arc::new(new_vec))
            }
        }
    }
}
