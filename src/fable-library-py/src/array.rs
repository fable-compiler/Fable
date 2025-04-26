use std::sync::Arc;

use pyo3::{prelude::*, types::{PyAnyMethods, PyList, PyListMethods}, };
use crate::ints::{Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64};
use crate::floats::{Float32, Float64};
use pyo3::exceptions;


/// A module for array operations
pub fn register_array_module(parent_module: &Bound<'_, PyModule>) -> PyResult<()> {
    let m = PyModule::new(parent_module.py(), "array")?;

    m.add_function(wrap_pyfunction!(allocate_array_from_cons, &m)?)?;
    m.add_class::<FSharpArray>()?;
    m.add_class::<FSharpCons>()?;

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
#[derive(Clone)]
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

#[pyclass(module="fable")]
#[derive(Clone)]
pub struct FSharpArray {
    storage: ArrayStorage,
    nominal_type: ArrayType,
}

#[pymethods]
impl FSharpArray {
    #[new]
    #[pyo3(signature = (elements=None, array_type=None))]
    pub fn new(py: Python<'_>, elements: Option<&Bound<'_, PyList>>, array_type: Option<&str>) -> PyResult<Self> {
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
            let len = elements.len();

            // Try to create specialized storage if possible
            match &nominal_type {
                ArrayType::Int8 => {
                    if let Ok(vec) = extract_typed_vec::<Int8, i8>(elements, |x| {
                        // Use Deref trait to access the internal value directly
                        Ok(*x)
                    }) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::Int8(vec),
                            nominal_type,
                        });
                    }
                },
                ArrayType::UInt8 => {
                    if let Ok(vec) = extract_typed_vec::<UInt8, u8>(elements, |x| {
                        Ok(*x)
                    }) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::UInt8(vec),
                            nominal_type,
                        });
                    }
                },
                ArrayType::Int16 => {
                    if let Ok(vec) = extract_typed_vec::<Int16, i16>(elements, |x| {
                        Ok(*x)
                    }) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::Int16(vec),
                            nominal_type,
                        });
                    }
                },
                ArrayType::UInt16 => {
                    if let Ok(vec) = extract_typed_vec::<UInt16, u16>(elements, |x| {
                        Ok(*x)
                    }) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::UInt16(vec),
                            nominal_type,
                        });
                    }
                },
                ArrayType::Int32 => {
                    if let Ok(vec) = extract_typed_vec::<Int32, i32>(elements, |x| {
                        Ok(*x)
                    }) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::Int32(vec),
                            nominal_type,
                        });
                    }
                },
                ArrayType::UInt32 => {
                    if let Ok(vec) = extract_typed_vec::<UInt32, u32>(elements, |x| {
                        Ok(*x)
                    }) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::UInt32(vec),
                            nominal_type,
                        });
                    }
                },
                ArrayType::Int64 => {
                    if let Ok(vec) = extract_typed_vec::<Int64, i64>(elements, |x| {
                        Ok(*x)
                    }) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::Int64(vec),
                            nominal_type,
                        });
                    }
                },
                ArrayType::UInt64 => {
                    if let Ok(vec) = extract_typed_vec::<UInt64, u64>(elements, |x| {
                        Ok(*x)
                    }) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::UInt64(vec),
                            nominal_type,
                        });
                    }
                },
                ArrayType::Float32 => {
                    if let Ok(vec) = extract_typed_vec::<Float32, f32>(elements, |x| Ok(*x)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::Float32(vec),
                            nominal_type,
                        });
                    }
                },
                ArrayType::Float64 => {
                    if let Ok(vec) = extract_typed_vec::<Float64, f64>(elements, |x| Ok(*x)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::Float64(vec),
                            nominal_type,
                        });
                    }
                },
                ArrayType::String => {
                    if let Ok(vec) = extract_typed_vec::<String, String>(elements, Ok) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::String(vec),
                            nominal_type,
                        });
                    }
                },
                _ => {}
            }

            // Fallback to PyObject storage
            let mut vec = Vec::with_capacity(len);
            for item in elements.iter() {
                vec.push(item.into_pyobject(py)?.into());
            }

            Ok(FSharpArray {
                storage: ArrayStorage::PyObject(Arc::new(vec)),
                nominal_type,
            })
        } else {
            // Empty array - create with the right type but no elements
            match nominal_type {
                ArrayType::Int8 => Ok(FSharpArray {
                    storage: ArrayStorage::Int8(Vec::new()),
                    nominal_type,
                }),
                ArrayType::UInt8 => Ok(FSharpArray {
                    storage: ArrayStorage::UInt8(Vec::new()),
                    nominal_type,
                }),
                ArrayType::Int16 => Ok(FSharpArray {
                    storage: ArrayStorage::Int16(Vec::new()),
                    nominal_type,
                }),
                ArrayType::UInt16 => Ok(FSharpArray {
                    storage: ArrayStorage::UInt16(Vec::new()),
                    nominal_type,
                }),
                ArrayType::Int32 => Ok(FSharpArray {
                    storage: ArrayStorage::Int32(Vec::new()),
                    nominal_type,
                }),
                ArrayType::UInt32 => Ok(FSharpArray {
                    storage: ArrayStorage::UInt32(Vec::new()),
                    nominal_type,
                }),
                ArrayType::Int64 => Ok(FSharpArray {
                    storage: ArrayStorage::Int64(Vec::new()),
                    nominal_type,
                }),
                ArrayType::UInt64 => Ok(FSharpArray {
                    storage: ArrayStorage::UInt64(Vec::new()),
                    nominal_type,
                }),
                ArrayType::Float32 => Ok(FSharpArray {
                    storage: ArrayStorage::Float32(Vec::new()),
                    nominal_type,
                }),
                ArrayType::Float64 => Ok(FSharpArray {
                    storage: ArrayStorage::Float64(Vec::new()),
                    nominal_type,
                }),
                ArrayType::String => Ok(FSharpArray {
                    storage: ArrayStorage::String(Vec::new()),
                    nominal_type,
                }),
                _ => Ok(FSharpArray {
                    storage: ArrayStorage::PyObject(Arc::new(Vec::new())),
                    nominal_type,
                }),
            }
        }
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

    pub fn __getitem__(&self, idx: isize, py: Python<'_>) -> PyResult<PyObject> {
        let len = self.__len__();
        let idx = if idx < 0 { len as isize + idx } else { idx };

        if idx < 0 || idx as usize >= len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>("index out of range"));
        }

        match &self.storage {
            ArrayStorage::Int8(vec) => {
                let value = vec[idx as usize];
                // Construct the Fable wrapper and convert to PyObject
                Ok(Int8(value).into_pyobject(py)?.into())
            },
            ArrayStorage::UInt8(vec) => {
                let value = vec[idx as usize];
                Ok(UInt8(value).into_pyobject(py)?.into())
            },
            ArrayStorage::Int16(vec) => {
                let value = vec[idx as usize];
                Ok(Int16(value).into_pyobject(py)?.into())
            },
            ArrayStorage::UInt16(vec) => {
                let value = vec[idx as usize];
                Ok(UInt16(value).into_pyobject(py)?.into())
            },
            ArrayStorage::Int32(vec) => {
                let value = vec[idx as usize];
                Ok(Int32(value).into_pyobject(py)?.into())
            },
            ArrayStorage::UInt32(vec) => {
                let value = vec[idx as usize];
                Ok(UInt32(value).into_pyobject(py)?.into())
            },
            ArrayStorage::Int64(vec) => {
                let value = vec[idx as usize];
                Ok(Int64(value).into_pyobject(py)?.into())
            },
            ArrayStorage::UInt64(vec) => {
                let value = vec[idx as usize];
                Ok(UInt64(value).into_pyobject(py)?.into())
            },
            ArrayStorage::Float32(vec) => {
                let value = vec[idx as usize];
                Ok(Float32(value).into_pyobject(py)?.into())
            },
            ArrayStorage::Float64(vec) => {
                let value = vec[idx as usize];
                Ok(Float64(value).into_pyobject(py)?.into())
            },
            ArrayStorage::String(vec) => Ok(vec[idx as usize].clone().into_pyobject(py)?.into()),
            ArrayStorage::PyObject(vec) => Ok(vec[idx as usize].clone_ref(py)),
        }
    }

    pub fn __setitem__(&mut self, idx: isize, value: &Bound<'_, PyAny>, py: Python<'_>) -> PyResult<()> {
        let len = self.__len__();
        let idx = if idx < 0 { len as isize + idx } else { idx };

        if idx < 0 || idx as usize >= len {
            return Err(PyErr::new::<exceptions::PyIndexError, _>("index out of range"));
        }

        match &mut self.storage {
            ArrayStorage::Int8(vec) => {
                let int8: Int8 = value.extract()?;
                vec[idx as usize] = *int8;
            },
            ArrayStorage::UInt8(vec) => {
                let uint8: UInt8 = value.extract()?;
                vec[idx as usize] = *uint8;
            },
            ArrayStorage::Int16(vec) => {
                let int16: Int16 = value.extract()?;
                vec[idx as usize] = *int16;
            },
            ArrayStorage::UInt16(vec) => {
                let uint16: UInt16 = value.extract()?;
                vec[idx as usize] = *uint16;
            },
            ArrayStorage::Int32(vec) => {
                let int32: Int32 = value.extract()?;
                vec[idx as usize] = *int32;
            },
            ArrayStorage::UInt32(vec) => {
                let uint32: UInt32 = value.extract()?;
                vec[idx as usize] = *uint32;
            },
            ArrayStorage::Int64(vec) => {
                let int64: Int64 = value.extract()?;
                vec[idx as usize] = *int64;
            },
            ArrayStorage::UInt64(vec) => {
                let uint64: UInt64 = value.extract()?;
                vec[idx as usize] = *uint64;
            },
            ArrayStorage::Float32(vec) => {
                let float32: Float32 = value.extract()?;
                vec[idx as usize] = *float32;
            },
            ArrayStorage::Float64(vec) => {
                let float64: Float64 = value.extract()?;
                vec[idx as usize] = *float64;
            },
            ArrayStorage::String(vec) => {
                vec[idx as usize] = value.extract()?;
            },
            ArrayStorage::PyObject(arc_vec) => {
                // Create a new vector and copy all elements
                // Explicitly type new_vec as Vec<Py<PyAny>> (PyObject)
                let mut new_vec: Vec<Py<PyAny>> = Vec::with_capacity(arc_vec.len());
                for (i, obj) in arc_vec.iter().enumerate() {
                    if i == idx as usize {
                        // Replace the element at the specified index
                        // Clone the Bound reference to get an owned Bound, then convert to PyObject
                        let item: PyObject = value.clone().into();
                        new_vec.push(item);
                    } else {
                        // Clone the existing element
                        let item: PyObject = obj.clone_ref(py);
                        new_vec.push(item);
                    }
                }
                // Replace the old Arc with a new one containing the updated vector
                self.storage = ArrayStorage::PyObject(Arc::new(new_vec));
            },
        }

        Ok(())
    }

    // Map implementation (Refactored with ArrayBuilder)
    #[pyo3(signature = (f, cons=None))]
    pub fn map(&self, py: Python<'_>, f: &Bound<'_, PyAny>, cons: Option<&Bound<'_, PyAny>>) -> PyResult<Self> {
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
            let item = self.__getitem__(i as isize, py)?;
            let mapped = f.call1((item,))?;
            // Push the mapped item into the results collector
            results.push_mapped(&mapped, py)?;
        }

        // Convert the collected results into the final storage
        let final_storage = results.into_storage();

        // Construct the final FSharpArray
        Ok(FSharpArray {
            storage: final_storage,
            nominal_type: target_type,
        })
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
            let item_obj = self.__getitem__(i as isize, py)?;
            let keep = predicate.call1((item_obj.clone_ref(py),))?.is_truthy()?;

            if keep {
                // Push the original item (by index) into the results collector
                results.push_original(&self.storage, i, py);
            }
        }

        // Convert the collected results into the final storage
        let final_storage = results.into_storage();

        Ok(FSharpArray {
            storage: final_storage,
            nominal_type: original_type, // Keep the original nominal type
        })
    }
}

// Constructor class for array allocation
#[pyclass]
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
    pub fn allocate(&self, py: Python<'_>, length: usize) -> PyResult<Py<FSharpArray>> {
        let array = match &self.array_type {  // Use &self.array_type to avoid moving
            ArrayType::Int8 => FSharpArray {
                storage: ArrayStorage::Int8(vec![0; length]),
                nominal_type: ArrayType::Int8,
            },
            ArrayType::UInt8 => FSharpArray {
                storage: ArrayStorage::UInt8(vec![0; length]),
                nominal_type: ArrayType::UInt8,
            },
            ArrayType::Int16 => FSharpArray {
                storage: ArrayStorage::Int16(vec![0; length]),
                nominal_type: ArrayType::Int16,
            },
            ArrayType::UInt16 => FSharpArray {
                storage: ArrayStorage::UInt16(vec![0; length]),
                nominal_type: ArrayType::UInt16,
            },
            ArrayType::Int32 => FSharpArray {
                storage: ArrayStorage::Int32(vec![0; length]),
                nominal_type: ArrayType::Int32,
            },
            ArrayType::UInt32 => FSharpArray {
                storage: ArrayStorage::UInt32(vec![0; length]),
                nominal_type: ArrayType::UInt32,
            },
            ArrayType::Int64 => FSharpArray {
                storage: ArrayStorage::Int64(vec![0; length]),
                nominal_type: ArrayType::Int64,
            },
            ArrayType::UInt64 => FSharpArray {
                storage: ArrayStorage::UInt64(vec![0; length]),
                nominal_type: ArrayType::UInt64,
            },
            ArrayType::Float32 => FSharpArray {
                storage: ArrayStorage::Float32(vec![0.0; length]),
                nominal_type: ArrayType::Float32,
            },
            ArrayType::Float64 => FSharpArray {
                storage: ArrayStorage::Float64(vec![0.0; length]),
                nominal_type: ArrayType::Float64,
            },
            ArrayType::String => FSharpArray {
                storage: ArrayStorage::String(vec![String::new(); length]),
                nominal_type: ArrayType::String,
            },
            _ => {
                // Create an empty vector and fill it with None values
                let mut vec = Vec::with_capacity(length);
                let none_obj = py.None(); // Get the None object
                for _ in 0..length {
                    vec.push(none_obj.clone_ref(py).into()); // Clone the reference and convert to PyObject
                }
                FSharpArray {
                    storage: ArrayStorage::PyObject(Arc::new(vec)),
                    nominal_type: self.array_type.clone(),  // Clone to avoid moving
                }
            },
        };
        Py::new(py, array)
    }

    // Allow calling the constructor directly
    fn __call__(&self, py: Python<'_>, length: usize) -> PyResult<Py<FSharpArray>> {
        self.allocate(py, length)
    }
}

// Helper function for allocating arrays
#[pyfunction]
pub fn allocate_array_from_cons(py: Python<'_>, cons: Option<&Bound<'_, PyAny>>, length: usize) -> PyResult<Py<FSharpArray>> {
    if let Some(cons) = cons {
        // Try to call the constructor (e.g., FSharpCons.__call__)
        match cons.call1((length,)) {
            Ok(result_array_obj) => {
                // If the call succeeds, try to extract the Py<FSharpArray>
                match result_array_obj.extract::<Py<FSharpArray>>() {
                    Ok(py_fsharp_array) => Ok(py_fsharp_array),
                    Err(e) => Err(PyErr::new::<exceptions::PyTypeError, _>(
                        format!("Constructor returned unexpected type: {}", e)
                    ))
                }
            },
            Err(e) => {
                // Propagate error if the call itself failed
                 Err(PyErr::new::<exceptions::PyTypeError, _>(
                    format!("Constructor call failed: {}", e)
                ))
            }
        }
    } else {
        // Create a generic array if no constructor is provided
        let mut vec = Vec::with_capacity(length);
        let none_obj = py.None(); // Get the None object
        for _ in 0..length {
            vec.push(none_obj.clone_ref(py).into()); // Clone the reference and convert to PyObject
        }
        let array = FSharpArray {
            storage: ArrayStorage::PyObject(Arc::new(vec)),
            nominal_type: ArrayType::Generic,
        };
        Py::new(py, array)
    }
}

// Utility function to extract typed vectors
fn extract_typed_vec<T, U>(elements: &Bound<'_, PyList>, extractor: impl Fn(T) -> PyResult<U>) -> PyResult<Vec<U>>
where
    T: for<'a> pyo3::FromPyObject<'a>,
{
    let mut vec = Vec::with_capacity(elements.len());
    for item in elements.iter() {
        let typed_item = item.extract::<T>()?;
        vec.push(extractor(typed_item)?);
    }
    Ok(vec)
}

// Helper enum for building result arrays efficiently for map/filter
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

    // Pushes the original element at `index` from `source_storage` into the builder.
    // Used by `filter`. Assumes `self` matches `source_storage` type.
    fn push_original(&mut self, source_storage: &ArrayStorage, index: usize, py: Python<'_>) {
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
            _ => panic!("Mismatched ArrayBuilder and ArrayStorage types in push_original. Builder: {:?}, Storage: {:?}", std::any::type_name_of_val(self), std::any::type_name_of_val(source_storage)),
        }
    }

    // Pushes a mapped item into the builder, performing necessary extraction/conversion.
    // Used by `map`. Assumes `self` matches the target type of the mapped item.
    fn push_mapped(&mut self, mapped_item: &Bound<'_, PyAny>, _py: Python<'_>) -> PyResult<()> {
         match self {
            ArrayBuilder::Int8(vec) => vec.push(mapped_item.extract::<Int8>()?.0),
            ArrayBuilder::UInt8(vec) => vec.push(mapped_item.extract::<UInt8>()?.0),
            ArrayBuilder::Int16(vec) => vec.push(mapped_item.extract::<Int16>()?.0),
            ArrayBuilder::UInt16(vec) => vec.push(mapped_item.extract::<UInt16>()?.0),
            ArrayBuilder::Int32(vec) => vec.push(mapped_item.extract::<Int32>()?.0),
            ArrayBuilder::UInt32(vec) => vec.push(mapped_item.extract::<UInt32>()?.0),
            ArrayBuilder::Int64(vec) => vec.push(mapped_item.extract::<Int64>()?.0),
            ArrayBuilder::UInt64(vec) => vec.push(mapped_item.extract::<UInt64>()?.0),
            ArrayBuilder::Float32(vec) => vec.push(mapped_item.extract::<Float32>()?.0),
            ArrayBuilder::Float64(vec) => vec.push(mapped_item.extract::<Float64>()?.0),
            ArrayBuilder::String(vec) => vec.push(mapped_item.extract()?),
            // Clone the Bound, then use .into() as Py<PyAny> implements From<Bound<'_, T>>
            ArrayBuilder::Generic(vec) => vec.push(mapped_item.clone().into()),
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
}
