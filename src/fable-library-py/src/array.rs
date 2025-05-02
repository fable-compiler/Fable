use std::sync::Arc;
use pyo3::class::basic::CompareOp;
use pyo3::types::PyTuple;
use crate::floats::{Float32, Float64};
use crate::ints::{Int16, Int32, Int64, Int8, UInt16, UInt32, UInt64, UInt8};
use pyo3::exceptions;
use pyo3::{
    prelude::*,
    types::{PyAnyMethods, PyList, PyListMethods},
};

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

#[pyclass(module = "fable")]
#[derive(Clone)]
pub struct FSharpArray {
    storage: ArrayStorage,
    nominal_type: ArrayType,
}

#[pymethods]
impl FSharpArray {
    #[new]
    #[pyo3(signature = (elements=None, array_type=None))]
    pub fn new(
        py: Python<'_>,
        elements: Option<&Bound<'_, PyList>>,
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
                }
                ArrayType::UInt8 => {
                    if let Ok(vec) = extract_typed_vec::<UInt8, u8>(elements, |x| Ok(*x)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::UInt8(vec),
                            nominal_type,
                        });
                    }
                }
                ArrayType::Int16 => {
                    if let Ok(vec) = extract_typed_vec::<Int16, i16>(elements, |x| Ok(*x)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::Int16(vec),
                            nominal_type,
                        });
                    }
                }
                ArrayType::UInt16 => {
                    if let Ok(vec) = extract_typed_vec::<UInt16, u16>(elements, |x| Ok(*x)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::UInt16(vec),
                            nominal_type,
                        });
                    }
                }
                ArrayType::Int32 => {
                    if let Ok(vec) = extract_typed_vec::<Int32, i32>(elements, |x| Ok(*x)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::Int32(vec),
                            nominal_type,
                        });
                    }
                }
                ArrayType::UInt32 => {
                    if let Ok(vec) = extract_typed_vec::<UInt32, u32>(elements, |x| Ok(*x)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::UInt32(vec),
                            nominal_type,
                        });
                    }
                }
                ArrayType::Int64 => {
                    if let Ok(vec) = extract_typed_vec::<Int64, i64>(elements, |x| Ok(*x)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::Int64(vec),
                            nominal_type,
                        });
                    }
                }
                ArrayType::UInt64 => {
                    if let Ok(vec) = extract_typed_vec::<UInt64, u64>(elements, |x| Ok(*x)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::UInt64(vec),
                            nominal_type,
                        });
                    }
                }
                ArrayType::Float32 => {
                    if let Ok(vec) = extract_typed_vec::<Float32, f32>(elements, |x| Ok(*x)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::Float32(vec),
                            nominal_type,
                        });
                    }
                }
                ArrayType::Float64 => {
                    if let Ok(vec) = extract_typed_vec::<Float64, f64>(elements, |x| Ok(*x)) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::Float64(vec),
                            nominal_type,
                        });
                    }
                }
                ArrayType::String => {
                    if let Ok(vec) = extract_typed_vec::<String, String>(elements, Ok) {
                        return Ok(FSharpArray {
                            storage: ArrayStorage::String(vec),
                            nominal_type,
                        });
                    }
                }
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
            Ok(ArrayBuilder::create_empty_array(&nominal_type))
        }
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

    pub fn __getitem__(&self, idx: isize, py: Python<'_>) -> PyResult<PyObject> {
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
                let int8: Int8 = value.extract()?;
                vec[idx as usize] = *int8;
            }
            ArrayStorage::UInt8(vec) => {
                let uint8: UInt8 = value.extract()?;
                vec[idx as usize] = *uint8;
            }
            ArrayStorage::Int16(vec) => {
                let int16: Int16 = value.extract()?;
                vec[idx as usize] = *int16;
            }
            ArrayStorage::UInt16(vec) => {
                let uint16: UInt16 = value.extract()?;
                vec[idx as usize] = *uint16;
            }
            ArrayStorage::Int32(vec) => {
                let int32: Int32 = value.extract()?;
                vec[idx as usize] = *int32;
            }
            ArrayStorage::UInt32(vec) => {
                let uint32: UInt32 = value.extract()?;
                vec[idx as usize] = *uint32;
            }
            ArrayStorage::Int64(vec) => {
                let int64: Int64 = value.extract()?;
                vec[idx as usize] = *int64;
            }
            ArrayStorage::UInt64(vec) => {
                let uint64: UInt64 = value.extract()?;
                vec[idx as usize] = *uint64;
            }
            ArrayStorage::Float32(vec) => {
                let float32: Float32 = value.extract()?;
                vec[idx as usize] = *float32;
            }
            ArrayStorage::Float64(vec) => {
                let float64: Float64 = value.extract()?;
                vec[idx as usize] = *float64;
            }
            ArrayStorage::String(vec) => {
                vec[idx as usize] = value.extract()?;
            }
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
            let item = self.__getitem__(i as isize, py)?;
            let mapped = f.call1((item,))?;
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
            let item_obj = self.__getitem__(i as isize, py)?;
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

    pub fn fold_back(
        &self,
        py: Python<'_>,
        folder: &Bound<'_, PyAny>,
        state: &Bound<'_, PyAny>,
    ) -> PyResult<PyObject> {
        let len = self.__len__();
        let mut acc = state.clone();

        for i in (0..len).rev() {
            let item = self.__getitem__(i as isize, py)?;
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
            let item = self.__getitem__(i as isize, py)?;
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

    // let equalsWith (equals: 'T -> 'T -> bool) (array1: 'T[]) (array2: 'T[])
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
                let item1 = self.__getitem__(i as isize, py)?;
                let item2 = other_array.__getitem__(i as isize, py)?;

                let result = equals_func.call1((item1, item2))?;
                if !result.is_truthy()? {
                    return Ok(false);
                }
            }
            Ok(true)
        } else {
            Err(PyErr::new::<exceptions::PyTypeError, _>(
                "The second argument must be a FSharpArray.",
            ))
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
        let mut acc = self.__getitem__(0, py)?;

        for i in 1..len {
            let item = self.__getitem__(i as isize, py)?;
            acc = reduction.call1((acc, item))?.into();
        }

        Ok(acc.into())
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
                let item1 = self.__getitem__(i as isize, py)?;
                let item2 = other_array.__getitem__(i as isize, py)?;
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
                let item1 = self.__getitem__(i as isize, py)?;
                let item2 = other_array.__getitem__(i as isize, py)?;
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
            let item = self.__getitem__(i as isize, py)?;
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
            let item = self.__getitem__(i as isize, py)?;
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
        let mut acc = adder.call0()?;

        for i in 0..len {
            let item = self.__getitem__(i as isize, py)?;
            acc = adder.call1((acc, item))?;
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
            let item1 = self.__getitem__(i as isize, py)?;
            let item2 = self.__getitem__((i + 1) as isize, py)?;
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
            let item = self.__getitem__(i as isize, py)?;
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
            let item = self.__getitem__(i as isize, py)?;
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
            let item = self.__getitem__(i as isize, py)?;
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

        let first_array = self.__getitem__(0, py)?;
        let len_inner = first_array.bind(py).len()?;

        // Check if all inner arrays have the same length
        for i in 1..len {
            let item = self.__getitem__(i as isize, py)?;
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
                let item = self.__getitem__(j as isize, py)?;
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
            let item = self.__getitem__(i as isize, py)?;
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
            let item = self.__getitem__(i as isize, py)?;
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
                let item = self.__getitem__((i + j) as isize, py)?;
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
            let item = self.__getitem__(i as isize, py)?;

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
            let item = self.__getitem__(i as isize, py)?;

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
        self.__getitem__(0, py)
    }

    // Try to get the first element, returning None if array is empty
    pub fn try_head(&self, py: Python<'_>) -> PyResult<Option<PyObject>> {
        if self.__len__() == 0 {
            Ok(None)
        } else {
            Ok(Some(self.__getitem__(0, py)?))
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
        self.__getitem__(index, py)
    }

    // Try to get an item at a specific index, returning None if out of bounds
    pub fn try_item(&self, py: Python<'_>, index: isize) -> PyResult<Option<PyObject>> {
        let len = self.__len__();
        let idx = if index < 0 { len as isize + index } else { index };

        if idx < 0 || idx as usize >= len {
            Ok(None)
        } else {
            Ok(Some(self.__getitem__(idx, py)?))
        }
    }
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

// Utility function to extract typed vectors
fn extract_typed_vec<T, U>(
    elements: &Bound<'_, PyList>,
    extractor: impl Fn(T) -> PyResult<U>,
) -> PyResult<Vec<U>>
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
            _ => panic!("Mismatched ArrayBuilder and ArrayStorage types in push_original. Builder: {:?}, Storage: {:?}", builder_type_name, storage_type_name),
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

}
