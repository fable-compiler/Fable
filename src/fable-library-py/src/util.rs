use pyo3::class::basic::CompareOp;
use pyo3::{prelude::*, types::PyAnyMethods};

/// Helper class for sort_in_place_by that applies projection before comparison
#[pyclass(module = "fable")]
pub struct ProjectionComparer {
    projection: Py<PyAny>,
    comparer: Py<PyAny>,
}

/// Helper class for default comparison using Python's built-in comparison
#[pyclass(module = "fable")]
pub struct DefaultComparer;

#[pymethods]
impl ProjectionComparer {
    #[new]
    pub fn new(projection: Py<PyAny>, comparer: Py<PyAny>) -> PyResult<Self> {
        Ok(ProjectionComparer {
            projection,
            comparer,
        })
    }

    pub fn __call__(
        &self,
        py: Python<'_>,
        x: &Bound<'_, PyAny>,
        y: &Bound<'_, PyAny>,
    ) -> PyResult<i32> {
        // Apply projection to both values
        let projected_x = self.projection.bind(py).call1((x,))?;
        let projected_y = self.projection.bind(py).call1((y,))?;

        // Compare the projected values using the comparer
        let result = self
            .comparer
            .bind(py)
            .call_method1("Compare", (projected_x, projected_y))?;
        result.extract()
    }
}

#[pymethods]
impl DefaultComparer {
    #[new]
    pub fn new() -> PyResult<Self> {
        Ok(DefaultComparer)
    }

    pub fn __call__(
        &self,
        _py: Python<'_>,
        x: &Bound<'_, PyAny>,
        y: &Bound<'_, PyAny>,
    ) -> PyResult<i32> {
        // Use Python's built-in comparison: (x > y) - (x < y)
        let gt = x.rich_compare(y, CompareOp::Gt)?.is_truthy()?;
        let lt = x.rich_compare(y, CompareOp::Lt)?.is_truthy()?;

        Ok(if gt {
            1
        } else if lt {
            -1
        } else {
            0
        })
    }
}
