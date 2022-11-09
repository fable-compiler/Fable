cdef class TypeInfo:
    cdef str fullname
    cdef list generics

cdef TypeInfo string_type
cdef TypeInfo int32_type
cdef TypeInfo record_type