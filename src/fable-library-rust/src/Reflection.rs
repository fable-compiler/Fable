pub mod Reflection_ {
    pub use core::any::TypeId; // re-export

    use crate::Native_::{box_, Any, Func1, LrcPtr, Vec};
    use crate::NativeArray_::{array_from, Array};
    use crate::String_::string;

    #[cfg(not(feature = "no_std"))]
    use core::cell::RefCell;
    #[cfg(not(feature = "no_std"))]
    use std::collections::HashMap;

    pub fn name<T: Clone>() -> string {
        // TODO: map some common type names to .NET type names
        string(core::any::type_name::<T>())
    }

    // The object (System.Object / obj) representation on the Rust target.
    type obj = LrcPtr<dyn Any>;

    // Compile-time helper emitted by `typeof<T>` to obtain a concrete type id.
    pub fn type_id<T: 'static>() -> TypeId {
        TypeId::of::<T>()
    }

    // PropertyInfo-like carrier for a single record field.
    // `get` downcasts the boxed record and reads the field, returning it boxed.
    #[derive(Clone)]
    pub struct RecordFieldInfo {
        pub name: string,
        pub get: Func1<obj, obj>,
    }

    // Rich reflection info attached to `typeof<Record>`. Carries everything
    // needed for MakeRecord (the `make` constructor closure) as well as
    // GetRecordFields/GetRecordField (field names + per-field getters).
    // This mirrors the JS/TS model where the reflected Type is an object
    // carrying field metadata, so the same F# reflection code behaves the same.
    #[derive(Clone)]
    pub struct RecordTypeInfo {
        pub tid: TypeId,
        pub name: string,
        pub fields: Array<LrcPtr<RecordFieldInfo>>,
        pub make: Func1<Array<obj>, obj>,
    }

    // Registry mapping a record's *concrete* TypeId to its reflection info.
    // Populated when `typeof<Record>` is evaluated. This is what makes
    // value-based reflection (GetRecordFields(record)) possible: from a bare
    // boxed record we can recover its runtime TypeId and look the info up.
    //
    // `thread_local!` is std-only, so under `no_std` there is no registry and
    // the value-first entry points degrade (like exception catching does).
    // Type-first reflection (typeof<T>, MakeRecord, GetRecordElements) is
    // unaffected, as those carry the info in the `System.Type` value itself.
    #[cfg(not(feature = "no_std"))]
    thread_local! {
        static RECORD_REGISTRY: RefCell<HashMap<TypeId, RecordTypeInfo>> =
            RefCell::new(HashMap::new());
    }

    #[cfg(not(feature = "no_std"))]
    fn registry_insert(tid: TypeId, info: RecordTypeInfo) {
        RECORD_REGISTRY.with(|r| {
            r.borrow_mut().insert(tid, info);
        });
    }

    #[cfg(feature = "no_std")]
    fn registry_insert(_tid: TypeId, _info: RecordTypeInfo) {
        // no registry when no_std
    }

    #[cfg(not(feature = "no_std"))]
    fn registry_get(tid: &TypeId) -> Option<RecordTypeInfo> {
        RECORD_REGISTRY.with(|r| r.borrow().get(tid).cloned())
    }

    #[cfg(feature = "no_std")]
    fn registry_get(_tid: &TypeId) -> Option<RecordTypeInfo> {
        None // no registry when no_std
    }

    // Emitted by generated `typeof<Record>`. Builds the field metadata, registers
    // the type by its concrete TypeId, and returns the boxed RecordTypeInfo
    // (which is what a `System.Type` value holds on the Rust target).
    pub fn recordType(
        tid: TypeId,
        name: string,
        field_names: Array<string>,
        make: Func1<Array<obj>, obj>,
        getters: Array<Func1<obj, obj>>,
    ) -> obj {
        let names: Vec<string> = field_names.get().iter().cloned().collect();
        let gets: Vec<Func1<obj, obj>> = getters.get().iter().cloned().collect();
        let fields_vec: Vec<LrcPtr<RecordFieldInfo>> = names
            .into_iter()
            .zip(gets.into_iter())
            .map(|(n, g)| LrcPtr::new(RecordFieldInfo { name: n, get: g }))
            .collect();
        let info = RecordTypeInfo {
            tid,
            name,
            fields: array_from(fields_vec),
            make,
        };
        registry_insert(tid, info.clone());
        box_(info)
    }

    fn type_info_of(typ: &obj) -> RecordTypeInfo {
        (**typ)
            .downcast_ref::<RecordTypeInfo>()
            .expect("Type does not carry record reflection info")
            .clone()
    }

    // FSharpValue.MakeRecord(typ, values, ?bindingFlags) -> obj
    pub fn makeRecord(typ: obj, values: Array<obj>, _flags: Option<i32>) -> obj {
        let info = type_info_of(&typ);
        (info.make)(values)
    }

    // FSharpType.GetRecordFields(typ, ?bindingFlags) -> PropertyInfo[]
    pub fn getRecordElements(typ: obj, _flags: Option<i32>) -> Array<LrcPtr<RecordFieldInfo>> {
        type_info_of(&typ).fields
    }

    // FSharpValue.GetRecordFields(record, ?bindingFlags) -> obj[]
    pub fn getRecordFields(record: obj, _flags: Option<i32>) -> Array<obj> {
        let tid = (*record).type_id();
        let info = registry_get(&tid).expect("Record type not registered; evaluate typeof<T> first");
        let vals: Vec<obj> = info
            .fields
            .get()
            .iter()
            .map(|f| (f.get)(record.clone()))
            .collect();
        array_from(vals)
    }

    // FSharpValue.GetRecordField(record, propInfo) -> obj
    pub fn getRecordField(record: obj, info: LrcPtr<RecordFieldInfo>) -> obj {
        (info.get)(record)
    }

    // PropertyInfo.Name -> string (dedicated accessor; distinct from name<T>()).
    pub fn propertyName(info: LrcPtr<RecordFieldInfo>) -> string {
        info.name.clone()
    }

    // PropertyInfo.GetValue(record) -> obj
    pub fn getValue(info: LrcPtr<RecordFieldInfo>, record: obj) -> obj {
        (info.get)(record)
    }

    // FSharpType.IsRecord(typ) -> bool
    pub fn isRecord(typ: obj, _flags: Option<i32>) -> bool {
        (*typ).downcast_ref::<RecordTypeInfo>().is_some()
    }

    // System.Type.FullName. On Rust a `System.Type` value is a boxed carrier: a record
    // type carries RecordTypeInfo (use its registered name), while a quotation-derived
    // declaring type (from MethodInfo.DeclaringType) carries the boxed fullname string.
    pub fn fullName(typ: obj) -> string {
        if let Some(info) = (*typ).downcast_ref::<RecordTypeInfo>() {
            info.name.clone()
        } else if let Some(s) = (*typ).downcast_ref::<string>() {
            s.clone()
        } else {
            crate::String_::string("")
        }
    }

    // obj.GetType() for a statically-erased value (static type = obj/Any).
    // Reads the boxed value's *runtime* type id and returns the concrete type's
    // registered reflection info, so `(box record : obj).GetType()` resolves to
    // the real record type (not typeof<obj>) whenever that type was registered
    // via a prior `typeof<T>`. Falls back to returning the object itself as an
    // opaque placeholder for unregistered types.
    pub fn getTypeFromObj(o: obj) -> obj {
        let tid = (*o).type_id();
        match registry_get(&tid) {
            Some(info) => box_(info),
            None => o,
        }
    }

    // System.Type equality (typeof<A> = typeof<B>). Compares the carried concrete
    // TypeId when both operands are record type infos; otherwise falls back to
    // comparing the boxed values' runtime type ids.
    pub fn typeEquals(a: obj, b: obj) -> bool {
        let ta = (*a).downcast_ref::<RecordTypeInfo>();
        let tb = (*b).downcast_ref::<RecordTypeInfo>();
        match (ta, tb) {
            (Some(x), Some(y)) => x.tid == y.tid,
            (None, None) => {
                // Non-record `System.Type` values carry a boxed concrete `TypeId`.
                // Downcast both and compare the *carried* ids; comparing the boxed
                // carriers' runtime `type_id()` would always be `TypeId::of::<TypeId>()`
                // and therefore make every non-record type compare equal.
                match ((*a).downcast_ref::<TypeId>(), (*b).downcast_ref::<TypeId>()) {
                    (Some(x), Some(y)) => x == y,
                    _ => (*a).type_id() == (*b).type_id(),
                }
            }
            // One record, one non-record: definitely different types.
            _ => false,
        }
    }
}
