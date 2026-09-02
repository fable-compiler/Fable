pub mod Reflection_ {
    pub use core::any::TypeId; // re-export

    use crate::Microsoft::FSharp::Quotations::FSharpPropertyInfo;
    use crate::Native_::{box_, box_lrc, Any, Func1, LrcPtr, Vec};
    use crate::NativeArray_::{array_from, Array};
    use crate::String_::{fromString, string};

    #[cfg(all(not(feature = "no_std"), not(feature = "threaded")))]
    use core::cell::RefCell;
    #[cfg(not(feature = "no_std"))]
    use std::collections::HashMap;
    #[cfg(all(not(feature = "no_std"), feature = "threaded"))]
    use std::sync::{OnceLock, RwLock};

    pub fn name<T: Clone>() -> string {
        // TODO: map some common type names to .NET type names
        string(core::any::type_name::<T>())
    }

    // The object (System.Object / obj) representation on the Rust target.
    type obj = LrcPtr<dyn Any>;

    #[cfg(not(feature = "lrc_ptr"))]
    fn object_type_id(value: &obj) -> TypeId {
        (&**value).type_id()
    }

    #[cfg(feature = "lrc_ptr")]
    fn object_type_id(value: &obj) -> TypeId {
        (***value).type_id()
    }

    #[cfg(all(not(feature = "no_std"), not(feature = "threaded")))]
    thread_local! {
        static TYPE_NAME_REGISTRY: RefCell<HashMap<TypeId, string>> =
            RefCell::new(HashMap::new());
    }

    #[cfg(all(not(feature = "no_std"), feature = "threaded"))]
    static TYPE_NAME_REGISTRY: OnceLock<RwLock<HashMap<TypeId, string>>> = OnceLock::new();

    #[cfg(all(not(feature = "no_std"), not(feature = "threaded")))]
    fn register_type_name_value(tid: TypeId, name: string) {
        TYPE_NAME_REGISTRY.with(|r| {
            r.borrow_mut().insert(tid, name);
        });
    }

    #[cfg(all(not(feature = "no_std"), feature = "threaded"))]
    fn register_type_name_value(tid: TypeId, name: string) {
        TYPE_NAME_REGISTRY
            .get_or_init(|| RwLock::new(HashMap::new()))
            .write()
            .unwrap()
            .insert(tid, name);
    }

    #[cfg(feature = "no_std")]
    fn register_type_name_value(_tid: TypeId, _name: string) {}

    #[cfg(all(not(feature = "no_std"), not(feature = "threaded")))]
    fn registered_type_name(tid: &TypeId) -> Option<string> {
        TYPE_NAME_REGISTRY.with(|r| r.borrow().get(tid).cloned())
    }

    #[cfg(all(not(feature = "no_std"), feature = "threaded"))]
    fn registered_type_name(tid: &TypeId) -> Option<string> {
        TYPE_NAME_REGISTRY
            .get_or_init(|| RwLock::new(HashMap::new()))
            .read()
            .unwrap()
            .get(tid)
            .cloned()
    }

    #[cfg(feature = "no_std")]
    fn registered_type_name(_tid: &TypeId) -> Option<string> {
        None
    }

    #[cfg(not(feature = "no_std"))]
    fn canonical_type_name<T: 'static>() -> string {
        let rust_name = core::any::type_name::<T>();
        let name = if TypeId::of::<T>() == TypeId::of::<string>() {
            "System.String"
        } else {
            match rust_name {
                "bool" => "System.Boolean",
                "char" => "System.Char",
                "i8" => "System.SByte",
                "u8" => "System.Byte",
                "i16" => "System.Int16",
                "u16" => "System.UInt16",
                "i32" => "System.Int32",
                "u32" => "System.UInt32",
                "i64" => "System.Int64",
                "u64" => "System.UInt64",
                "f32" => "System.Single",
                "f64" => "System.Double",
                _ => rust_name,
            }
        };
        fromString(name.to_string())
    }

    #[cfg(all(not(feature = "no_std"), not(feature = "threaded")))]
    pub fn register_type_name<T: 'static>() {
        register_type_name_value(TypeId::of::<T>(), canonical_type_name::<T>());
    }

    #[cfg(all(not(feature = "no_std"), feature = "threaded"))]
    pub fn register_type_name<T: 'static>() {
        register_type_name_value(TypeId::of::<T>(), canonical_type_name::<T>());
    }

    #[cfg(feature = "no_std")]
    pub fn register_type_name<T: 'static>() {}

    // Compile-time helper emitted by `typeof<T>` to obtain a concrete type id.
    pub fn type_id<T: 'static>() -> TypeId {
        register_type_name::<T>();
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
    // Populated by `typeof<Record>` and generated record reflection calls. This is what makes
    // value-based reflection (GetRecordFields(record)) possible: from a bare
    // boxed record we can recover its runtime TypeId and look the info up.
    //
    // `thread_local!` is std-only, so under `no_std` there is no registry and
    // the value-first entry points degrade (like exception catching does).
    // Type-first reflection (typeof<T>, MakeRecord, GetRecordElements) is
    // unaffected, as those carry the info in the `System.Type` value itself.
    #[cfg(all(not(feature = "no_std"), not(feature = "threaded")))]
    thread_local! {
        static RECORD_REGISTRY: RefCell<HashMap<TypeId, LrcPtr<RecordTypeInfo>>> =
            RefCell::new(HashMap::new());
    }

    #[cfg(all(not(feature = "no_std"), feature = "threaded"))]
    static RECORD_REGISTRY: OnceLock<RwLock<HashMap<TypeId, LrcPtr<RecordTypeInfo>>>> = OnceLock::new();

    #[cfg(all(not(feature = "no_std"), not(feature = "threaded")))]
    fn registry_insert(tid: TypeId, info: LrcPtr<RecordTypeInfo>) {
        RECORD_REGISTRY.with(|r| {
            r.borrow_mut().insert(tid, info);
        });
    }

    #[cfg(all(not(feature = "no_std"), feature = "threaded"))]
    fn registry_insert(tid: TypeId, info: LrcPtr<RecordTypeInfo>) {
        RECORD_REGISTRY
            .get_or_init(|| RwLock::new(HashMap::new()))
            .write()
            .unwrap()
            .insert(tid, info);
    }

    #[cfg(feature = "no_std")]
    fn registry_insert(_tid: TypeId, _info: LrcPtr<RecordTypeInfo>) {
        // no registry when no_std
    }

    #[cfg(all(not(feature = "no_std"), not(feature = "threaded")))]
    fn registry_get(tid: &TypeId) -> Option<LrcPtr<RecordTypeInfo>> {
        RECORD_REGISTRY.with(|r| r.borrow().get(tid).cloned())
    }

    #[cfg(all(not(feature = "no_std"), feature = "threaded"))]
    fn registry_get(tid: &TypeId) -> Option<LrcPtr<RecordTypeInfo>> {
        RECORD_REGISTRY
            .get_or_init(|| RwLock::new(HashMap::new()))
            .read()
            .unwrap()
            .get(tid)
            .cloned()
    }

    #[cfg(feature = "no_std")]
    fn registry_get(_tid: &TypeId) -> Option<LrcPtr<RecordTypeInfo>> {
        None // no registry when no_std
    }

    // Emitted by the generated record declaration's reflection method. Builds the field metadata, registers
    // the type by its concrete TypeId, and returns the boxed RecordTypeInfo
    // (which is what a `System.Type` value holds on the Rust target).
    pub fn recordType(
        tid: TypeId,
        name: string,
        field_names: Array<string>,
        make: Func1<Array<obj>, obj>,
        getters: Array<Func1<obj, obj>>,
    ) -> obj {
        if let Some(typ) = registry_get(&tid) {
            return box_lrc(typ);
        }

        let names: Vec<string> = field_names.get().iter().cloned().collect();
        let gets: Vec<Func1<obj, obj>> = getters.get().iter().cloned().collect();
        let fields_vec: Vec<LrcPtr<RecordFieldInfo>> = names
            .into_iter()
            .zip(gets.into_iter())
            .map(|(n, g)| LrcPtr::new(RecordFieldInfo { name: n, get: g }))
            .collect();
        let info = LrcPtr::new(RecordTypeInfo {
            tid,
            name,
            fields: array_from(fields_vec),
            make,
        });
        register_type_name_value(tid, info.name.clone());
        registry_insert(tid, info.clone());
        box_lrc(info)
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

    // Resolves a field getter for a record value by property name, via the registry.
    // The PropertyInfo carrier holds only a name (a quotation knows nothing more), so the
    // getter is looked up here, mirroring JS/TS where getValue(pi, v) reads v[pi.Name].
    fn getterOf(record: &obj, name: &string) -> Func1<obj, obj> {
        let tid = object_type_id(record);
        let info = registry_get(&tid).expect("Record type not registered; evaluate typeof<T> first");
        let field = info
            .fields
            .get()
            .iter()
            .find(|f| &f.name == name)
            .cloned()
            .expect("Property not found on record type");
        field.get.clone()
    }

    // FSharpType.GetRecordFields(typ, ?bindingFlags) -> PropertyInfo[]
    pub fn getRecordElements(typ: obj, _flags: Option<i32>) -> Array<LrcPtr<FSharpPropertyInfo>> {
        let info = type_info_of(&typ);
        let props: Vec<LrcPtr<FSharpPropertyInfo>> = info
            .fields
            .get()
            .iter()
            .map(|f| LrcPtr::new(FSharpPropertyInfo { Name: f.name.clone() }))
            .collect();
        array_from(props)
    }

    // FSharpValue.GetRecordFields(record, ?bindingFlags) -> obj[]
    pub fn getRecordFields(record: obj, typ: obj, _flags: Option<i32>) -> Array<obj> {
        let info = type_info_of(&typ);
        registry_insert(info.tid, LrcPtr::new(info.clone()));
        let vals: Vec<obj> = info
            .fields
            .get()
            .iter()
            .map(|f| (f.get)(record.clone()))
            .collect();
        array_from(vals)
    }

    // FSharpValue.GetRecordField(record, propInfo) -> obj
    pub fn getRecordField(record: obj, info: LrcPtr<FSharpPropertyInfo>) -> obj {
        let get = getterOf(&record, &info.Name);
        get(record)
    }

    // PropertyInfo.Name -> string (dedicated accessor; distinct from name<T>()).
    pub fn propertyName(info: LrcPtr<FSharpPropertyInfo>) -> string {
        info.Name.clone()
    }

    // PropertyInfo.GetValue(record) -> obj
    pub fn getValue(info: LrcPtr<FSharpPropertyInfo>, record: obj) -> obj {
        let get = getterOf(&record, &info.Name);
        get(record)
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
        } else if let Some(tid) = (*typ).downcast_ref::<TypeId>() {
            registered_type_name(tid).unwrap_or_else(|| crate::String_::string(""))
        } else {
            crate::String_::string("")
        }
    }

    // obj.GetType() for a statically-erased value (static type = obj/Any).
    // Reads the boxed value's *runtime* type id and returns the concrete type's
    // registered reflection info, so `(box record : obj).GetType()` resolves to
    // the real record type (not typeof<obj>). Unregistered values receive a
    // TypeId carrier, which still supports type identity and primitive names.
    pub fn getTypeFromObj(o: obj) -> obj {
        let tid = object_type_id(&o);
        match registry_get(&tid) {
            Some(typ) => box_lrc(typ),
            None => box_(tid),
        }
    }

    // System.Type equality (typeof<A> = typeof<B>). Compares the carried concrete
    // TypeId when both operands are record type infos; otherwise compares the
    // TypeId carried by each non-record type value.
    pub fn typeEquals(a: obj, b: obj) -> bool {
        let ta = (*a).downcast_ref::<RecordTypeInfo>();
        let tb = (*b).downcast_ref::<RecordTypeInfo>();
        match (ta, tb) {
            (Some(x), Some(y)) => x.tid == y.tid,
            (Some(x), None) => (*b).downcast_ref::<TypeId>().map_or(false, |y| x.tid == *y),
            (None, Some(y)) => (*a).downcast_ref::<TypeId>().map_or(false, |x| *x == y.tid),
            (None, None) => {
                // Non-record `System.Type` values carry a boxed concrete `TypeId`.
                // Downcast both and compare the *carried* ids; comparing the boxed
                // carriers' runtime `type_id()` would always be `TypeId::of::<TypeId>()`
                // and therefore make every non-record type compare equal.
                match ((*a).downcast_ref::<TypeId>(), (*b).downcast_ref::<TypeId>()) {
                    (Some(x), Some(y)) => x == y,
                    _ => object_type_id(&a) == object_type_id(&b),
                }
            }
        }
    }
}
