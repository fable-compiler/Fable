module rec Fable.Transforms.Rust.AST.Printer

open Fable.Transforms.Rust.AST.Adapters
open Fable.Transforms.Rust.AST.Types
open Fable.Transforms.Rust.AST.Visitor
open Fable.Transforms.Printer

type RustPrinter(printer: Printer) =

    inherit VisitorImpl()

    member self.visit_name(span: Span, name: Symbol) = printer.Print(name)

// member self.visit_ident(ident: Ident) =
//     walk_ident(self, ident)

// member self.visit_foreign_item(i: ForeignItem) =
//     walk_foreign_item(self, i)

// member self.visit_global_asm(ga: GlobalAsm) =
//     walk_global_asm(self, ga)

// member self.visit_item(i: Item) =
//     walk_item(self, i)

// member self.visit_local(l: Local) =
//     walk_local(self, l)

// member self.visit_block(b: Block) =
//     walk_block(self, b)

// member self.visit_stmt(s: Stmt) =
//     walk_stmt(self, s)

// member self.visit_param(param: Param) =
//     walk_param(self, param)

// member self.visit_arm(a: Arm) =
//     walk_arm(self, a)

// member self.visit_pat(p: Pat) =
//     walk_pat(self, p)

// member self.visit_anon_const(c: AnonConst) =
//     walk_anon_const(self, c)

// member self.visit_expr(ex: Expr) =
//     walk_expr(self, ex)

// member self.visit_expr_post(_ex: Expr) =
//     ()

// member self.visit_ty(t: Ty) =
//     walk_ty(self, t)

// member self.visit_generic_param(param: GenericParam) =
//     walk_generic_param(self, param)

// member self.visit_generics(g: Generics) =
//     walk_generics(self, g)

// member self.visit_where_predicate(p: WherePredicate) =
//     walk_where_predicate(self, p)

// member self.visit_fn(fk: FnKind, s: Span, _: NodeId) =
//     walk_fn(self, fk, s)

// member self.visit_assoc_item(i: AssocItem, ctxt: AssocCtxt) =
//     walk_assoc_item(self, i, ctxt)

// member self.visit_trait_ref(t: TraitRef) =
//     walk_trait_ref(self, t)

// member self.visit_param_bound(bounds: GenericBound) =
//     walk_param_bound(self, bounds)

// member self.visit_poly_trait_ref(t: PolyTraitRef, m: TraitBoundModifier) =
//     walk_poly_trait_ref(self, t, m)

// member self.visit_variant_data(s: VariantData) =
//     walk_struct_def(self, s)

// member self.visit_field_def(s: FieldDef) =
//     walk_field_def(self, s)

// member self.visit_enum_def(enum_definition: EnumDef, generics: Generics, item_id: NodeId, _: Span) =
//     walk_enum_def(self, enum_definition, generics, item_id)

// member self.visit_variant(v: Variant) =
//     walk_variant(self, v)

// member self.visit_label(label: Label) =
//     walk_label(self, label)

// member self.visit_lifetime(lifetime: Lifetime) =
//     walk_lifetime(self, lifetime)

// member self.visit_mac_call(mac: MacCall) =
//     walk_mac(self, mac)

// member self.visit_mac_def(_mac: MacroDef, _id: NodeId) =
//     ()

// member self.visit_path(path: Path, _id: NodeId) =
//     walk_path(self, path)

// member self.visit_use_tree(use_tree: UseTree, id: NodeId, _nested: bool) =
//     walk_use_tree(self, use_tree, id)

// member self.visit_path_segment(path_span: Span, path_segment: PathSegment) =
//     walk_path_segment(self, path_span, path_segment)

// member self.visit_generic_args(path_span: Span, generic_args: GenericArgs) =
//     walk_generic_args(self, path_span, generic_args)

// member self.visit_generic_arg(generic_arg: GenericArg) =
//     walk_generic_arg(self, generic_arg)

// member self.visit_assoc_ty_constraint(``constraint``: AssocTyConstraint) =
//     walk_assoc_ty_constraint(self, ``constraint``)

// member self.visit_attribute(attr: Attribute) =
//     walk_attribute(self, attr)

// member self.visit_vis(vis: Visibility) =
//     walk_vis(self, vis)

// member self.visit_fn_ret_ty(ret_ty: FnRetTy) =
//     walk_fn_ret_ty(self, ret_ty)

// member self.visit_fn_header(_header: FnHeader) =
//     ()

// member self.visit_expr_field(f: ExprField) =
//     walk_expr_field(self, f)

// member self.visit_pat_field(fp: PatField) =
//     walk_pat_field(self, fp)
