// Source: https://github.com/rust-lang/rust/blob/master/compiler/rustc_ast/src/visit.rs

module rec Fable.Transforms.Rust.AST.Visitor

open Fable.Transforms.Rust.AST.Adapters
open Fable.Transforms.Rust.AST.Types

//! AST walker. Each overridden visit method has full control over what
//! happens with its node, it can do its own traversal of the node's children,
//! call `visit::walk_*` to apply the default traversal algorithm, or prevent
//! deeper traversal by doing nothing.
//!
//! Note: it is an important invariant that the default visitor walks the body
//! of a function in "execution order" (more concretely, reverse post-order
//! with respect to the CFG implied by the AST), meaning that if AST node A may
//! execute before AST node B, then A is visited first. The borrow checker in
//! particular relies on this property.
//!
//! Note: walking an AST before macro expansion is probably a bad idea. For
//! instance, a walker looking for item names in a module will miss all of
//! those that are created by the expansion of a macro.

// use crate::ast::*;
// use crate::token;

// use rustc_span::symbol::{Ident, Symbol};
// use rustc_span::Span;

type AssocCtxt =
    | Trait
    | Impl

type FnCtxt =
    | Free
    | Foreign
    | Assoc of AssocCtxt

type FnKind =
    /// E.g., `fn foo()`, `fn foo`, or `extern "Abi" fn foo()`.
    | Fn of FnCtxt * Ident * FnSig * Visibility * Option<Block>

    /// E.g., `|x, y| body`.
    | Closure of FnDecl * Expr

    member self.header: Option<FnHeader> =
        match self with
        | Fn(_, _, sig_, _, _) -> Some(sig_.header)
        | Closure(_, _) -> None

    member self.ident: Option<Ident> =
        match self with
        | Fn(_, ident, _, _, _) -> Some(ident)
        | _ -> None

    member self.decl: FnDecl =
        match self with
        | Fn(_, _, sig_, _, _) -> sig_.decl
        | Closure(decl, _) -> decl

    member self.ctxt: Option<FnCtxt> =
        match self with
        | Fn(ctxt, _, _, _, _) -> Some(ctxt)
        | Closure(_, _) -> None


/// Each method of the `Visitor` trait is a hook to be potentially
/// overridden. Each method's default implementation recursively visits
/// the substructure of the input via the corresponding `walk` method;
/// e.g., the `visit_item` method by default calls `visit::walk_item`.
///
/// If you want to ensure that your code handles every variant
/// explicitly, you need to override each method. (And you also need
/// to monitor future changes to `Visitor` in case a new method with a
/// new default implementation gets introduced.)
type Visitor =
    abstract visit_name: span: Span * name: Symbol -> unit
    abstract visit_ident: ident: Ident -> unit
    abstract visit_foreign_item: i: ForeignItem -> unit
    abstract visit_global_asm: ga: GlobalAsm -> unit
    abstract visit_item: i: Item -> unit
    abstract visit_local: l: Local -> unit
    abstract visit_block: b: Block -> unit
    abstract visit_stmt: s: Stmt -> unit
    abstract visit_param: param: Param -> unit
    abstract visit_arm: a: Arm -> unit
    abstract visit_pat: p: Pat -> unit
    abstract visit_anon_const: c: AnonConst -> unit
    abstract visit_expr: ex: Expr -> unit
    abstract visit_expr_post: ex: Expr -> unit
    abstract visit_ty: t: Ty -> unit
    abstract visit_generic_param: param: GenericParam -> unit
    abstract visit_generics: g: Generics -> unit
    abstract visit_where_predicate: p: WherePredicate -> unit
    abstract visit_fn: fk: FnKind * s: Span * id: NodeId -> unit
    abstract visit_assoc_item: i: AssocItem * ctxt: AssocCtxt -> unit
    abstract visit_trait_ref: t: TraitRef -> unit
    abstract visit_param_bound: bounds: GenericBound -> unit

    abstract visit_poly_trait_ref:
        t: PolyTraitRef * m: TraitBoundModifier -> unit

    abstract visit_variant_data: s: VariantData -> unit
    abstract visit_field_def: s: FieldDef -> unit

    abstract visit_enum_def:
        enum_definition: EnumDef *
        generics: Generics *
        item_id: NodeId *
        span: Span ->
            unit

    abstract visit_variant: v: Variant -> unit
    abstract visit_label: label: Label -> unit
    abstract visit_lifetime: lifetime: Lifetime -> unit
    abstract visit_mac_call: mac: MacCall -> unit
    abstract visit_mac_def: mac: MacroDef * id: NodeId -> unit
    abstract visit_path: path: Path * id: NodeId -> unit

    abstract visit_use_tree:
        use_tree: UseTree * id: NodeId * nested: bool -> unit

    abstract visit_path_segment:
        path_span: Span * path_segment: PathSegment -> unit

    abstract visit_generic_args:
        path_span: Span * generic_args: GenericArgs -> unit

    abstract visit_generic_arg: generic_arg: GenericArg -> unit
    abstract visit_assoc_ty_constraint: constraint_: AssocTyConstraint -> unit
    abstract visit_attribute: attr: Attribute -> unit
    abstract visit_vis: vis: Visibility -> unit
    abstract visit_fn_ret_ty: ret_ty: FnRetTy -> unit
    abstract visit_fn_header: header: FnHeader -> unit
    abstract visit_expr_field: f: ExprField -> unit
    abstract visit_pat_field: fp: PatField -> unit

[<AbstractClass>]
type VisitorImpl() =

    interface Visitor with

        member self.visit_name(_span: Span, _name: Symbol) = () // Nothing to do.

        member self.visit_ident(ident: Ident) = walk_ident (self, ident)

        member self.visit_foreign_item(i: ForeignItem) =
            walk_foreign_item (self, i)

        member self.visit_global_asm(ga: GlobalAsm) = walk_global_asm (self, ga)

        member self.visit_item(i: Item) = walk_item (self, i)

        member self.visit_local(l: Local) = walk_local (self, l)

        member self.visit_block(b: Block) = walk_block (self, b)

        member self.visit_stmt(s: Stmt) = walk_stmt (self, s)

        member self.visit_param(param: Param) = walk_param (self, param)

        member self.visit_arm(a: Arm) = walk_arm (self, a)

        member self.visit_pat(p: Pat) = walk_pat (self, p)

        member self.visit_anon_const(c: AnonConst) = walk_anon_const (self, c)

        member self.visit_expr(ex: Expr) = walk_expr (self, ex)

        member self.visit_expr_post(_ex: Expr) = ()

        member self.visit_ty(t: Ty) = walk_ty (self, t)

        member self.visit_generic_param(param: GenericParam) =
            walk_generic_param (self, param)

        member self.visit_generics(g: Generics) = walk_generics (self, g)

        member self.visit_where_predicate(p: WherePredicate) =
            walk_where_predicate (self, p)

        member self.visit_fn(fk: FnKind, s: Span, _: NodeId) =
            walk_fn (self, fk, s)

        member self.visit_assoc_item(i: AssocItem, ctxt: AssocCtxt) =
            walk_assoc_item (self, i, ctxt)

        member self.visit_trait_ref(t: TraitRef) = walk_trait_ref (self, t)

        member self.visit_param_bound(bounds: GenericBound) =
            walk_param_bound (self, bounds)

        member self.visit_poly_trait_ref
            (
                t: PolyTraitRef,
                m: TraitBoundModifier
            )
            =
            walk_poly_trait_ref (self, t, m)

        member self.visit_variant_data(s: VariantData) =
            walk_struct_def (self, s)

        member self.visit_field_def(s: FieldDef) = walk_field_def (self, s)

        member self.visit_enum_def
            (
                enum_definition: EnumDef,
                generics: Generics,
                item_id: NodeId,
                _: Span
            )
            =
            walk_enum_def (self, enum_definition, generics, item_id)

        member self.visit_variant(v: Variant) = walk_variant (self, v)

        member self.visit_label(label: Label) = walk_label (self, label)

        member self.visit_lifetime(lifetime: Lifetime) =
            walk_lifetime (self, lifetime)

        member self.visit_mac_call(mac: MacCall) = walk_mac (self, mac)

        member self.visit_mac_def(_mac: MacroDef, _id: NodeId) = () // Nothing to do

        member self.visit_path(path: Path, _id: NodeId) = walk_path (self, path)

        member self.visit_use_tree
            (
                use_tree: UseTree,
                id: NodeId,
                _nested: bool
            )
            =
            walk_use_tree (self, use_tree, id)

        member self.visit_path_segment
            (
                path_span: Span,
                path_segment: PathSegment
            )
            =
            walk_path_segment (self, path_span, path_segment)

        member self.visit_generic_args
            (
                path_span: Span,
                generic_args: GenericArgs
            )
            =
            walk_generic_args (self, path_span, generic_args)

        member self.visit_generic_arg(generic_arg: GenericArg) =
            walk_generic_arg (self, generic_arg)

        member self.visit_assoc_ty_constraint(constraint_: AssocTyConstraint) =
            walk_assoc_ty_constraint (self, constraint_)

        member self.visit_attribute(attr: Attribute) =
            walk_attribute (self, attr)

        member self.visit_vis(vis: Visibility) = walk_vis (self, vis)

        member self.visit_fn_ret_ty(ret_ty: FnRetTy) =
            walk_fn_ret_ty (self, ret_ty)

        member self.visit_fn_header(_header: FnHeader) = () // Nothing to do

        member self.visit_expr_field(f: ExprField) = walk_expr_field (self, f)

        member self.visit_pat_field(fp: PatField) = walk_pat_field (self, fp)

let walk_opt (method: 'expr -> unit, opt: Option<'expr>) =
    match opt with
    | Some elem -> method (elem)
    | _ -> ()

let walk_list (method: 'expr -> unit, list: Vec<'expr>) =
    for elem in list do
        method (elem)

let walk_list2
    (
        method: 'expr * AssocCtxt -> unit,
        list: Vec<'expr>,
        ctxt: AssocCtxt
    )
    =
    for elem in list do
        method (elem, ctxt)

let walk_ident (visitor: Visitor, ident: Ident) =
    visitor.visit_name (ident.span, ident.name)

let walk_crate (visitor: Visitor, krate: Crate) =
    walk_list (visitor.visit_item, krate.items)
    walk_list (visitor.visit_attribute, krate.attrs)

let walk_local (visitor: Visitor, local: Local) =
    for attr in local.attrs do
        visitor.visit_attribute (attr)

    visitor.visit_pat (local.pat)
    walk_opt (visitor.visit_ty, local.ty)
    walk_opt (visitor.visit_expr, local.init)

let walk_label (visitor: Visitor, label: Label) =
    visitor.visit_ident (label.ident)

let walk_lifetime (visitor: Visitor, lifetime: Lifetime) =
    visitor.visit_ident (lifetime.ident)

let walk_poly_trait_ref
    (
        visitor: Visitor,
        trait_ref: PolyTraitRef,
        _: TraitBoundModifier
    )
    =
    walk_list (visitor.visit_generic_param, trait_ref.bound_generic_params)
    visitor.visit_trait_ref (trait_ref.trait_ref)

let walk_trait_ref (visitor: Visitor, trait_ref: TraitRef) =
    visitor.visit_path (trait_ref.path, trait_ref.ref_id)

let walk_item (visitor: Visitor, item: Item) =
    visitor.visit_vis (item.vis)
    visitor.visit_ident (item.ident)

    match item.kind with
    | ItemKind.ExternCrate(orig_name) ->
        match orig_name with
        | Some(orig_name) -> visitor.visit_name (item.span, orig_name)
        | _ -> ()

    | ItemKind.Use(use_tree) ->
        visitor.visit_use_tree (use_tree, item.id, false)
    | ItemKind.Static(typ, _, expr)
    | ItemKind.Const(_, typ, expr) ->
        visitor.visit_ty (typ)
        walk_opt (visitor.visit_expr, expr)

    | ItemKind.Fn((_, sig_, generics, body)) ->
        visitor.visit_generics (generics)
        let kind = Fn(FnCtxt.Free, item.ident, sig_, item.vis, body)
        visitor.visit_fn (kind, item.span, item.id)

    | ItemKind.Mod(_unsafety, mod_kind) ->
        match mod_kind with
        | ModKind.Loaded(items, _inline, _inner_span) ->
            walk_list (visitor.visit_item, items)
        | ModKind.Unloaded -> ()

    | ItemKind.ForeignMod(foreign_module) ->
        walk_list (visitor.visit_foreign_item, foreign_module.items)

    | ItemKind.GlobalAsm(ga) -> visitor.visit_global_asm (ga)
    | ItemKind.TyAlias((_, generics, bounds, ty)) ->
        visitor.visit_generics (generics)
        walk_list (visitor.visit_param_bound, bounds)
        walk_opt (visitor.visit_ty, ty)

    | ItemKind.Enum(enum_definition, generics) ->
        visitor.visit_generics (generics)
        visitor.visit_enum_def (enum_definition, generics, item.id, item.span)

    | ItemKind.Impl({
                        generics = generics
                        of_trait = of_trait
                        self_ty = self_ty
                        items = items
                    }) ->
        visitor.visit_generics (generics)
        walk_opt (visitor.visit_trait_ref, of_trait)
        visitor.visit_ty (self_ty)
        walk_list2 (visitor.visit_assoc_item, items, AssocCtxt.Impl)

    | ItemKind.Struct(struct_definition, generics)
    | ItemKind.Union(struct_definition, generics) ->
        visitor.visit_generics (generics)
        visitor.visit_variant_data (struct_definition)

    | ItemKind.Trait((_, _, generics, bounds, items)) ->
        visitor.visit_generics (generics)
        walk_list (visitor.visit_param_bound, bounds)
        walk_list2 (visitor.visit_assoc_item, items, AssocCtxt.Trait)

    | ItemKind.TraitAlias(generics, bounds) ->
        visitor.visit_generics (generics)
        walk_list (visitor.visit_param_bound, bounds)

    | ItemKind.MacCall(mac) -> visitor.visit_mac_call (mac)
    | ItemKind.MacroDef(ts) -> visitor.visit_mac_def (ts, item.id)

    walk_list (visitor.visit_attribute, item.attrs)

let walk_enum_def
    (
        visitor: Visitor,
        enum_definition: EnumDef,
        _: Generics,
        _: NodeId
    )
    =
    walk_list (visitor.visit_variant, enum_definition.variants)

let walk_variant (visitor: Visitor, variant: Variant) =
    visitor.visit_ident (variant.ident)
    visitor.visit_vis (variant.vis)
    visitor.visit_variant_data (variant.data)
    walk_opt (visitor.visit_anon_const, variant.disr_expr)
    walk_list (visitor.visit_attribute, variant.attrs)

let walk_expr_field (visitor: Visitor, f: ExprField) =
    visitor.visit_expr (f.expr)
    visitor.visit_ident (f.ident)
    walk_list (visitor.visit_attribute, f.attrs)

let walk_pat_field (visitor: Visitor, fp: PatField) =
    visitor.visit_ident (fp.ident)
    visitor.visit_pat (fp.pat)
    walk_list (visitor.visit_attribute, fp.attrs)

let walk_ty (visitor: Visitor, typ: Ty) =
    match typ.kind with
    | TyKind.Slice(ty)
    | TyKind.Paren(ty) -> visitor.visit_ty (ty)
    | TyKind.Ptr(mutable_type) -> visitor.visit_ty (mutable_type.ty)
    | TyKind.Rptr(opt_lifetime, mutable_type) ->
        walk_opt (visitor.visit_lifetime, opt_lifetime)
        visitor.visit_ty (mutable_type.ty)

    | TyKind.Tup(tuple_element_types) ->
        walk_list (visitor.visit_ty, tuple_element_types)

    | TyKind.BareFn(function_declaration) ->
        walk_list (
            visitor.visit_generic_param,
            function_declaration.generic_params
        )

        walk_fn_decl (visitor, function_declaration.decl)

    | TyKind.Path(maybe_qself, path) ->
        match maybe_qself with
        | Some(qself) -> visitor.visit_ty (qself.ty)
        | _ -> ()

        visitor.visit_path (path, typ.id)

    | TyKind.Array(ty, length) ->
        visitor.visit_ty (ty)
        visitor.visit_anon_const (length)

    | TyKind.TraitObject(bounds, _)
    | TyKind.ImplTrait(_, bounds) ->
        walk_list (visitor.visit_param_bound, bounds)

    | TyKind.Typeof(expression) -> visitor.visit_anon_const (expression)
    | TyKind.Infer
    | TyKind.ImplicitSelf
    | TyKind.Err -> ()
    | TyKind.MacCall(mac) -> visitor.visit_mac_call (mac)
    | TyKind.Never
    | TyKind.CVarArgs -> ()

let walk_path (visitor: Visitor, path: Path) =
    for segment in path.segments do
        visitor.visit_path_segment (path.span, segment)

let walk_use_tree (visitor: Visitor, use_tree: UseTree, id: NodeId) =
    visitor.visit_path (use_tree.prefix, id)

    match use_tree.kind with
    | UseTreeKind.Simple(rename, _, _) ->
        // The extra IDs are handled during HIR lowering.
        match rename with
        | Some(rename) -> visitor.visit_ident (rename)
        | _ -> ()

    | UseTreeKind.Glob -> ()
    | UseTreeKind.Nested(use_trees) ->
        for (nested_tree, nested_id) in use_trees do
            visitor.visit_use_tree (nested_tree, nested_id, true)

let walk_path_segment
    (
        visitor: Visitor,
        path_span: Span,
        segment: PathSegment
    )
    =
    visitor.visit_ident (segment.ident)

    match segment.args with
    | Some(args) -> visitor.visit_generic_args (path_span, args)
    | _ -> ()

let walk_generic_args
    (
        visitor: Visitor,
        _path_span: Span,
        generic_args: GenericArgs
    )
    =
    match generic_args with
    | GenericArgs.AngleBracketed(data) ->
        for arg in data.args do
            match arg with
            | AngleBracketedArg.Arg(a) -> visitor.visit_generic_arg (a)
            | AngleBracketedArg.Constraint(c) ->
                visitor.visit_assoc_ty_constraint (c)

    | GenericArgs.Parenthesized(data) ->
        walk_list (visitor.visit_ty, data.inputs)
        walk_fn_ret_ty (visitor, data.output)

let walk_generic_arg (visitor: Visitor, generic_arg: GenericArg) =
    match generic_arg with
    | GenericArg.Lifetime(lt) -> visitor.visit_lifetime (lt)
    | GenericArg.Type(ty) -> visitor.visit_ty (ty)
    | GenericArg.Const(ct) -> visitor.visit_anon_const (ct)

let walk_assoc_ty_constraint
    (
        visitor: Visitor,
        constraint_: AssocTyConstraint
    )
    =
    visitor.visit_ident (constraint_.ident)

    match constraint_.gen_args with
    | Some(gen_args) ->
        let span = constraint_.span // gen_args.span()
        visitor.visit_generic_args (span, gen_args)
    | _ -> ()

    match constraint_.kind with
    | AssocTyConstraintKind.Equality(ty) -> visitor.visit_ty (ty)

    | AssocTyConstraintKind.Bound(bounds) ->
        walk_list (visitor.visit_param_bound, bounds)

let walk_pat (visitor: Visitor, pattern: Pat) =
    match pattern.kind with
    | PatKind.TupleStruct(path, elems) ->
        visitor.visit_path (path, pattern.id)
        walk_list (visitor.visit_pat, elems)

    | PatKind.Path(opt_qself, path) ->
        match opt_qself with
        | Some(qself) -> visitor.visit_ty (qself.ty)
        | _ -> ()

        visitor.visit_path (path, pattern.id)

    | PatKind.Struct(path, fields, _) ->
        visitor.visit_path (path, pattern.id)
        walk_list (visitor.visit_pat_field, fields)

    | PatKind.Box(subpattern)
    | PatKind.Ref(subpattern, _)
    | PatKind.Paren(subpattern) -> visitor.visit_pat (subpattern)
    | PatKind.Ident(_, ident, optional_subpattern) ->
        visitor.visit_ident (ident)
        walk_opt (visitor.visit_pat, optional_subpattern)

    | PatKind.Lit(expression) -> visitor.visit_expr (expression)
    | PatKind.Range(lower_bound, upper_bound, _) ->
        walk_opt (visitor.visit_expr, lower_bound)
        walk_opt (visitor.visit_expr, upper_bound)

    | PatKind.Wild
    | PatKind.Rest -> ()
    | PatKind.Tuple(elems)
    | PatKind.Slice(elems)
    | PatKind.Or(elems) -> walk_list (visitor.visit_pat, elems)

    | PatKind.MacCall(mac) -> visitor.visit_mac_call (mac)

let walk_foreign_item (visitor: Visitor, item: ForeignItem) =
    match item with
    | {
          attrs = attrs
          id = id
          span = span
          vis = vis
          ident = ident
          kind = kind
      } ->
        visitor.visit_vis (vis)
        visitor.visit_ident (ident)
        walk_list (visitor.visit_attribute, attrs)

        match kind with
        | ForeignItemKind.Static(ty, _, expr) ->
            visitor.visit_ty (ty)
            walk_opt (visitor.visit_expr, expr)

        | ForeignItemKind.Fn((_, sig_, generics, body)) ->
            visitor.visit_generics (generics)
            let kind = Fn(FnCtxt.Foreign, ident, sig_, vis, body)
            visitor.visit_fn (kind, span, id)

        | ForeignItemKind.TyAlias((_, generics, bounds, ty)) ->
            visitor.visit_generics (generics)
            walk_list (visitor.visit_param_bound, bounds)
            walk_opt (visitor.visit_ty, ty)

        | ForeignItemKind.MacCall(mac) -> visitor.visit_mac_call (mac)

let walk_global_asm (_: Visitor, _: GlobalAsm) = () // Empty!

let walk_param_bound (visitor: Visitor, bound: GenericBound) =
    match bound with
    | GenericBound.Trait(typ, modifier) ->
        visitor.visit_poly_trait_ref (typ, modifier)
    | GenericBound.Outlives(lifetime) -> visitor.visit_lifetime (lifetime)

let walk_generic_param (visitor: Visitor, param: GenericParam) =
    visitor.visit_ident (param.ident)
    walk_list (visitor.visit_attribute, param.attrs)
    walk_list (visitor.visit_param_bound, param.bounds)

    match param.kind with
    | GenericParamKind.Lifetime -> ()
    | GenericParamKind.Type(default_) -> walk_opt (visitor.visit_ty, default_)
    | GenericParamKind.Const(ty, _, default_) ->
        visitor.visit_ty (ty)

        match default_ with
        | Some(default_) -> visitor.visit_anon_const (default_)
        | _ -> ()

let walk_generics (visitor: Visitor, generics: Generics) =
    walk_list (visitor.visit_generic_param, generics.params_)
    walk_list (visitor.visit_where_predicate, generics.where_clause.predicates)

let walk_where_predicate (visitor: Visitor, predicate: WherePredicate) =
    match predicate with
    | WherePredicate.BoundPredicate({
                                        bound_generic_params = bound_generic_params
                                        bounded_ty = bounded_ty
                                        bounds = bounds
                                    }) ->
        visitor.visit_ty (bounded_ty)
        walk_list (visitor.visit_param_bound, bounds)
        walk_list (visitor.visit_generic_param, bound_generic_params)

    | WherePredicate.RegionPredicate({
                                         lifetime = lifetime
                                         bounds = bounds
                                     }) ->
        visitor.visit_lifetime (lifetime)
        walk_list (visitor.visit_param_bound, bounds)

    | WherePredicate.EqPredicate({
                                     lhs_ty = lhs_ty
                                     rhs_ty = rhs_ty
                                 }) ->
        visitor.visit_ty (lhs_ty)
        visitor.visit_ty (rhs_ty)

let walk_fn_ret_ty (visitor: Visitor, ret_ty: FnRetTy) =
    match ret_ty with
    | FnRetTy.Ty(output_ty) -> visitor.visit_ty (output_ty)
    | _ -> ()

let walk_fn_decl (visitor: Visitor, function_declaration: FnDecl) =
    for param in function_declaration.inputs do
        visitor.visit_param (param)

    visitor.visit_fn_ret_ty (function_declaration.output)

let walk_fn (visitor: Visitor, kind: FnKind, _span: Span) =
    match kind with
    | FnKind.Fn(_, _, sig_, _, body) ->
        visitor.visit_fn_header (sig_.header)
        walk_fn_decl (visitor, sig_.decl)
        walk_opt (visitor.visit_block, body)

    | FnKind.Closure(decl, body) ->
        walk_fn_decl (visitor, decl)
        visitor.visit_expr (body)

let walk_assoc_item (visitor: Visitor, item: AssocItem, ctxt: AssocCtxt) =
    match item with
    | {
          attrs = attrs
          id = id
          span = span
          vis = vis
          ident = ident
          kind = kind
      } ->
        visitor.visit_vis (vis)
        visitor.visit_ident (ident)
        walk_list (visitor.visit_attribute, attrs)

        match kind with
        | AssocItemKind.Const(_, ty, expr) ->
            visitor.visit_ty (ty)
            walk_opt (visitor.visit_expr, expr)

        | AssocItemKind.Fn((_, sig_, generics, body)) ->
            visitor.visit_generics (generics)
            let kind = Fn(FnCtxt.Assoc(ctxt), ident, sig_, vis, body)
            visitor.visit_fn (kind, span, id)

        | AssocItemKind.TyAlias((_, generics, bounds, ty)) ->
            visitor.visit_generics (generics)
            walk_list (visitor.visit_param_bound, bounds)
            walk_opt (visitor.visit_ty, ty)

        | AssocItemKind.MacCall(mac) -> visitor.visit_mac_call (mac)

let walk_struct_def (visitor: Visitor, struct_definition: VariantData) =
    // walk_list(visitor.visit_field_def, struct_definition.fields())
    match struct_definition with
    | VariantData.Struct(fields, _)
    | VariantData.Tuple(fields, _) ->
        walk_list (visitor.visit_field_def, fields)
    | VariantData.Unit(_) -> ()

let walk_field_def (visitor: Visitor, field: FieldDef) =
    visitor.visit_vis (field.vis)

    match field.ident with
    | Some(ident) -> visitor.visit_ident (ident)
    | _ -> ()

    visitor.visit_ty (field.ty)
    walk_list (visitor.visit_attribute, field.attrs)

let walk_block (visitor: Visitor, block: Block) =
    walk_list (visitor.visit_stmt, block.stmts)

let walk_stmt (visitor: Visitor, statement: Stmt) =
    match statement.kind with
    | StmtKind.Local(local) -> visitor.visit_local (local)
    | StmtKind.Item(item) -> visitor.visit_item (item)
    | StmtKind.Expr(expr)
    | StmtKind.Semi(expr) -> visitor.visit_expr (expr)
    | StmtKind.Empty -> ()
    | StmtKind.MacCall({
                           mac = mac
                           style = _
                           attrs = attrs
                           tokens = _
                       }) ->
        visitor.visit_mac_call (mac)

        for attr in attrs do
            visitor.visit_attribute (attr)

let walk_mac (visitor: Visitor, mac: MacCall) =
    visitor.visit_path (mac.path, node_id.DUMMY_NODE_ID)

let walk_anon_const (visitor: Visitor, constant: AnonConst) =
    visitor.visit_expr (constant.value)

let walk_expr (visitor: Visitor, expression: Expr) =
    walk_list (visitor.visit_attribute, expression.attrs)

    match expression.kind with
    | ExprKind.Box(subexpression) -> visitor.visit_expr (subexpression)
    | ExprKind.Array(subexpressions) ->
        walk_list (visitor.visit_expr, subexpressions)

    | ExprKind.ConstBlock(anon_const) -> visitor.visit_anon_const (anon_const)
    | ExprKind.Repeat(element, count) ->
        visitor.visit_expr (element)
        visitor.visit_anon_const (count)

    | ExprKind.Struct(se) ->
        visitor.visit_path (se.path, expression.id)
        walk_list (visitor.visit_expr_field, se.fields)

        match se.rest with
        | StructRest.Base(expr) -> visitor.visit_expr (expr)
        | StructRest.Rest(_span) -> ()
        | StructRest.None -> ()

    | ExprKind.Tup(subexpressions) ->
        walk_list (visitor.visit_expr, subexpressions)

    | ExprKind.Call(callee_expression, arguments) ->
        visitor.visit_expr (callee_expression)
        walk_list (visitor.visit_expr, arguments)

    | ExprKind.MethodCall(segment, arguments, _span) ->
        visitor.visit_path_segment (expression.span, segment)
        walk_list (visitor.visit_expr, arguments)

    | ExprKind.Binary(_, left_expression, right_expression) ->
        visitor.visit_expr (left_expression)
        visitor.visit_expr (right_expression)

    | ExprKind.AddrOf(_, _, subexpression)
    | ExprKind.Unary(_, subexpression) -> visitor.visit_expr (subexpression)

    | ExprKind.Cast(subexpression, typ)
    | ExprKind.Type(subexpression, typ) ->
        visitor.visit_expr (subexpression)
        visitor.visit_ty (typ)

    | ExprKind.Let(pat, scrutinee) ->
        visitor.visit_pat (pat)
        visitor.visit_expr (scrutinee)

    | ExprKind.If(head_expression, if_block, optional_else) ->
        visitor.visit_expr (head_expression)
        visitor.visit_block (if_block)
        walk_opt (visitor.visit_expr, optional_else)

    | ExprKind.While(subexpression, block, opt_label) ->
        walk_opt (visitor.visit_label, opt_label)
        visitor.visit_expr (subexpression)
        visitor.visit_block (block)

    | ExprKind.ForLoop(pattern, subexpression, block, opt_label) ->
        walk_opt (visitor.visit_label, opt_label)
        visitor.visit_pat (pattern)
        visitor.visit_expr (subexpression)
        visitor.visit_block (block)

    | ExprKind.Loop(block, opt_label) ->
        walk_opt (visitor.visit_label, opt_label)
        visitor.visit_block (block)

    | ExprKind.Match(subexpression, arms) ->
        visitor.visit_expr (subexpression)
        walk_list (visitor.visit_arm, arms)

    | ExprKind.Closure(_, _, _, decl, body, _decl_span) ->
        visitor.visit_fn (
            FnKind.Closure(decl, body),
            expression.span,
            expression.id
        )

    | ExprKind.Block(block, opt_label) ->
        walk_opt (visitor.visit_label, opt_label)
        visitor.visit_block (block)

    | ExprKind.Async(_, _, body) -> visitor.visit_block (body)

    | ExprKind.Await(expr) -> visitor.visit_expr (expr)
    | ExprKind.Assign(lhs, rhs, _) ->
        visitor.visit_expr (lhs)
        visitor.visit_expr (rhs)

    | ExprKind.AssignOp(_, left_expression, right_expression) ->
        visitor.visit_expr (left_expression)
        visitor.visit_expr (right_expression)

    | ExprKind.Field(subexpression, ident) ->
        visitor.visit_expr (subexpression)
        visitor.visit_ident (ident)

    | ExprKind.Index(main_expression, index_expression) ->
        visitor.visit_expr (main_expression)
        visitor.visit_expr (index_expression)

    | ExprKind.Range(start, end_, _) ->
        walk_opt (visitor.visit_expr, start)
        walk_opt (visitor.visit_expr, end_)

    | ExprKind.Underscore -> ()
    | ExprKind.Path(maybe_qself, path) ->
        match maybe_qself with
        | Some(qself) -> visitor.visit_ty (qself.ty)
        | _ -> ()

        visitor.visit_path (path, expression.id)

    | ExprKind.Break(opt_label, opt_expr) ->
        walk_opt (visitor.visit_label, opt_label)
        walk_opt (visitor.visit_expr, opt_expr)

    | ExprKind.Continue(opt_label) -> walk_opt (visitor.visit_label, opt_label)

    | ExprKind.Ret(optional_expression) ->
        walk_opt (visitor.visit_expr, optional_expression)

    | ExprKind.MacCall(mac) -> visitor.visit_mac_call (mac)
    | ExprKind.Paren(subexpression) -> visitor.visit_expr (subexpression)
    | ExprKind.InlineAsm(ia) ->
        for (op, _) in ia.operands do
            match op with
            | InlineAsmOperand.In(_, expr)
            | InlineAsmOperand.InOut(_, _, expr)
            | InlineAsmOperand.Sym(expr) -> visitor.visit_expr (expr)
            | InlineAsmOperand.Out(_, _, expr) ->
                match expr with
                | Some(expr) -> visitor.visit_expr (expr)
                | _ -> ()

            | InlineAsmOperand.SplitInOut(_, _, in_expr, out_expr) ->
                visitor.visit_expr (in_expr)

                match out_expr with
                | Some(out_expr) -> visitor.visit_expr (out_expr)
                | _ -> ()

            | InlineAsmOperand.Const(anon_const) ->
                visitor.visit_anon_const (anon_const)

    | ExprKind.LlvmInlineAsm(ia) ->
        for (_, input) in ia.inputs do
            visitor.visit_expr (input)

        for output in ia.outputs do
            visitor.visit_expr (output.expr)


    | ExprKind.Yield(optional_expression) ->
        walk_opt (visitor.visit_expr, optional_expression)

    | ExprKind.Try(subexpression) -> visitor.visit_expr (subexpression)
    | ExprKind.TryBlock(body) -> visitor.visit_block (body)
    | ExprKind.Lit(_)
    | ExprKind.Err -> ()

    visitor.visit_expr_post (expression)

let walk_param (visitor: Visitor, param: Param) =
    walk_list (visitor.visit_attribute, param.attrs)
    visitor.visit_pat (param.pat)
    visitor.visit_ty (param.ty)

let walk_arm (visitor: Visitor, arm: Arm) =
    visitor.visit_pat (arm.pat)
    walk_opt (visitor.visit_expr, arm.guard)
    visitor.visit_expr (arm.body)
    walk_list (visitor.visit_attribute, arm.attrs)

let walk_vis (visitor: Visitor, vis: Visibility) =
    match vis.kind with
    | VisibilityKind.Restricted(path, id) -> visitor.visit_path (path, id)
    | _ -> ()

let walk_attribute (visitor: Visitor, attr: Attribute) =
    match attr.kind with
    | AttrKind.Normal(item, _tokens) -> walk_mac_args (visitor, item.args)
    | AttrKind.DocComment(_, _) -> ()

let walk_mac_args (visitor: Visitor, args: MacArgs) =
    match args with
    | MacArgs.Empty -> ()
    | MacArgs.Delimited(_dspan, _delim, _tokens) -> ()
    // The value in `#[key = VALUE]` must be visited as an expression for backward
    // compatibility, so that macros can be expanded in that position.
    | MacArgs.Eq(_eq_span, token) ->
        match token.kind with
        | TokenKind.Interpolated(nt) ->
            match nt with
            | Nonterminal.NtExpr(expr) -> visitor.visit_expr (expr)
            | t -> failwithf "unexpected token in key-value attribute: %A" t
        | t -> failwithf "unexpected token in key-value attribute: %A" t
