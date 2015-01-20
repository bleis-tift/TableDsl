module TableDsl.Extensions

open TableDsl

type Attribute with
  member this.Key =
    match this with SimpleAttr k | ComplexAttr (k, _) -> k

type ColumnAttribute with
  member this.Key =
    match this with SimpleColAttr k | ComplexColAttr (k, _) -> k
  member this.ToAttribute() =
    match this with
    | SimpleColAttr k -> SimpleAttr k
    | ComplexColAttr (k, v) -> ComplexAttr (k, [v |> List.map (function Lit l -> l) |> String.concat ""])

type NonEnumType with
  member this.TypeNameWithParams =
    let paramsStr =
      if this.TypeParameters.IsEmpty then ""
      else
        "(" + (this.TypeParameters |> List.map (function BoundValue p -> p | TypeVariable p -> "@" + p) |> String.concat ", ") + ")"
    sprintf "%s%s" this.TypeName paramsStr

type TypeDef with
  member this.Summary =
    match this with
    | BuiltinType _ | EnumTypeDef _ -> None
    | AliasDef (_, org) -> org.Summary
  member this.TypeName =
    match this with
    | BuiltinType t -> t.TypeNameWithParams
    | AliasDef (_, org) -> org.RootTypeName
    | EnumTypeDef t -> t.BaseType.TypeName
  member this.JpName =
    match this with
    | BuiltinType _ | EnumTypeDef _ -> None
    | AliasDef (_, org) -> org.JpName
  member this.Attributes(parentAttrs: ColumnAttribute seq) =
    match this with
    | BuiltinType _ | EnumTypeDef _ -> Seq.empty
    | AliasDef (_, org) ->
        org.Attributes
        |> Seq.filter (fun (a: ColumnAttribute) -> parentAttrs |> Seq.forall (fun p -> a.Key <> p.Key))

and ColumnTypeDef with
  member this.Summary =
    match this.ColumnTypeDefSummary with
    | Some summary -> Some summary
    | None -> this.ColumnTypeDef.Summary
  /// coltype TypeName = ...のTypeName(パラメータを含まない)
  member this.TypeName =
    match this.ColumnTypeDef with
    | BuiltinType t
    | AliasDef (t, _) -> t.TypeName
    | EnumTypeDef t -> t.EnumTypeName
  member this.Params =
    match this.ColumnTypeDef with
    | BuiltinType t
    | AliasDef (t, _) -> t.TypeParameters
    | EnumTypeDef t -> t.BaseType.TypeParameters
  member this.RootTypeName =
    match this.ColumnTypeDef with
    | BuiltinType t -> t.TypeNameWithParams
    | AliasDef (_, org) -> org.RootTypeName
    | EnumTypeDef t -> t.BaseType.TypeNameWithParams
  member this.JpName =
    match this.ColumnTypeDefJpName with
    | Some jpName -> Some jpName
    | None -> this.ColumnTypeDef.JpName
  member this.Attributes =
    seq {
      let attrs = this.ColumnTypeDefAttributes
      yield! attrs
      yield! this.ColumnTypeDef.Attributes(attrs)
    }

type ColumnTypeRefKind with
  member this.ColumnType =
    match this with NonEnum t | Enum (t, _) -> t
  static member FromColumnTypeDef(colTypeDef: ColumnTypeDef) =
    let name = colTypeDef.RootTypeName
    let summary = colTypeDef.Summary
    let jpName = colTypeDef.JpName
    let attrs = colTypeDef.Attributes |> Seq.map (fun a -> a.ToAttribute()) |> Seq.toList
    match colTypeDef.ColumnTypeDef with
    | BuiltinType _ -> NonEnum (ColumnTypeRef.Create(name, summary, jpName, attrs))
    | AliasDef (_, org) ->
        match ColumnTypeRefKind.FromColumnTypeDef(org) with
        | NonEnum _ -> NonEnum (ColumnTypeRef.Create(name, summary, jpName, attrs))
        | Enum (_, cases) -> Enum (ColumnTypeRef.Create(name, summary, jpName, attrs), cases)
    | EnumTypeDef t -> Enum (ColumnTypeRef.Create(name, summary, jpName, attrs), t.Cases)

type ColumnType with
  member this.Summary = this.Ref.ColumnType.ColumnTypeRefSummary
  member this.JpName = this.Ref.ColumnType.ColumnTypeRefJpName
  member this.RootTypeName = this.Ref.ColumnType.RootType
  member this.TypeRefName =
    let name =
      this.ColumnTypeName + "(" + (this.ColumnTypeParams |> String.concat ", ") + ")"
    if this.IsNullable then
      "nullable(" + name + ")"
    else
      name
  member this.AllAttrbutes =
    seq {
      let attrs = this.ColumnTypeAttributes
      yield! attrs
      yield! this.Ref.ColumnType.ColumnTypeRefAttributes |> Seq.filter (fun a -> attrs |> Seq.forall (fun a2 -> a2.Key <> a.Key))
    }