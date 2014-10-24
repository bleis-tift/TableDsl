namespace TableDsl

module PrinterPluginUtil =
  open Basis.Core

  module internal ResultList =
    let partition xs =
      let ss, fs = xs |> List.partition Result.isSuccess
      let ss = ss |> List.map (function Success s -> s | _ -> failwith "oops!")
      let fs = fs |> List.map (function Failure f -> f | _ -> failwith "oops!")
      (ss, fs)

  type ConvertError =
    | UnboundParameters of string seq
    | InvalidParameter of string
    | Composite of ConvertError seq

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module ConvertError =
    let toStr err =
      let rec toStr' acc = function
      | UnboundParameters ps -> acc + "未確定のパラメータ: " + (ps |> Str.join ", ") + "\n"
      | InvalidParameter p -> acc + "不正なパラメータ: " + p + "\n"
      | Composite errs -> acc + (errs |> Seq.map (toStr' "") |> Str.join "\n") + "\n"

      toStr' "" err

  module ColumnAttributeValue =
    let tryToStr attrValueElems =
      let tryToStr' = function
      | Lit l -> Success l
      | Var v -> Failure v

      let strs, errs =
        attrValueElems
        |> List.map tryToStr'
        |> ResultList.partition

      if errs |> List.isEmpty then
        Success (Str.concat strs)
      else
        Failure (UnboundParameters errs)

  let private toResult (res, errs) =
    if errs |> List.isEmpty then
      Success res
    else
      match errs with
      | [oneErr] -> Failure oneErr
      | moreErrs -> Failure (Composite moreErrs)

  module ColumnAttribute =
    let tryToKeyVal = function
    | SimpleColAttr attr -> Success (attr, attr)
    | ComplexColAttr (key, value) ->
        result {
          let! valueStr = ColumnAttributeValue.tryToStr value
          return (key, valueStr)
        }

    let tryToAList attrs =
      attrs |> Seq.map tryToKeyVal |> Seq.toList |> ResultList.partition |> toResult

  module ColumnTypeDef =
    let private (++) xs ys = seq { yield! xs; yield! ys }

    type TypeName = {
      TypeName: string
      Nullable: bool
      Attributes: (string * string) list
    }

    let rec internal nonEnumTypeName attrs = function
    | { TypeName = "nullable"; TypeParameters = typeParams } ->
        let innerTypeNameElems, errs =
          typeParams
          |> List.map (function
                       | BoundValue v -> Success (v, [])
                       | BoundType t ->
                           match tryToTypeName [] t with
                           | Success { TypeName.TypeName = typeName; Attributes = attrs } -> Success (typeName, attrs)
                           | Failure f -> Failure f
                       | TypeVariable v -> Failure (UnboundParameters (Seq.singleton v)))
          |> ResultList.partition

        let innerTypeName = innerTypeNameElems |> List.map fst |> Str.concat
        let innerAttrs =
          innerTypeNameElems
          |> List.collect snd
          |> List.map (fun (k, v) -> ComplexColAttr (k, [Lit v]))
        toResult ((innerTypeName, true, innerAttrs ++ attrs), errs)
    | typ ->
        let typeParams, errs =
          match typ.TypeParameters with
          | [] -> [], []
          | notEmpty ->
              notEmpty
              |> List.map (function
                           | BoundValue v -> Success v
                           | BoundType t ->
                               match tryToTypeName [] t with
                               | Success { TypeName.TypeName = typeName } -> Failure (InvalidParameter typeName)
                               | Failure f -> Failure f
                           | TypeVariable v -> Failure (UnboundParameters (Seq.singleton v)))
              |> ResultList.partition
        let typeParam =
          match typeParams with
          | [] -> ""
          | _ -> "(" + (typeParams |> Str.join ", ") + ")"
        toResult ((typ.TypeName + typeParam, false, attrs), errs)

    and internal colTypeDefName attrs colTypeDef =
      match colTypeDef with
      | BuiltinType typ -> nonEnumTypeName attrs typ
      | AliasDef (_typ, orgType) -> colTypeDefName (attrs ++ orgType.ColumnAttributes) orgType.ColumnTypeDef
      | EnumTypeDef typ -> nonEnumTypeName attrs typ.BaseType

    and tryToTypeName attrs (typ: ColumnTypeDef) = result {
      let attrs2 = attrs ++ typ.ColumnAttributes
      let! typeName, nullable, attrs3 = colTypeDefName attrs2 typ.ColumnTypeDef
      let! attrs4 = attrs3 |> ColumnAttribute.tryToAList
      return { TypeName = typeName; Nullable = nullable; Attributes = attrs4 }
    }