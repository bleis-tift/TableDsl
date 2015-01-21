namespace TableDsl

type KeyNamePrefix =
  | Default of string
  | UserSpecified of string
with
  override this.ToString() = sprintf "%A" this

type ClusteredType =
  | NonClustered
  | Clustered
with
  override this.ToString() = sprintf "%A" this

type LayoutOrder =
  | Asc
  | Desc
with
  override this.ToString() = sprintf "%A" this

type SingleTableKeyInfo = {
  ClusteredType: ClusteredType
  KeyNamePrefix: KeyNamePrefix
  ColumnNum: int
  LayoutOrder: LayoutOrder
}
with
  override this.ToString() = sprintf "%A" this

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SingleTableKeyInfo =
  let parseIndexAttr defaultInfo = function
  | ["clustered"] -> { defaultInfo with ClusteredType = Clustered }
  | ["clustered"; "asc"] -> { defaultInfo with ClusteredType = Clustered; LayoutOrder = Asc }
  | ["clustered"; "desc"] -> { defaultInfo with ClusteredType = Clustered; LayoutOrder = Desc }
  | ["clustered"; keyNamePrefix] -> { defaultInfo with ClusteredType = Clustered; KeyNamePrefix = UserSpecified keyNamePrefix }
  | ["clustered"; keyNamePrefix; colNum] -> { defaultInfo with ClusteredType = Clustered; KeyNamePrefix = UserSpecified keyNamePrefix; ColumnNum = int colNum }
  | ["clustered"; keyNamePrefix; colNum; "asc"] -> { defaultInfo with ClusteredType = Clustered; KeyNamePrefix = UserSpecified keyNamePrefix; ColumnNum = int colNum; LayoutOrder = Asc }
  | ["clustered"; keyNamePrefix; colNum; "desc"] -> { defaultInfo with ClusteredType = Clustered; KeyNamePrefix = UserSpecified keyNamePrefix; ColumnNum = int colNum; LayoutOrder = Desc }
  | ["nonclustered"] -> { defaultInfo with ClusteredType = NonClustered }
  | ["nonclustered"; "asc"] -> { defaultInfo with ClusteredType = NonClustered; LayoutOrder = Asc }
  | ["nonclustered"; "desc"] -> { defaultInfo with ClusteredType = NonClustered; LayoutOrder = Desc }
  | ["nonclustered"; keyNamePrefix] -> { defaultInfo with ClusteredType = NonClustered; KeyNamePrefix = UserSpecified keyNamePrefix }
  | ["nonclustered"; keyNamePrefix; colNum] -> { defaultInfo with ClusteredType = NonClustered; KeyNamePrefix = UserSpecified keyNamePrefix; ColumnNum = int colNum }
  | ["nonclustered"; keyNamePrefix; colNum; "asc"] -> { defaultInfo with ClusteredType = NonClustered; KeyNamePrefix = UserSpecified keyNamePrefix; ColumnNum = int colNum; LayoutOrder = Asc }
  | ["nonclustered"; keyNamePrefix; colNum; "desc"] -> { defaultInfo with ClusteredType = NonClustered; KeyNamePrefix = UserSpecified keyNamePrefix; ColumnNum = int colNum; LayoutOrder = Desc }
  | ["asc"] -> { defaultInfo with LayoutOrder = Asc }
  | ["desc"] -> { defaultInfo with LayoutOrder = Desc }
  | [keyNamePrefix] -> { defaultInfo with KeyNamePrefix = UserSpecified keyNamePrefix }
  | [keyNamePrefix; "asc"] -> { defaultInfo with KeyNamePrefix = UserSpecified keyNamePrefix; LayoutOrder = Asc }
  | [keyNamePrefix; "desc"] -> { defaultInfo with KeyNamePrefix = UserSpecified keyNamePrefix; LayoutOrder = Desc }
  | [keyNamePrefix; colNum] -> { defaultInfo with KeyNamePrefix = UserSpecified keyNamePrefix; ColumnNum = int colNum }
  | [keyNamePrefix; colNum; "asc"] -> { defaultInfo with KeyNamePrefix = UserSpecified keyNamePrefix; ColumnNum = int colNum; LayoutOrder = Asc }
  | [keyNamePrefix; colNum; "desc"] -> { defaultInfo with KeyNamePrefix = UserSpecified keyNamePrefix; ColumnNum = int colNum; LayoutOrder = Desc }
  | _ -> assert false; defaultInfo

module private PrimaryKey =
  let defaultInfo =
    { ClusteredType = Clustered; KeyNamePrefix = Default "PK"; ColumnNum = 0; LayoutOrder = Asc }

module private UniqueKey =
  let defaultInfo =
    { ClusteredType = NonClustered; KeyNamePrefix = Default "UQ"; ColumnNum = 0; LayoutOrder = Asc }

module private Index =
  let defaultInfo =
    { ClusteredType = NonClustered; KeyNamePrefix = Default "IX"; ColumnNum = 0; LayoutOrder = Asc }

type RelationshipKeyInfo = {
  Table: string
  Column: string
  Prefix: string option
  ColumnNum: int
}
with
  override this.ToString() = sprintf "%A" this

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RelationshipKeyInfo =
  let parseIndexAttr = function
  | [parentTable; parentCol] -> { Table = parentTable; Column = parentCol; Prefix = None; ColumnNum = 0 }
  | [prefix; parentTable; parentCol] -> { Table = parentTable; Column = parentCol; Prefix = Some prefix; ColumnNum = 0 }
  | [prefix; order; parentTable; parentCol] -> { Table = parentTable; Column = parentCol; Prefix = Some prefix; ColumnNum = int order }
  | other -> failwithf "外部キーの形式は({prefix}.({colNum}.)){parentTable}.{parentCol}ですが、異なる形式が指定されました: %s" (other |> String.concat ".")

type KeyKindWithName =
  | PrimaryKeyName of string
  | UniqueKeyName of string
  | IndexName of string
  | ForeiginKeyName of string * string
  | DefaultConstraintName of string * string * string
with
  override this.ToString() = sprintf "%A" this

type KeyColumn =
  | SingleTableKeyCol of KeyKindWithName * ColumnDef * SingleTableKeyInfo
  | RelationshipKeyCol of KeyKindWithName * ColumnDef * RelationshipKeyInfo
  | DefaultConstraintCol of KeyKindWithName
with
  override this.ToString() = sprintf "%A" this

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module KeyColumn =
  let keyWithName = function
  | SingleTableKeyCol (kn, _, _)
  | RelationshipKeyCol (kn, _, _)
  | DefaultConstraintCol kn -> kn

  let columnDef = function
  | SingleTableKeyCol (_, colDef, _)
  | RelationshipKeyCol (_, colDef, _) -> colDef

  let columnNum = function
  | SingleTableKeyCol (_, _, sinfo) -> sinfo.ColumnNum
  | RelationshipKeyCol (_, _, rinfo) -> rinfo.ColumnNum

type SingleTableKeyColumn = {
  Name: string
  LayoutOrder: LayoutOrder
}
with
  override this.ToString() = sprintf "%A" this

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SingleTableKeyColumns =
  let ofKeyColumns (keyCols: KeyColumn seq) =
    keyCols
    |> Seq.sortBy KeyColumn.columnNum
    |> Seq.map (function
                | SingleTableKeyCol (_, colDef, info) -> { Name = ColumnDef.actualName colDef; LayoutOrder = info.LayoutOrder }
                | _ -> assert false; { Name = "*INVALID COL*"; LayoutOrder = Asc })
    |> Seq.toList

type SingleTableKey = {
  Name: string
  ClusteredType: ClusteredType
  Columns: SingleTableKeyColumn list
}
with
  override this.ToString() = sprintf "%A" this

type ColumnPair = {
  OwnColumnName: string
  ParentColumnName: string
}
with
  override this.ToString() = sprintf "%A" this

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ColumnPairs =
  let ofKeyColumns (keyCols: KeyColumn seq) =
    keyCols
    |> Seq.sortBy KeyColumn.columnNum
    |> Seq.map (function
                | RelationshipKeyCol (_, colDef, info) -> { OwnColumnName = ColumnDef.actualName colDef; ParentColumnName = info.Column }
                | _ -> assert false; { OwnColumnName = "*INVALID COL*"; ParentColumnName = "*INVALID COL*" })
    |> Seq.toList

type RelationshipKey = {
  KeyName: string
  RelationshipTableName: string
  ColumnPairs: ColumnPair list
}
with
  override this.ToString() = sprintf "%A" this

type DefaultConstraint = {
  Name: string
  ColumnName: string
  Value: string
}
with
  override this.ToString() = sprintf "%A" this

type Key =
  | PrimaryKey of SingleTableKey
  | UniqueKey of SingleTableKey
  | Index of SingleTableKey
  | ForeignKey of RelationshipKey
  | DefaultConstraint of DefaultConstraint
with
  override this.ToString() = sprintf "%A" this

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Key =
  open Basis.Core

  let private nameMap = System.Collections.Generic.Dictionary<string, int>()
  let naming name =
    match nameMap.TryGetValue(name) with
    | false, _ ->
      nameMap.Add(name, 1)
      name
    | true, count -> 
      nameMap.[name] <- count + 1
      name + "_" + string (count + 1)
  let conv tableName (colDef: ColumnDef) =
    let f colDef = function
    | SimpleAttr "PK" -> Some (SingleTableKeyCol (PrimaryKeyName (naming ("PK_" + tableName)), colDef, PrimaryKey.defaultInfo))
    | ComplexAttr ("PK", [value]) ->
        let info = SingleTableKeyInfo.parseIndexAttr PrimaryKey.defaultInfo (value |> Str.splitBy "." |> Array.toList)
        match info.KeyNamePrefix with
        | Default prefix -> Some (SingleTableKeyCol (PrimaryKeyName (naming (prefix + "_" + tableName)), colDef, info))
        | UserSpecified prefix -> Some (SingleTableKeyCol (PrimaryKeyName (prefix + "_" + tableName), colDef, info))
    | SimpleAttr "unique" -> Some (SingleTableKeyCol (UniqueKeyName (naming ("UQ_" + tableName)), colDef, UniqueKey.defaultInfo))
    | ComplexAttr ("unique", [value]) ->
        let info = SingleTableKeyInfo.parseIndexAttr UniqueKey.defaultInfo (value |> Str.splitBy "." |> Array.toList)
        match info.KeyNamePrefix with
        | Default prefix -> Some (SingleTableKeyCol (UniqueKeyName (naming (prefix + "_" + tableName)), colDef, info))
        | UserSpecified prefix -> Some (SingleTableKeyCol (UniqueKeyName (prefix + "_" + tableName), colDef, info))
    | SimpleAttr "index" -> Some (SingleTableKeyCol (IndexName (naming ("IX_" + tableName)), colDef, Index.defaultInfo))
    | ComplexAttr ("index", [value]) ->
        let info = SingleTableKeyInfo.parseIndexAttr Index.defaultInfo (value |> Str.splitBy "." |> Array.toList)
        match info.KeyNamePrefix with
        | Default prefix -> Some (SingleTableKeyCol (IndexName (naming (prefix + "_" + tableName)), colDef, info))
        | UserSpecified prefix -> Some (SingleTableKeyCol (IndexName (prefix + "_" + tableName), colDef, info))
    | ComplexAttr ("FK", [value]) ->
        let info = RelationshipKeyInfo.parseIndexAttr (value |> Str.splitBy "." |> Array.toList)
        match info.Prefix with
        | None -> Some (RelationshipKeyCol (ForeiginKeyName (naming ("FK_" + tableName + "_" + info.Table), info.Table), colDef, info))
        | Some prefix -> Some (RelationshipKeyCol (ForeiginKeyName ((prefix + "_" + tableName + "_" + info.Table), info.Table), colDef, info))
    | ComplexAttr ("default", [value]) ->
        let colName = ColumnDef.actualName colDef
        Some (DefaultConstraintCol (DefaultConstraintName ("DF_" + tableName + "_" + colName, colName, value)))
    | _ -> None
    
    colDef
    |> ColumnDef.attributes
    |> Seq.choose (f colDef)

  let clusteredType (keyCols: KeyColumn seq) =
    let clustered, nonclustered =
      keyCols
      |> Seq.toList
      |> List.partition (function SingleTableKeyCol (_, _, info) -> info.ClusteredType = Clustered)

    match clustered, nonclustered with
    | [], [] -> assert false; NonClustered
    | _,  [] -> Clustered
    | [], _ -> NonClustered
    | _ ->
        let kn = keyCols |> Seq.head |> KeyColumn.keyWithName
        failwithf "クラスタ化インデックスの設定はインデックスグループ内で統一する必要がありますが、%Aでは一致していません。" kn

  let create keyCols = function
  | PrimaryKeyName name -> PrimaryKey { Name = name; ClusteredType = clusteredType keyCols; Columns = SingleTableKeyColumns.ofKeyColumns keyCols }
  | UniqueKeyName name -> UniqueKey { Name = name; ClusteredType = clusteredType keyCols; Columns = SingleTableKeyColumns.ofKeyColumns keyCols }
  | IndexName name -> Index { Name = name; ClusteredType = clusteredType keyCols; Columns = SingleTableKeyColumns.ofKeyColumns keyCols }
  | ForeiginKeyName (name, table) -> ForeignKey { KeyName = name; RelationshipTableName = table; ColumnPairs = ColumnPairs.ofKeyColumns keyCols }
  | DefaultConstraintName (name, col, value) -> DefaultConstraint { Name = name; ColumnName = col; Value = value }

  let collectTableKeys (tableDef: TableDef) : Key list =
    nameMap.Clear()
    let temp =
      tableDef.ColumnDefs
      |> Seq.collect (conv tableDef.TableDefName)
      |> Seq.groupBy (KeyColumn.keyWithName)
    let res =
      temp
      |> Seq.map (fun (kn, keyCols) -> create keyCols kn)
      |> Seq.toList
    res

  let collectKeys (elems: Element list) : (TableDef * Key list) list =
    elems
    |> List.choose (function TableDef t -> Some (t, collectTableKeys t) | _ -> None)