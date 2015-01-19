namespace TableDsl.Core.Tests

open FsUnit
open NUnit.Framework

[<TestFixture>]
module InvalidPrograms =
  open TableDsl
  open Basis.Core

  let shouldNotParse expectedMessages input =
    input
    |> Parser.tryParse
    |> function
       | Success res -> failwithf "should not parse but parsed: %A" res
       | Failure f ->
           match f with
           | Parser.FParsecDefaultMessage msg -> [msg] |> should equal expectedMessages
           | Parser.UserFriendlyMessages errs ->
               errs |> List.map (fun (_, _, x) -> x) |> should equal expectedMessages

  [<Test>]
  let ``nullable type in coltype def`` () =
    """///名前の基底型1
       coltype Name1(@n)[名前1] = nvarchar(@n)
       ///名前の基底型2
       coltype Name2(@n)[名前2] = { nullable(Name1(@n)) with collate = JAPANESE_BIN }
       ///名前の型
       coltype Name[名前] = { Name2(256) with default = empty }

       ///ユーザテーブル。
       table Users[ユーザ] = {
         _: Name
       }"""
    |> shouldNotParse ["nullableを元に型を定義することは出来ません。"]

  [<Test>]
  let ``nullable type in simple coltype def`` () =
    """///名前の基底型1
       coltype Name1(@n)[名前1] = nvarchar(@n)
       ///名前の基底型2
       coltype Name2(@n)[名前2] = nullable(Name1(@n))
       ///名前の型
       coltype Name[名前] = { Name2(256) with default = empty }

       ///ユーザテーブル。
       table Users[ユーザ] = {
         _: Name
       }"""
    |> shouldNotParse ["nullableを元に型を定義することは出来ません。"]

  [<Test>]
  let ``alias def in the enum based def`` () =
    """
    coltype foo = int
    coltype Platform =
      | iOS = 1
      | Android = 2
    based { foo with default = 1 }
    """
    |> shouldNotParse ["列挙型の定義(Platform)の基底型には組み込み型しか指定できませんが、列定義(foo)が指定されました。"]

  [<Test>]
  let ``dup type variable`` () =
    "coltype nvarchar(@n, @n) = { nvarchar(@n) with collate = Japanese_BIN }"
    |> shouldNotParse ["型nvarcharの定義で型変数@nが重複しています。"]

  [<Test>]
  let ``can't inherit enum type`` () =
    """
    coltype Platform =
      | iOS = 1
      | Android = 2
    based { int with default = 1 }
    
    coltype Platform2 =
      | WP = 3
    based Platform"""
    |> shouldNotParse ["列挙型の定義(Platform2)の基底型には組み込み型しか指定できませんが、他の列挙型(Platform)が指定されました。"]

  [<Test>]
  let ``type not found`` () =
    """
    table Users = {
      Name: string
    }
    """
    |> shouldNotParse ["stringという型が見つかりませんでした。"]
