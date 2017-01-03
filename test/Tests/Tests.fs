module Tests

open Expecto
open ExpectoFsCheck
open FsCheck
open Source

let listArb = Arb.generate<int> |> Gen.listOf |> Gen.filter (fun ls -> List.length ls > 0) |> Arb.fromGen

[<Tests>]
let tests =
  testList "samples" [
    testCase "X should return F#" <| fun _ ->
      let target = new Source.Source()
      Expect.equal target.X "F#" "X result was not F#"
    
    testProperty "Addition is commutative" <| fun a b ->
      a + b = b + a

    (fun a -> List.length a > 0) |> FsCheck.Prop.forAll listArb |> testProperty "List length"
  ]