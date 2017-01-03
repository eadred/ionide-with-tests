module Tests

open Expecto
open ExpectoFsCheck
open FsCheck
open Source

let listArb = Arb.generate<int> |> Gen.listOf |> Gen.filter (fun ls -> List.length ls > 0) |> Arb.fromGen

let bigIntGen = Gen.choose(11, 20)
let smallIntGen = Gen.choose(0, 9)

let intPairs = (fun x y -> (x,y)) <!> bigIntGen <*> smallIntGen |> Arb.fromGen

let charListGen = Arb.generate<char> |> Gen.listOfLength 10

let charListShrink ls = 
  match ls with
  | [] -> Seq.empty
  | [c] -> Seq.empty
  | _ -> seq {
    for startIdx in (List.length ls - 1)..(-1)..0 do
    for count in (List.length ls - startIdx - 1)..(-1)..1 do
    yield ls |> List.skip startIdx |> List.take count
  }

let charListArb = Arb.fromGenShrink (charListGen, charListShrink)

[<Tests>]
let tests =
  testList "samples" [
    testCase "X should return F#" <| fun _ ->
      let target = new Source.Source()
      Expect.equal target.X "F#" "X result was not F#"
    
    testProperty "Addition is commutative" <| fun a b ->
      a + b = b + a

    (fun a -> List.length a > 0) |> Prop.forAll listArb |> testProperty "List length"

    (fun (a,b) -> a > b) |> Prop.forAll intPairs |> testProperty "Multi-generator test"

    //To see if this test (which will fail) will shrink the initially generated 10 item lists into smaller and smaller lists
    //(fun ls -> List.head ls = 'a') |> Prop.forAll charListArb |> testProperty "Shrinker test"
  ]