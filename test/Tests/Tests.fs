module Tests

open Expecto
open Source

[<Tests>]
let tests =
  testList "samples" [
    testCase "X should return F#" <| fun _ ->
      let target = new Source.Source()
      Expect.equal target.X "F#" "X result was not F#"
  ]