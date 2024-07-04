module TestSort exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Sort

suite : Test
suite =
  Test.describe "lexicographic sort" <|
    [ Test.test "empty" <| \() ->
        Expect.equal
          (Sort.lexicographic [])
          EQ
    , Test.test "LT GT" <| \() ->
        Expect.equal
          (Sort.lexicographic [LT, GT])
          LT
    , Test.test "EQ GT" <| \() ->
        Expect.equal
          (Sort.lexicographic [EQ, GT])
          GT
    ]
