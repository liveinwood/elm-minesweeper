module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Matrix exposing (generate, set)
import Set
import Test exposing (..)


suite : Test
suite =
    describe "The Main module"
        [ describe "getNeighbourCells" <|
            [ test "近隣のセルを返す" <|
                \_ ->
                    --   0 1 2 3 4
                    -- 0 - - - - -
                    -- 1 - - - - -
                    -- 2 - - - - -
                    -- 3 - - - @ -
                    -- 4 - - - - -
                    let
                        cell =
                            Cell ( 3, 3 ) NOT_OPEN True 0

                        board =
                            generateBoard 5 5

                        expect =
                            Set.fromList [ ( 3, 2 ), ( 4, 2 ), ( 4, 3 ), ( 4, 4 ), ( 3, 4 ), ( 2, 4 ), ( 2, 3 ), ( 2, 2 ) ]
                    in
                    Expect.equalSets (getNeighbourCells cell board |> List.map getPosition |> Set.fromList) expect
            , test "近隣のセルを返す(角を選択)" <|
                \_ ->
                    --   0 1 2 3 4
                    -- 0 @ - - - -
                    -- 1 - - - - -
                    -- 2 - - - - -
                    -- 3 - - - - -
                    -- 4 - - - - -
                    let
                        cell =
                            Cell ( 0, 0 ) NOT_OPEN True 0

                        board =
                            generateBoard 5 5

                        expect =
                            Set.fromList [ ( 1, 0 ), ( 1, 1 ), ( 0, 1 ) ]
                    in
                    Expect.equalSets (getNeighbourCells cell board |> List.map getPosition |> Set.fromList) expect
            ]
        , describe "getCellsToBeOpened"
            [ test "地雷を踏んだら空リストを返す" <|
                \_ ->
                    let
                        cell =
                            Cell ( 2, 2 ) NOT_OPEN True 1

                        board =
                            generateBoard 3 3 |> set 2 2 (Cell ( 2, 2 ) NOT_OPEN True 1)
                    in
                    Expect.equal (getCellsToBeOpened cell board []) []
            , test "近隣に地雷がある安全なセルを選んだら、そのセルだけのリストを返す" <|
                \_ ->
                    let
                        cell =
                            Cell ( 2, 2 ) NOT_OPEN False 1

                        board =
                            generateBoard 3 3 |> set 2 2 (Cell ( 2, 2 ) NOT_OPEN False 1)
                    in
                    Expect.equal (getCellsToBeOpened cell board []) [ cell ]
            , test "近隣に地雷がない安全なセルを選んだら、近隣のセルと選んだセルのリストを返す(1)" <|
                \_ ->
                    --   0 1 2 3 4
                    -- 0 - - * - -
                    -- 1 - - - - -
                    -- 2 * - @ - *
                    -- 3 - - - - -
                    -- 4 - - * - -
                    let
                        cell =
                            Cell ( 2, 2 ) NOT_OPEN False 0

                        board =
                            generateBoard 5 5
                                |> set 1 1 (Cell ( 1, 1 ) NOT_OPEN False 1)
                                |> set 2 1 (Cell ( 2, 1 ) NOT_OPEN False 1)
                                |> set 3 1 (Cell ( 3, 1 ) NOT_OPEN False 1)
                                |> set 3 2 (Cell ( 3, 2 ) NOT_OPEN False 1)
                                |> set 3 3 (Cell ( 3, 3 ) NOT_OPEN False 1)
                                |> set 2 3 (Cell ( 2, 3 ) NOT_OPEN False 1)
                                |> set 1 3 (Cell ( 1, 3 ) NOT_OPEN False 1)
                                |> set 1 2 (Cell ( 1, 2 ) NOT_OPEN False 1)

                        expect =
                            Set.fromList [ ( 1, 1 ), ( 2, 1 ), ( 3, 1 ), ( 3, 2 ), ( 3, 3 ), ( 2, 3 ), ( 1, 3 ), ( 1, 2 ), ( 2, 2 ) ]
                    in
                    Expect.equalSets (getCellsToBeOpened cell board [] |> List.map getPosition |> Set.fromList) expect

            , test "近隣に地雷がない安全なセルを選んだら、近隣のセルと選んだセルのリストを返す(2)" <|
                \_ ->
                    --   0 1 2 3 4
                    -- 0 * 1 + 1 *
                    -- 1 1 1 + 1 1
                    -- 2 + + @ + +
                    -- 3 1 1 + 1 1
                    -- 4 * 1 + 1 *
                    let
                        cell =
                            Cell ( 2, 2 ) NOT_OPEN False 0

                        board =
                            generateBoard 5 5
                                |> set 0 0 (Cell ( 0, 0 ) NOT_OPEN True 1)
                                |> set 1 0 (Cell ( 1, 0 ) NOT_OPEN False 1)
                                |> set 3 0 (Cell ( 3, 0 ) NOT_OPEN False 1)
                                |> set 4 0 (Cell ( 4, 0 ) NOT_OPEN True 1)

                                |> set 0 1 (Cell ( 0, 1 ) NOT_OPEN False 1)
                                |> set 1 1 (Cell ( 1, 1 ) NOT_OPEN False 1)
                                |> set 3 1 (Cell ( 3, 1 ) NOT_OPEN False 1)
                                |> set 4 1 (Cell ( 4, 1 ) NOT_OPEN False 1)

                                |> set 0 3 (Cell ( 0, 3 ) NOT_OPEN False 1)
                                |> set 1 3 (Cell ( 1, 3 ) NOT_OPEN False 1)
                                |> set 3 3 (Cell ( 3, 3 ) NOT_OPEN False 1)
                                |> set 4 3 (Cell ( 4, 3 ) NOT_OPEN False 1)

                                |> set 0 4 (Cell ( 0, 4 ) NOT_OPEN True 1)
                                |> set 1 4 (Cell ( 1, 4 ) NOT_OPEN False 1)
                                |> set 3 4 (Cell ( 3, 4 ) NOT_OPEN False 1)
                                |> set 4 4 (Cell ( 4, 4 ) NOT_OPEN True 1)
                        expect =
                            Set.fromList [ (1,0),(2,0),(3,0),(0,1),(1,1),(2,1),(3,1),(4,1),(0,2),(1,2),(2,2),(3,2),(4,2),(0,3),(1,3),(2,3),(3,3),(4,3),(1,4),(2,4),(3,4)]
                    in
                    Expect.equalSets (getCellsToBeOpened cell board [] |> List.map getPosition |> Set.fromList) expect
            ]
        ]



-- UTILITY


generateBoard : Int -> Int -> Board
generateBoard x y =
    generate x y (\a b -> Cell ( a, b ) NOT_OPEN False 0)
