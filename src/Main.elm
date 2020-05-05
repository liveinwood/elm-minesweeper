module Main exposing (..)

import Array
import List.Extra exposing (andThen, uniqueBy)
import Matrix exposing (Matrix)
import Neighbours exposing (MatrixTopology(..), neighbours)
import Result exposing (withDefault)
import Tuple exposing (first, second)



-- MODEL
-- cellの座標


type alias Position =
    ( Int, Int )



-- cellの状態


type CellStatus
    = NOT_OPEN
    | OPEN



-- cellは座標、状態、地雷がの有無、周辺地雷の数


type alias Cell =
    { position : Position
    , status : CellStatus
    , mine : Bool
    , mines : Int
    }


emptyCell : Cell
emptyCell =
    Cell (0,0) NOT_OPEN False 0



-- boardはcellの２次元配列


type alias Board =
    Matrix Cell



-- UPDATE


openCell : Cell -> Cell
openCell c = { c | status = OPEN }

getPosition : Cell -> Position
getPosition c = c.position

-- 指定座標のセルを返す


getAt : Position -> Board -> Cell
getAt ( x, y ) b =
    Matrix.get x y b |> withDefault emptyCell


open : Position -> Board -> Maybe Board
open p b = 
    let cells = getCellsToBeOpened (getAt p b) b []
    in
    if List.isEmpty cells then Nothing
    else Matrix.map (\c -> if List.member c cells then c else openCell c) b |> Just

-- 近隣のセルを返す
getNeighbourCells : Cell -> Board -> List Cell
getNeighbourCells c b = neighbours Plane (first c.position) (second c.position) b |> Array.toList



getCellsToBeOpened : Cell -> Board -> List Cell -> List Cell
getCellsToBeOpened c b explored = 
    if c.mine then []
    else if c.mines /= 0 then [c]
    --else cell :: (getNeighbourCells cell b |> andThen (\c1 -> getCellsToBeOpened c1 b)) |> uniqueBy (\c2 -> c2.position)
    else 
        let
            neighboursToBeExplored = getNeighbourCells c b |> List.filter (\cc -> not (List.member cc explored))
        in
        c :: (neighboursToBeExplored |> andThen (\c1 -> getCellsToBeOpened c1 b (neighboursToBeExplored ++ explored)) |> uniqueBy (\c2 -> c2.position))
        
