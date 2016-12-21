module Cells exposing (newCells, empty, Cells, State, stateToColor, nextStates, toggleState, view)

import Basics.Extra exposing ((=>))
import Color exposing (Color)
import Random exposing (Generator)
import Random.Extra exposing (sample)
import Html exposing (Attribute, Html, div, text)
import Html.Events exposing (onClick)
import Graph exposing (Node, NodeContext, NodeId, fromNodeLabelsAndEdgePairs, Graph, alongIncomingEdges)
import CssBasics exposing (Declaration, toStyleAttribute, CssValue(..), UnitType(..))


(<--) : (a -> b -> c) -> b -> (a -> c)
(<--) f b =
  (flip f) b


infixl 0 <--

type alias Size =
  Int


type State
  = Living
  | Dead


reverseState : State -> State
reverseState state =
  case state of
    Living ->
      Dead

    Dead ->
      Living


stateToColor : State -> Color
stateToColor state =
  case state of
    Living ->
      Color.lightGreen

    Dead ->
      Color.darkGreen


type alias Cells =
  { cells : Graph State ()
  , size : Size
  }


empty : Cells
empty =
  { cells = Graph.empty
  , size = 0
  }


newStates : Int -> Generator (List State)
newStates length =
  sample [ Living , Dead ]
    |> Random.map (Maybe.withDefault Dead)
    |> Random.list length


newCells : Size -> Generator Cells
newCells size =
  newStates (size ^ 2)
    |> Random.map (\states -> fromNodeLabelsAndEdgePairs states (edgePairs size))
    |> Random.map (\cells -> Cells cells size)


edgePairs : Size -> List (NodeId, NodeId)
edgePairs size =
  List.range 0 (size ^ 2 - 1)
    |> List.concatMap (\id ->
        [ rightNodeId size id => id
        , leftNodeId size id => id
        , topNodeId size id => id
        , bottomNodeId size id => id
        , rightTopNodeId size id => id
        , rightBottomNodeId size id => id
        , leftTopNodeId size id => id
        , leftBottomNodeId size id => id
        ]
      )


rightNodeId : Size -> NodeId -> NodeId
rightNodeId size id =
  if rem id size == size - 1 then
    id - (size - 1)
  else
    id + 1


leftNodeId : Size -> NodeId -> NodeId
leftNodeId size id =
  if rem id size == 0 then
    id + (size - 1)
  else
    id - 1


topNodeId : Size -> NodeId -> NodeId
topNodeId size id =
  rem (id - size + (size ^ 2)) (size ^ 2)


bottomNodeId : Size -> NodeId -> NodeId
bottomNodeId size id =
  rem (id + size) (size ^ 2)


rightTopNodeId : Size -> NodeId -> NodeId
rightTopNodeId size =
  rightNodeId size >> topNodeId size


rightBottomNodeId : Size -> NodeId -> NodeId
rightBottomNodeId size =
  rightNodeId size >> bottomNodeId size


leftTopNodeId : Size -> NodeId -> NodeId
leftTopNodeId size =
  leftNodeId size >> topNodeId size


leftBottomNodeId : Size -> NodeId -> NodeId
leftBottomNodeId size =
  leftNodeId size >> bottomNodeId size


getRow : Int -> Cells -> List (Node State)
getRow index { cells, size } =
  let
    ids = List.range (size * index) (size * (index + 1) - 1)
  in
    List.filterMap (\id -> Graph.get id cells) ids
      |> List.sortBy (.node >> .id)
      |> List.map .node


nextStates : Cells -> Cells
nextStates ({ cells } as record) =
  { record | cells = Graph.mapContexts (nextState cells) cells }


nextState : Graph State () -> NodeContext State () -> NodeContext State ()
nextState cells cell =
  let
    livingCount =
      alongIncomingEdges cell
        |> List.filterMap (\id -> Graph.get id cells)
        |> List.map (.node >> .label)
        |> List.filter ((==) Living)
        |> List.length

    node =
      cell.node
  in
    { cell | node = { node | label = rule livingCount cell.node.label } }


rule : Int -> State -> State
rule count state =
  case (count, state) of
    (2, Living) ->
      Living

    (3, _) ->
      Living

    (_, _) ->
      Dead


toggleState : NodeId -> Cells -> Cells
toggleState id ({ cells } as record) =
  let
    nodeUpdate : Node State -> Node State
    nodeUpdate node =
      if node.id == id then
        { node | label = reverseState node.label }
      else
        node


    contextUpdate : NodeContext State () -> NodeContext State ()
    contextUpdate cell =
      { cell | node = nodeUpdate cell.node }

  in
    { record | cells = Graph.mapContexts contextUpdate cells }


view : Cells -> Html NodeId
view ({ cells, size } as rec) =
  List.range 0 (size - 1)
    |> List.map (getRow <-- rec)
    |> List.map (List.map viewCell)
    |> List.map (div [ toStyleAttribute rowStyle ])
    |> div [ toStyleAttribute gridStyle ]


viewCell : Node State -> Html NodeId
viewCell { id, label } =
  div
    [ onClick id
    , cellStyle label |> toStyleAttribute
    ]
    []


cellStyle : State -> List (Declaration number)
cellStyle state =
  [ "width" => Unit 1 Em
  , "height" => Unit 1 Em
  , "background-color" => (Col <| stateToColor state)
  ]


rowStyle : List (Declaration number)
rowStyle =
  [ "display" => Str "flex"
  ]


gridStyle : List (Declaration number)
gridStyle =
  [ "display" => Str "flex"
  , "flex-direction" => Str "column"
  ]