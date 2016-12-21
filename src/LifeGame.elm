module LifeGame exposing (..)

import Basics.Extra exposing ((=>))
import Color exposing (Color)
import Response exposing (withCmd, withNone)
import Random exposing (Generator)
import Random.Extra exposing (sample)
import Html exposing (..)
import Time

import Graph exposing (Node, NodeContext, NodeId, fromNodeLabelsAndEdgePairs, Graph, alongIncomingEdges)
import CssBasics exposing (toStyleAttribute)

import Cells exposing (Cells)


main : Program Never LifeGame Msg
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Tick =
  Int


type alias LifeGame =
  { cells : Cells
  , tick : Tick
  }


init : (LifeGame, Cmd Msg)
init =
  { cells = Cells.empty
  , tick = 0
  } |> withCmd (Random.generate NewGame <| Cells.newCells 20)


type Msg
  = NoOp
  | NewGame Cells
  | Reset
  | ToggleState Int
  | NextTick


update : Msg -> LifeGame -> (LifeGame, Cmd Msg)
update msg game =
  case msg of
    NewGame cells ->
      { cells = cells
      , tick = 0
      } |> withNone

    Reset ->
      game
        |> withCmd (Random.generate NewGame <| Cells.newCells 50)

    NextTick ->
      { game
        | tick = game.tick + 1
        , cells = Cells.nextStates game.cells
      } |> withNone

    ToggleState id ->
      { game | cells = Cells.toggleState id game.cells }
        |> withNone

    NoOp ->
      withNone game


subscriptions : LifeGame -> Sub Msg
subscriptions game =
  Time.every Time.second (always NextTick)


view : LifeGame -> Html Msg
view { cells, tick } =
  Cells.view cells
    |> Html.map ToggleState



