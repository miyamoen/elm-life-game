module LifeGame exposing (..)

import Basics.Extra exposing ((=>))
import Response exposing (withCmd, withNone)
import Random exposing (Generator)
import Html exposing (..)
import Html.Events exposing (onClick)
import Time

import CssBasics exposing (Declaration, toStyleAttribute, CssValue(..), UnitType(..))

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
  , stopped : Bool
  }


init : (LifeGame, Cmd Msg)
init =
  { cells = Cells.empty
  , tick = 0
  , stopped = False
  } |> withCmd (Random.generate NewGame <| Cells.newCells 20)


type Msg
  = NoOp
  | NewGame Cells
  | Start
  | Stop
  | Reset
  | ToggleState Int
  | NextTick


update : Msg -> LifeGame -> (LifeGame, Cmd Msg)
update msg game =
  case msg of
    NewGame cells ->
      { game | cells = cells, tick = 0 }
        |> withNone

    Reset ->
      game
        |> withCmd (Random.generate NewGame <| Cells.newCells 20)

    Start ->
      { game | stopped = False }
        |> withNone

    Stop ->
      { game | stopped = True }
        |> withNone

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
  if game.stopped then
    Sub.none
  else
    Time.every Time.second (always NextTick)


view : LifeGame -> Html Msg
view ({ cells } as game) =
  div [ toStyleAttribute gameStyle ]
    [ text "Life Game"
    , Cells.view cells
      |> Html.map ToggleState
    , controllerView game
    ]


controllerView : LifeGame -> Html Msg
controllerView { tick, stopped } =
  div [ toStyleAttribute controllerStyle ]
    [ buttonView Start
    , buttonView Stop
    , buttonView Reset
    , div [] [ text <| "tick : " ++ toString tick ]
    ]


buttonView : Msg -> Html Msg
buttonView msg =
  button
    [ onClick msg
    , toStyleAttribute buttonStyle
    ]
    [ text <| toString msg ]


gameStyle : List (Declaration number)
gameStyle =
  [ "padding" => Unit 2 Em
  ]

controllerStyle : List (Declaration number)
controllerStyle =
  [ "display" => Str "flex"
  ]


buttonStyle : List (Declaration number)
buttonStyle =
  []