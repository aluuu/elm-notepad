module Main where
import Window
import Debug
import Markdown
import Signal as Signal exposing (Mailbox, Address, mailbox)
import Graphics.Element exposing (Element, right, flow, size, spacer)
import Html exposing (Html, textarea, div, text, fromElement, toElement)
import Html.Attributes exposing (value, rows, cols, style)
import Html.Events exposing (on, targetValue)

type alias State = {content: String, width: Int, height: Int}

type Action = Reset | Update String | Resize (Int, Int)

actions : Mailbox Action
actions = mailbox Reset

emptyState : State
emptyState = {content="", width=1000, height=500}

editor : State -> Element
editor state =
  let
    onEdit = (\t -> Signal.message actions.address (Update t))
    styles = [ ("width", "100%")
                       , ("height", "100%")
                       , ("padding", "10px")]
  in
    textarea [ style styles
             , value (Debug.watch "updater" state.content)
             , on "input" targetValue onEdit] []
    |> toElement (state.width // 3) state.height

preview : State -> Element
preview state = state.content
              |> Markdown.toElement
              |> size (state.width // 3) state.height

view : State -> Element
view state =
  flow right [ editor state
             , spacer 50 state.height
             , preview state]

update : Action -> State -> State
update action state =
  case action of
    Reset -> state
    Update content -> { state | content <- content }
    Resize (width, height) -> {state | width <- width, height <- height}

resize : Signal Action
resize =
  Signal.map Resize Window.dimensions

state : Signal State
state =
  Signal.mergeMany [actions.signal, resize]
  |> Signal.foldp update emptyState


main : Signal Element
main =
  Signal.map view state