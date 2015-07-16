module Main where

import Debug
import Markdown
import Signal as Signal exposing (Mailbox, Address, mailbox)

import Html exposing (Html, textarea, div, text, fromElement)
import Html.Attributes exposing (value, rows, cols, style)
import Html.Events exposing (on, targetValue)

type alias State = {content: String}

type Action = Reset | Update String

actions : Mailbox Action
actions = mailbox Reset

emptyState : State
emptyState = {content=""}

view : State -> Html
view state =
  div [style [("margin", "50px auto"),
              ("width", "80%")]]
        [ textarea [ style [ ("float", "left")
                           , ("width", "40%")
                           , ("padding", "10px")]
                   , rows 50
                   , cols 40
                   , value (Debug.watch "updater" state.content)
                   , on "input" targetValue
                          (\t -> Signal.message actions.address (Update t))] []
        , div [style [ ("float", "left")
                     , ("width", "40%")
                     , ("padding", "10px")]]
                [ Markdown.toElement state.content |> fromElement ]]

update : Action -> State -> State
update action state =
  case action of
    Reset -> state
    Update content -> { state | content <- content }

state : Signal State
state =
  Signal.foldp update emptyState actions.signal

main : Signal Html
main =
  Signal.map view state