module Main where

import Prelude

import Effect (Effect)

import Data.Array (snoc, modifyAt, elemIndex)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)

import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM

import Partial.Unsafe (unsafePartial)

import React as React
import ReactDOM as ReactDOM
import Reframe as Reframe
import Signal.Channel as C

import Example.TodoList (todoListClass)
import Example.Types (Todo(..), TodoStatus(..))

main :: Effect Unit
main = void $ do
  window <- DOM.window

  document <- DOM.document window

  let
      node = DOM.toNonElementParentNode document

  element <- DOM.getElementById "example" node

  let
      element' = unsafePartial (fromJust element)
  
  events <- C.channel NOOP 
  loop <- Reframe.app (AppState {todo : Nothing, todos : []}) events

  ReactDOM.render (React.createLeafElement mainClass { }) element'

data AppEvent = NOOP
              | Add Todo 
              | Edit Todo 
              | Done Todo 
              | Clear Todo 

newtype AppState = AppState { todo :: Maybe Todo
                            , todos :: Array Todo
                            }

instance appEvent :: Reframe.EventClass AppEvent AppState where 
  step NOOP (AppState state)          = AppState state
  step (Add todo') (AppState state)   = AppState $ state { todo = Nothing 
                                                         , todos = snoc state.todos todo'}
  step (Edit todo') (AppState state)  = AppState $ state { todo = Just todo' }
  step (Done todo') (AppState state)  = AppState $ state { todos = setStatus state.todos todo' TodoDone }
  step (Clear todo') (AppState state) = AppState $ state { todos = setStatus state.todos todo' TodoCleared } 

setStatus :: Array Todo -> Todo -> TodoStatus -> Array Todo
setStatus todos todo status = fromMaybe todos $ do
  i <- elemIndex todo todos
  modifyAt i (\(Todo a) -> Todo a { status = status }) todos

mainClass :: React.ReactClass { }
mainClass = React.component "Main" component
  where
  component this =
    pure { state:
            { todo: Nothing
            , todos: []
            }
         , render: render <$> React.getState this
         }
    where
    render
      { todo
      , todos
      } =
      React.createLeafElement todoListClass
        { todos
        , todo

        , onAdd: \todo' -> React.modifyState this \a ->
            a { todo = Nothing
              , todos = snoc a.todos todo'
              }

        , onEdit: \todo' -> React.modifyState this
            _ { todo = Just todo'
              }

        , onDone: \todo' -> React.modifyState this \a ->
            a { todos = setStatus a.todos todo' TodoDone
              }

        , onClear : \todo' -> React.modifyState this \a ->
            a { todos = setStatus a.todos todo' TodoCleared
              }
        }

    setStatus todos todo status = fromMaybe todos $ do
      i <- elemIndex todo todos

      modifyAt i (\(Todo a) -> Todo a { status = status }) todos
