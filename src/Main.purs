module Main where

import Prelude

import Effect (Effect)

import Data.Array (snoc, modifyAt, elemIndex)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)

import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM

import Effect.Console (log)
import Partial.Unsafe (unsafePartial)

import React as React
import ReactDOM as ReactDOM
import Reframe as Reframe
import Signal.Channel as C
import Signal as S

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

  let 
    eventsSig = C.subscribe events

  let states = Reframe.app (AppState {todo : Nothing, todos : []}) eventsSig


  ReactDOM.render (React.createLeafElement (mainClass events states) { }) element'

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

mainClass :: C.Channel AppEvent -> S.Signal AppState -> React.ReactClass { }
mainClass channel states = React.component "Main" component
  where
  component this = do
    log "main render"
    pure { state : {}
         , render: (render <$> S.get states)
         }
    where
    render
      (AppState { todo, todos }) =
      React.createLeafElement todoListClass
        { todos
        , todo
        , onAdd   : \todo' -> C.send channel (Add todo')
        , onEdit  : \todo' -> do 
                      log "edit" 
                      C.send channel (Edit todo')
        , onDone  : \todo' -> C.send channel (Done todo')
        , onClear : \todo' -> C.send channel (Clear todo')
        }
