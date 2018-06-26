module Example.TodoList where

import Prelude

import Effect (Effect)

import Data.Array (filter)
import Data.Maybe (Maybe)
import Signal as S

import React as React
import React.DOM as DOM
import React.DOM.Props as Props
import Reframe as Reframe

import Example.TodoForm (todoFormClass)
import Example.TodoItem (todoItemClass)
import Example.Types (Todo(..), TodoStatus(..))

type TodoListProps
  = { todos :: Array Todo
    , todo :: Maybe Todo
    , onAdd :: Todo -> Effect Unit
    , onEdit :: Todo -> Effect Unit
    , onDone :: Todo -> Effect Unit
    , onClear :: Todo -> Effect Unit
    }

type TestProps = { message :: String }

renderTest :: TestProps -> String -> React.ReactElement 
renderTest { message } val = DOM.div [] [ DOM.div' [ DOM.text message ] 
                                       , DOM.div' [ DOM.text val ]]

reframeTest :: React.ReactClass TestProps
reframeTest = Reframe.component "reframeTest" renderTest s 
  where s = S.constant "bar"

todoListClass :: React.ReactClass TodoListProps
todoListClass = React.component "TodoList" component
  where
  component this =
    pure { state: {}
         , render: render <$> React.getProps this
         }
    where
    render
      { todos
      , todo
      , onAdd
      , onEdit
      , onDone
      , onClear
      } =
      DOM.div
        [ ]
        [ React.createLeafElement reframeTest { message : "foo" }
        , React.createLeafElement todoFormClass
            { todo
            , onEdit
            , onAdd
            }
        , DOM.ol
            [ ]
            (renderItem <$> todos')
        ]
      where
      todos' = filter (\(Todo { status }) -> TodoCleared /= status) todos

      renderItem todo' @ Todo { status } =
        DOM.li
          [ ]
          [ React.createLeafElement todoItemClass { todo: todo' }
          , DOM.button
              [ Props._type "button"
              , Props.onClick onClick
              ]
              [ DOM.text text ]
          ]
        where
        text =
          case status of
               TodoPending -> "Done"
               TodoDone -> "Clear"
               _ -> ""

        onClick event =
          case status of
               TodoPending -> onDone todo'
               TodoDone -> onClear todo'
               _ -> pure unit
