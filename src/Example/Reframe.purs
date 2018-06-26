module Reframe
  ( class EventClass 
  , step 
  , component
  , app ) where 

import Prelude 

import Effect.Console (log)

import Signal.Channel as C 
import Signal as S

import React as React
import Record.Extra as RC
import Prim.RowList as RL

class EventClass e a where 
  step :: e -> a -> a

app :: forall e a. (EventClass e a)
    => a 
    -> C.Channel e 
    -> S.Signal a 
app state' events = S.foldp step state' (C.subscribe events)

component :: forall props a rl
           . RL.RowToList props rl 
          => RC.OrdRecord rl props 
          => String 
          -> ({ | props} -> a -> React.ReactElement)
          -> S.Signal a
          -> React.ReactClass { | props}
component name f s1 = React.component name cmp 
  where 
    m = do 
      log "rendering!"
      pure f  

    shouldUpdate this props state = do 
      val <- update 
      log $ show val
      not <$> eq EQ <$> update 
      where update = RC.compareRecord props <$> React.getProps this
 
    cmp this = do 
      log "mounting!"
      pure { state  : {}
           , shouldComponentUpdate : (shouldUpdate this)
           , render : m <*> React.getProps this <*> S.get s1 }
           