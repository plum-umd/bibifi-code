module Queue (Queue, empty, enqueue, dequeue) where

import qualified Data.List as List

data Queue a = Queue [a] [a]

empty :: Queue a
empty = Queue [] []

enqueue :: Queue a -> a -> Queue a
enqueue (Queue [] []) e = Queue [e] []
enqueue (Queue f b) e = Queue f $ e:b

dequeue :: Queue a -> (Maybe a, Queue a)
dequeue q@(Queue [] []) = ( Nothing, q)
dequeue (Queue [] b) = dequeue $ Queue (List.reverse b) []
dequeue (Queue (f:rest) b) = ( Just f, Queue rest b)

