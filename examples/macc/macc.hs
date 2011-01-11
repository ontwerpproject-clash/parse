{-# LANGUAGE TemplateHaskell , TypeOperators, RecordWildCards,
ScopedTypeVariables, TypeFamilies #-}
module MultiplyAccumulate where
import CLasH.HardwareTypes
--type Word = SizedInt D8
type Word = Signed D8

{-# ANN macc TopEntity #-}
{-# ANN macc (InitState 'initAccum) #-}
macc :: (Word , Word ) -> State Word -> (State Word , Word )
macc (x , y) (State acc) = (State u, u)
  where u = acc + x * y
  
initAccum :: Word
initAccum = 0

{-# ANN program TestInput #-}
program :: [(Word , Word )]
program =
  [(4, 2) -- 4 * 2 + 0 = 8
  , (1, 3) -- 1 * 3 + 8 = 11
  , (2, 2) -- 2 * 2 + 11 = 15
  ]
  
{-
simulate _ _ [] = []
simulate arch state (i : input) = o : out
  where
    (state' , o) = arch i state
    out = simulate arch state' input
-}
