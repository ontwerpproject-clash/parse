module ParseVHDL where

-- VHDL Imports
import Language.VHDL.AST hiding (Function)

import Datastruct
import ParseTypes

-- These imports are required for easy state sessions
import qualified Control.Monad.Trans.State as State            -- Needs package: transformers
import qualified Data.Accessor.Monad.Trans.State as MonadState -- Needs package: data-accessor-transformers, data-accessor
import qualified Data.Accessor.Template                        -- Needs package: data-accessors-template, data-accessor
import qualified Data.Set as Set

type Types = [(VHDLId, PortId -> Port)]

-- Make a datatype that represents the state
data EnvState = ES {
   counter_   :: Int,
   vhdlFiles_ :: [(VHDLId, DesignFile)],
   types_     :: Types
}

-- Let template-haskell derive easy accessors for the state
--Data.Accessor.Template.deriveAccessors ''EnvState

-- Make a type alias for computations that work with the state made above
type EnvSession a = State.State EnvState a

data Backtrack =
  Backtrack {
    archElem      :: ArchElem (),
    wires         :: [Wire ()],
    prevArchElems :: [ArchElem ()]
  }

