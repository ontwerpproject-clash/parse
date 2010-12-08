{-
The Datastructure of the Hardware Visualisation project in Haskell.

Hardware will be specified by this structure, and will be interpreted by the visualisation program. The datastructure has recursive elmenents so function with deeper levels can be constructed too.

To help the visualisation group we added an extra object a to make it possible to specify options, of other usefull extra information.
For example a could be used to add information about the positioning of an object in gui. In this case a would be a Postion item (which is not specified in this document).

Specified hardware by this structure always have a Function as the top node.

-}
module Datastruct where

data ArchElem a =
    Function
        Id
        (Maybe Name)            -- ^ Optional name of the function
        [InPort]                -- ^ List of the inports
        OutPort                 -- ^ Outport of the function
        ([ArchElem a],[Wire a]) -- ^ Functions have deeper architecture. These are specified by a tuple of a list of architectures and a list of wires, which connect the different architectures.
        a |

    Operator
        Id
        OpType
        [In]
        Out
        a |

    Literal
        Id
        Value
        Out
        a |

    Mux Id
       [In] -- ^ List of inports from which the outport is selected
        Out -- ^ The outport
       [In]  -- ^ Inport signal to choise an from the list inports
        a |

    Register
        Id
        (Maybe In)
        Out
        a |

    PortReference
        Port

    deriving (Show,Eq,Ord)

data Wire a =
    Wire
        (Maybe Name)  -- ^ Optional name of the wire
        PortId        -- ^ Source port
        PortId        -- ^ Destination port
        a
    deriving (Show,Eq,Ord)

type Name = String
type Value = String
type OpType = String

type PortId = String
type Id = String

type In = PortId
type Out = Port

type InPort = Port
type OutPort = Port

data Port =
    SinglePort
        PortId |

    MultiPort
        PortId
        [Port]

    deriving (Show,Eq,Ord)
{-
Als een out(port) gewoon 1 enkele waarde teruggeeft,
dan is het een 'SinglePort portId'

Als je nu een out(port) heb die meerdere waardes oplevert,
f :: (Int,Float)
dan heeft die als outport
MultiPort res_f
  [ SinglePort res_f_int,
    SinglePost res_f_float]

En dit kan ook genest worden.
g :: (Int,(Id,String))
dan heeft die als outport
MultiPort res_g
  [ SinglePort res_g_int,
    MultiPort res_g_id_string
      [ SinglePort res_g_id
        SinglePort res_g_string ]
-}

