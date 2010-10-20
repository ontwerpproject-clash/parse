{-
De typeparameter a kan je gebruiken om extra informatie aan datastructuur toe te voegen.

Je zou bijvoorbeeld een functie kunnen maken die bepaald wat waar getekend moet worden.
plaatsElementen :: ArchElem a -> ArchElem Positie

en dan een teken functie die een argument van het type ArchElem Positie verwacht.


-}
data ArchElem a =
    Function Id (Maybe Name) [InPort] OutPort ([ArchElem a],[Wire a]) a |
    Operator Id OpType [In] Out a |
    Literal Id Value Out a |

    Mux Id   
       [In] -- ^ de ingangen
        Out -- ^ de uitgang
        In  -- ^ de select ingang, die selecteert welke ingang er doorgaat naar de uitgang
        a |
    
    Register Id (Maybe In) Out a
    deriving (Show,Eq,Ord)

data Wire a = Wire (Maybe Name) PortId PortId a     deriving (Show,Eq,Ord)
--                     naam     source dest

data OpType =
  OpPlus |
  OpMin |
  OpMult |
  OpDiv |
  OpAnd |
  OpOr |
  OpXor
  -- ...
  deriving (Show,Eq,Ord)

type Name = String
type Value = String

type PortId = String
type Id = String

type In = PortId
type Out = Port

type InPort = Port
type OutPort = Port
data Port = SinglePort PortId | MultiPort PortId [Out]     deriving (Show,Eq,Ord)
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

