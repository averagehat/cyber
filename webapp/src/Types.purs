module Types where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Symbol (SProxy(..))
import Record as R

type Passcode = String 
type Message = String
type OperationType = String
type Id = String

{-  simple product types  -}
data Operation = Lock Passcode | Unlock Passcode | Take  | Put String  | Copy  | Enter Id | Use | RootOp String

data Payload = Data String | Exit  | Empty
data Failure = NodeLocked | BadPassCode | BadID | NoData
data Success = Success

 -- each step moves. has some payload?
type Virus = { speed :: Int, effect :: Consequence} 
data Consequence = DataLoss Int  | Viral Virus 

data UserItem = Disk String | Infection Virus
type Operator  = { handle :: String, inventory :: Array UserItem }

type Node_ r = { children :: Array Node, name :: String, id_ :: Id, payload :: Payload | r } 

data Node =  Open  ( Node_ () ) 
           | Locked   ( Node_ (passCode :: String) )
           | Secured ( Node_ (consequence :: Consequence, triggers :: Array Operation) )


children_ = SProxy :: SProxy "children"
name_ = SProxy :: SProxy "name"
id__ = SProxy :: SProxy "id_"
consequence_ = SProxy :: SProxy "consequence"
triggers_ = SProxy :: SProxy "triggers"
passCode_ = SProxy :: SProxy "passCode"

_rmPassCode = R.delete passCode_
_rmConsequence = R.delete consequence_
_rmTriggers = R.delete triggers_

get_ f_ (Open n) =   R.get f_ n
get_ f_ (Locked n) = R.get f_ n
get_ f_ (Secured n) = R.get f_ n

type BaseModel r = { tree :: Node, user :: Operator | r }
-- these always get paired with their operations for now...  

startState = {tree: tree', user: user', alerts: 0, loggedOut: false}
user' = { handle:  "User1", inventory: [] }
makeRec = { children: _, name: _, id_: _, payload: _ }

tree' = Open { 
     children: [inner],
     name :  "startNode",
     id_ : "0", 
     payload: Empty }
 where 
  inner = Locked{
     children : [Open $ makeRec [] "outplace" "001" Exit],
     name : "N",
     id_  : "NID1",
     payload :  Data "foo",
     passCode : "bar"
   }

derive instance genericOperation :: Generic Operation _
derive instance eqOperation :: Eq Operation
instance showOperation :: Show Operation where
  show = genericShow

-- derive instance genericVirus :: Generic Virus _
-- instance showVirus :: Show Virus where
--   show x = genericShow x

derive instance genericConsequence :: Generic Consequence _
instance showConsequence :: Show Consequence where
  show x = genericShow x

derive instance genericPayload :: Generic Payload _
instance showPayload :: Show Payload where
  show = genericShow

derive instance genericNode :: Generic Node _
instance showNode :: Show Node where
  show x = genericShow x

derive instance genericUserItem :: Generic UserItem _
instance showUserItem :: Show UserItem where
  show x = genericShow x

-- derive instance genericOperator :: Generic Operator _
-- instance showOperator :: Show Operator where
--   show x = genericShow x

-- data NodeType = Locked' | Open' | Secured' | LoggedOut'
-- nodeType (Locked _) = "LockedNode"
-- nodeType (Open _) = "UnlockedNode"
-- nodeType (Secured _) = "SecurityNode"
-- nodeType LoggedOut = "Done"



data OpType = Lock_ | Unlock_ | Take_ | Put_ | Copy_ | Enter_ | Use_ | RootOp_
optypes =  [Lock_ , Unlock_ , Take_ , Put_ , Copy_ , Enter_ , Use_ , RootOp_]
derive instance genericOpType :: Generic OpType _
derive instance eqOpType :: Eq OpType
instance showOpType :: Show OpType where
    show x = genericShow x 

-- getKids' n =  map extract ((extract n).children)
-- extract :: forall r. Node -> { children :: Array Node | r } 
-- extract (Open x) = x
-- extract (Locked x) = x
-- extract (Secured x) = x


--- data State
---  = Open
---  | Locked String -- passcode
---  | Secured Consequence (Array Operation) -- consequence and triggers
---
---type Node = { children :: Array Node, name :: String, id_ :: id, payload :: Payload, state :: State }



-- main = do
--   interface <- RL.createConsoleInterface RL.noCompletion
--   RL.setPrompt "> " 2 interface
--   RL.prompt interface
--   RL.setLineHandler interface $ \s ->
--     if s == "quit"
--        then RL.close interface
--        else do
--         log $ "You typed: " <> s
--         RL.prompt interface
-- main' = do
--   itf <- RL.createConsoleInterface $ completer' startState
--   RL.setLineHandler itf handler
--   result <- RL.question "Well? :> " (\x -> log $ x <> "foo") itf
--   close itf
