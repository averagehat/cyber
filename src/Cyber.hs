{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleInstances #-}  -- needed for SourceF CyberTerm instance
{-# LANGUAGE DeriveFunctor #-} 
{-# LANGUAGE LambdaCase #-} 
{-# LANGUAGE DeriveAnyClass #-}

module Cyber (main2) where 

import Lens.Micro 
import Lens.Micro.TH (makeLenses, makeClassy)
import Data.Maybe (fromMaybe)
import Data.Foldable (find)
import Control.Monad.State
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (trace)
import Control.Monad.Trans
import Control.Monad.Free 
import Text.Show.Functions -- provides a `show` instance for functions 
-- import Text.Show.Deriving -- not like the function thing
import Data.Functor.Classes (Show1(..), liftShowsPrec, showsUnaryWith)
type Passcode = String 
type Message = String
type OperationType = String
type Id = String

class Print a  where
  print_ :: a -> String

data Operation = Lock Passcode | Unlock Passcode | Take  | Put String  | Copy  | Enter Id | Use | RootOp String -- Node -- | RootOp (Node -> Node)
  deriving (Show,Read)

instance Print Operation where 
  print_ = show
  

data Virus = Virus { speed :: Int, effect :: Consequence} -- each step moves. has some payload?
  deriving (Show,Eq)
    

data Consequence = DataLoss Int  | Viral Virus  deriving (Show,Eq)

data Payload = Data String | Exit  | Empty deriving (Show,Eq) 


instance Print Node where
  print_ n = let    md = _nodeMetaData n in
             let  kids = map (("Child@" ++) . _id_ . _nodeMetaData) (_children md)  in
                      "Node@" ++ show  (_id_ md, _metaDataPayload md, kids)
  
data MetaData = MetaData { _children :: [Node], _name :: String, _id_ :: Id, _metaDataPayload :: Payload }
  deriving (Show,Eq)
                 
data Node = LockedNode {_passCode :: String, _nodeMetaData :: MetaData } 
          | UnlockedNode { _nodeMetaData :: MetaData }
          | SecurityNode { _nodeMetaData :: MetaData, _consequence :: Consequence, _triggers :: [OperationType] }
          | Done { _nodeMetaData :: MetaData }
  deriving (Eq,Show)

nodeType (LockedNode {}) = "LockedNode"
nodeType (UnlockedNode {}) = "UnlockedNode"
nodeType (SecurityNode {}) = "SecurityNode"
nodeType (Done {}) = "Done"
  
data UserItem = Disk String | Infection Virus  deriving (Show)
data Operator  = Operator { _handle :: String, _inventory :: [UserItem]}
  deriving (Show)
  
makeClassy ''Operator
makeClassy ''MetaData 
makeClassy ''Node
makeClassy ''Payload
instance HasMetaData Node where metaData = nodeMetaData 
instance HasPayload  MetaData where payload = metaDataPayload
instance HasPayload  Node where payload = metaData.payload

type State' = (Node, Operator, Message)

impact :: Consequence -> Operator -> Operator
impact (DataLoss n) u   = u & inventory %~ drop n 
impact (Viral v@(Virus {..})) u = impact effect u & inventory %~ ((Infection v) :)

m_ = ""

step :: Operation -> Node -> Operator -> State'
-- step  (RootOp f) n u                         = (f n, u, m_)
step  (RootOp s) n u                         = (n, u, "no imp.")
step  op n@SecurityNode{..} u = if (print_ op) `elem` _triggers  
  then (n,  impact _consequence u, show _consequence)  
  else step op (UnlockedNode _nodeMetaData) u
step  (Unlock p) n@LockedNode{..}  u      = if p == _passCode then (UnlockedNode _nodeMetaData, u, m_) else (n, u, m_)
step  _  n@(LockedNode{}) u               = (n, u, m_)
step  (Unlock _) n u                      = (n, u, m_)
step  op n@(UnlockedNode {..}) u   = step' op (n ^. metaData .payload)
  where 
    step' :: Operation -> Payload -> State'
    step' Copy (Data d)             = (n, u & inventory %~ ((Disk d) :), m_)
    step' Take (Data d)             = (n & payload .~ Empty, u & inventory %~ ((Disk d) :), m_)
    step' (Put d') _          = (n & payload .~ (Data d'), u, m_)
    step' Use Exit            = (Done {..}, u, "out!")
    step' (Enter id') _       = let match = find (\x -> id' == x ^. metaData.id_) $ n ^. metaData.children in
      (fromMaybe n match, u, m_) 
      

readOp :: IO Operation
readOp = putStr "$> " >> readLn

cmdstr = "\nOperation = Lock Passcode | Unlock Passcode | Take  | Put String  | Copy  | Enter Id | Use | RootOp <UNKNOWN>\n" -- Node -- | RootOp (Node -> Node)
-- runTerm :: (State State' (Int))-- [UserItem]
runTerm :: StateT State' IO ()
runTerm = do
  (n, u, m) <- get
  lift $ putStrLn  $ "  @  " ++ print_ n ++ cmdstr ++ show u
  when (nodeType n == "Done") 
    (return ())
  op <- lift readOp
  put $ step op n u 
  runTerm

main2 = do
  print "herio"
  runStateT runTerm startState
  print "done"

runTerm' :: CyberTerm m => StateT State' m ()
runTerm' = do
  (n, u, m) <- get
  lift $ showState (n, u, m)
  when (nodeType n == "Done") 
    (return ())
  op <- lift getOp
  lift $ sendMsg $ show op
  put $ step op n u 
  runTerm'

runTerm'' :: CyberTerm m => State' -> m ()
runTerm'' (n, u, m) = do
  showState (n, u, m)
  when (nodeType n == "Done") 
    (return ())
  op <- getOp
  sendMsg $ show op
  let s' = step op n u 
  runTerm'' s'

class Monad m => CyberTerm m where
  getOp        :: m Operation
  sendMsg      :: String -> m ()
  showState    :: State' -> m ()
  
instance CyberTerm IO where
  getOp     = readOp
  sendMsg   = putStr
  showState (n, u, _) = putStrLn  $ "  @  " ++ print_ n ++ cmdstr ++ show u

data Source next = GetOp (Operation -> next) | Send String next | ShowState State' next
  deriving (Functor, Show)--deriving (Functor, Show)

instance Show1 Source where
  liftShowsPrec sp _ d (GetOp x)     = showString "GetOp"
  liftShowsPrec sp _ d (Send x _)      = showString ("Send: " ++ x)
  liftShowsPrec sp _ d (ShowState x _) = showString ("Showstate" ++ show x)


{- example data -}
startState = (tree, user, "Starting....\n")
user = Operator "User1" []
tree = UnlockedNode { _nodeMetaData = MetaData { _children = [inner], _name = "startNode", _id_ = "0", _metaDataPayload = Empty } }
inner = LockedNode {_nodeMetaData = MetaData {..}, _passCode = "bar"}
 where 
   _children = [UnlockedNode $ MetaData [] "outplace" "001" Exit]
   _name = "N"
   _id_  = "NID1"
   _metaDataPayload =  Data "foo"
