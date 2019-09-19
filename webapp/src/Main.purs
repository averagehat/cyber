module Main where

import Prelude
import Types 
import Data.Array ((:))
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Data.String as S
import Data.String.Utils as S
import Data.Tuple as Tup
import Effect (Effect)
import Effect.Aff (nonCanceler, makeAff, runAff_, launchAff, joinFiber, forkAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Exception (throw, try)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Exception.Unsafe as Unsafe
import Record as R

error :: String -> Unit
error err = Unsafe.unsafeThrow err

main :: Effect Unit
main = do
  log "yup.\n"

impact :: Consequence -> Operator -> Operator
impact (DataLoss n) u   = u { inventory = A.drop n u.inventory  }
impact (Viral v) u      = let u' = impact v.effect u in 
                              u' { inventory = (Infection v) : u'.inventory }

type State' = BaseModel (alerts :: Int, loggedOut :: Boolean)
type Tree = Node

step :: Operation -> State' -> State'
step     (RootOp _) s         = Unsafe.unsafeThrow "Meta-ERROR: uninmplmemented"
step  op s@{tree: (Secured n) }
       | op `A.elem` n.triggers = s { user = impact n.consequence s.user, 
                                      alerts = s.alerts + 1    }
       | otherwise              = step op $ s { tree = fixed }
          where fixed = Open $ R.delete consequence_ $ R.delete triggers_ n

step  (Unlock p) s@{tree: (Locked n)}
      | p == n.passCode    = s { tree = Open $ _rmPassCode n }
      | otherwise          = s { alerts = s.alerts + 1 }

step  _  s@{tree: (Locked _)}         = s
step  (Unlock _) s                    = s
step  op s@{tree: (Open n)}  = step' op n.payload
  where 
    step' :: Operation -> Payload -> State'
    step' Copy (Data d)             = s { user { inventory = ((Disk d) : s.user.inventory) } } 
    step' Take (Data d)             = s { tree = Open $ n { payload = Empty },
                                          user { inventory = ((Disk d) : s.user.inventory) } }
    step' (Put d) _           = s { tree = Open $ n { payload = Data d } }
    step' Use Exit            = s { loggedOut = true }
    step' (Enter id') _       = s { tree = fromMaybe (Open n) match }
      where 
        matchID (Open n') =   id' == n'.id_
        matchID x_         =  Unsafe.unsafeThrow $ "Meta-ERROR: step':matchID unimplemented pattern: " <> show x_ <> id'
        match = A.find matchID n.children
    step' op pld              =  Unsafe.unsafeThrow $ "Meta-ERROR: step' unimplemented pattern: " <> show op <> show pld
    
cmdstr = "\nOperation = Lock Passcode | Unlock Passcode | Take  | Put String  | Copy  | Enter Id | Use | RootOp <UNKNOWN>\n"

runTerm :: forall m. (CyberTerm m) => State' -> m Unit
runTerm s = do
  renderModel s
  when (s.loggedOut)
     (pure unit)
  op <- getOp
  sendMsg $ show op
  let s' = step op s
  runTerm s'

class Monad m <= CyberTerm m where
  init         :: forall a. BaseModel a -> m Unit
  getOp        :: m Operation
  sendMsg      :: String -> m Unit
  renderModel  :: forall a. BaseModel a -> m Unit

type Completer = String -> Effect { completions :: Array String, 
 matched :: String }


completer' :: State' -> Completer
completer' m s = pure { completions : res, matched:  s } 
 where
   res = case (S.words s) of 
      ["Enter",y] -> A.filter (S.startsWith y) neighborIds
      _ -> completer'' s
   completer'' s = case (A.filter (S.startsWith s) opStrings) of
                   [] -> opStrings
                   x  -> x
   neighborIds :: Array String
   neighborIds = map (get_ id__) $ get_ children_ m.tree

opStrings = map (\x-> fold $ S.stripSuffix (S.Pattern "_") $ show x)  optypes 
showState s = "  @  " <> show s.tree <> cmdstr <> show s.user

str2Op :: String -> Operation
str2Op s = case (S.words s) of
    ["Enter", x] -> Enter x
    ["Lock", x] -> Lock x
    ["Unlock", x] -> Unlock x
    ["Put", x] -> Put x
    ["RootOp", x]         -> RootOp x
    ["Take"]         -> Take
    ["Copy"]         -> Copy
    ["Use"]         -> Use
    _ -> Unsafe.unsafeThrow $ "Meta-ERROR: str2Op unimplemented pattern: " <> s 

-- main_ = do
--     _ <- init
--     runTerm startState

-- instance CyberTerm Effect where
--   init        m = RL.createConsoleInterface $ completer' m
--   getOp         =  :: m Operation
--   sendMsg      :: String -> m Unit
--   renderModel  :: forall a. BaseModel a -> m Unit
-- renderSuccessfulOp :: State' -> Operation -> State' -> m ()
-- renderFailure      :: State' -> Failure -> m ()
-- renderConsequence  :: State' -> Consequence -> State' -> m ()

