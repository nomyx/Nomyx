{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | All the building blocks to allow rules to build variables.
-- for example, you can create a variable with:
--do
--   newMsgVar_ "MyMoney" (0::Int)

module Language.Nomyx.Variables (
   V(..),
   VEvent(..),
   MsgVar(..),
   newVar,    newVar_,    newVar',    readVar,    readVar_,    writeVar,    modifyVar,    delVar,
   newMsgVar, newMsgVar_, newMsgVar', readMsgVar, readMsgVar_, writeMsgVar, modifyMsgVar, delMsgVar,
   msgVar,
   newMsgVarOnEvent,
   onMsgVarEvent, onMsgVarChange, onMsgVarDelete,
   getMsgVarMessage,
   getMsgVarData, getMsgVarData_,
   getMsgVarName,
   ArrayVar,
   newArrayVar, newArrayVar_, newArrayVar', newArrayVarOnce,
   cleanOnFull,
   isFullArrayVar_,   
   putArrayVar, putArrayVar_
   ) where
   

import Language.Nomyx.Expression
import Language.Nomyx.Events
import Data.Typeable
import Control.Monad.State
import Control.Applicative
import Data.Maybe
import qualified Data.Map as M
import Data.Map hiding (map, filter, insert, mapMaybe, null)
import Data.Foldable as F (mapM_)

-- * Variables
-- | variable creation
newVar :: (Typeable a, Show a) => VarName -> a -> Nomex (Maybe (V a))
newVar = NewVar

newVar_ :: (Typeable a, Show a) => VarName -> a -> Nomex (V a)
newVar_ s a = partial "newVar_: Variable existing" (newVar s a)

newVar' :: (Typeable a, Show a) => V a -> a -> Nomex Bool
newVar' v a = maybe False (const True) <$> (newVar (varName v) a)

-- | variable reading
readVar :: (Typeable a, Show a) => V a -> NomexNE (Maybe a)
readVar = ReadVar 

readVar_ :: (Typeable a, Show a) => V a -> Nomex a
readVar_ v@(V a) = partial ("readVar_: Variable \"" ++ a ++ "\" with type \"" ++ (show $ typeOf v) ++ "\" not existing") (liftEffect $ readVar v)

-- | variable writing
writeVar :: (Typeable a, Show a) => V a -> a -> Nomex Bool
writeVar = WriteVar

-- | modify a variable using the provided function
modifyVar :: (Typeable a, Show a) => V a -> (a -> a) -> Nomex Bool
modifyVar v f = writeVar v . f =<< readVar_ v

-- | delete variable
delVar :: V a -> Nomex Bool
delVar = DelVar

-- * Message Variable
-- | a MsgVar is a variable with a message attached, allowing to trigger registered functions anytime the var if modified
data VEvent a = VUpdated a | VDeleted deriving (Typeable, Show, Eq)
data MsgVar a = MsgVar {message :: Msg (VEvent a), variable :: V a }

msgVar :: String -> MsgVar a
msgVar a = MsgVar (Msg a) (V a)

newMsgVar :: (Typeable a, Show a) => VarName -> a -> Nomex (Maybe (MsgVar a))
newMsgVar name a = do
    mv <- newVar name a
    return $ mv >>= Just . MsgVar (Msg name)

newMsgVar_ :: (Typeable a, Show a) => VarName -> a -> Nomex (MsgVar a)
newMsgVar_ name a = partial "newMsgVar_: Variable existing" (newMsgVar name a)

newMsgVar' :: (Typeable a, Show a) => MsgVar a -> a -> Nomex Bool
newMsgVar' v a = maybe False (const True) <$> (newMsgVar (getMsgVarName v) a)

-- | create a new MsgVar and register callback in case of change (update, delete)
newMsgVarOnEvent :: (Typeable a, Show a, Eq a) => VarName -> a -> (VEvent a -> Nomex ()) -> Nomex (Maybe (MsgVar a))
newMsgVarOnEvent name a f = do
    mv <- newMsgVar name a
    case mv of
       Just (MsgVar m _) -> do
          onMessage m f
          return mv
       Nothing -> return Nothing

writeMsgVar :: (Typeable a, Show a) => MsgVar a -> a -> Nomex Bool
writeMsgVar (MsgVar m v) a = do
   r <- writeVar v a
   sendMessage m (VUpdated a)
   return r

readMsgVar :: (Typeable a, Show a) => MsgVar a -> NomexNE (Maybe a)
readMsgVar (MsgVar _ v) = readVar v

readMsgVar_ :: (Typeable a, Show a) => MsgVar a -> Nomex a
readMsgVar_ mv = partial "readMsgVar_: variable not existing" (liftEffect $ readMsgVar mv)

modifyMsgVar :: (Typeable a, Show a) => MsgVar a -> (a -> a) -> Nomex Bool
modifyMsgVar mv f = readMsgVar_ mv >>= writeMsgVar mv . f 

delMsgVar :: (Typeable a, Show a) => MsgVar a -> Nomex Bool
delMsgVar (MsgVar m v) = do
   sendMessage m VDeleted
   --delAllEvents (messageEvent m)
   delVar v

onMsgVarEvent :: (Typeable a, Show a) => MsgVar a -> (VEvent a -> Nomex ()) -> Nomex EventNumber
onMsgVarEvent mv f = do
   m <- liftEffect $ getMsgVarMessage mv
   onMessage m f

-- | adds a callback for each of the MsgVar events: Create, Update, Delete
onMsgVarChange :: (Typeable a, Show a, Eq a)
               => MsgVar a             -- ^ the MsgVar
               -> (a -> Nomex b)       -- ^ callback on creation (called immediatly)
               -> (a -> b -> Nomex ()) -- ^ callback on update
               -> (b -> Nomex ())      -- ^ callback on delete
               -> Nomex EventNumber    -- ^ event number generated for update and delete
onMsgVarChange mv create update delete = do
   val <- readMsgVar_ mv
   c <- create val
   onMsgVarEvent mv $ f c where
      f c' (VUpdated v) = update v c'
      f c' VDeleted     = delete c'

onMsgVarDelete :: (Typeable a, Show a) => MsgVar a -> Nomex () -> Nomex EventNumber
onMsgVarDelete mv f = onMsgVarEvent mv onMsgVarDel where
    onMsgVarDel VDeleted     = f
    onMsgVarDel _ = return ()

-- | get the messsage triggered when the array is filled
getMsgVarMessage :: (Typeable a, Show a) => MsgVar a -> NomexNE (Msg (VEvent a))
getMsgVarMessage (MsgVar m _) = return m

-- | get the association array
getMsgVarData :: (Typeable a, Show a) => MsgVar a -> NomexNE (Maybe a)
getMsgVarData (MsgVar _ v) = readVar v

getMsgVarData_ :: (Typeable a, Show a) => MsgVar a -> Nomex a
getMsgVarData_ (MsgVar _ v) = readVar_ v

getMsgVarName :: (Typeable a, Show a) => MsgVar a -> String
getMsgVarName (MsgVar _ (V varName)) = varName

    
-- * Variable arrays
-- | ArrayVar is an indexed array with a signal attached triggered at every change.
-- | each indexed elements starts empty (value=Nothing).
type ArrayVar i a = MsgVar [(i, Maybe a)]

-- | initialize an empty ArrayVar
newArrayVar :: (Typeable a, Show a, Typeable i, Show i) => VarName -> [i] -> Nomex (Maybe (ArrayVar i a))
newArrayVar name l = do
    let list = map (\i -> (i, Nothing)) l
    newMsgVar name list

newArrayVar_ :: (Typeable a, Show a, Typeable i, Show i) => VarName -> [i] -> Nomex (ArrayVar i a)
newArrayVar_ name l = partial "newArrayVar_: Variable existing" (newArrayVar name l)

-- | initialize an empty ArrayVar, registering a callback that will be triggered at every change
newArrayVar' :: (Typeable a, Show a, Eq a, Typeable i, Show i, Eq i) => VarName -> [i] -> (VEvent [(i,Maybe a)] -> Nomex ()) -> Nomex (Maybe (ArrayVar i a))
newArrayVar' name l f = do
    let list = map (\i -> (i, Nothing)) l
    newMsgVarOnEvent name list f

-- | initialize an empty ArrayVar, registering a callback.
-- the ArrayVar will be deleted when full
newArrayVarOnce :: (Typeable a, Show a, Eq a, Typeable i, Show i, Ord i) => VarName -> [i] -> (VEvent [(i, Maybe a)] -> Nomex ()) -> Nomex (Maybe (ArrayVar i a))
newArrayVarOnce name l f = do
   mv <- newArrayVar' name l f
   F.mapM_ cleanOnFull mv
   return mv

cleanOnFull :: (Typeable a, Show a, Eq a, Typeable i, Show i, Ord i) => ArrayVar i a -> Nomex ()
cleanOnFull ar = do
   m <- liftEffect $ getMsgVarMessage ar 
   onMessage m $ \_ -> do
      full <- liftEffect $ isFullArrayVar_ ar
      when full $ void $ delMsgVar ar
      return ()
   return ()

isFullArrayVar_ :: (Typeable a, Show a, Typeable i, Show i, Ord i) => ArrayVar i a -> NomexNE Bool
isFullArrayVar_ av = do
   md <- getMsgVarData av
   return $ all isJust (map snd $ fromJust md)       
   
-- | store one value and the given index. If this is the last filled element, the registered callbacks are triggered.
putArrayVar :: (Typeable a, Show a, Eq a, Typeable i, Show i, Eq i, Ord i) => ArrayVar i a -> i -> a -> Nomex Bool
putArrayVar mv i a = do
    ma <- liftEffect $ readMsgVar mv
    case ma of
       Just ar -> do
          let ar2 = M.insert i (Just a) (fromList ar)
          writeMsgVar mv (toList ar2)
       Nothing -> return False

putArrayVar_ :: (Typeable a, Show a, Eq a, Typeable i, Show i, Ord i) => ArrayVar i a  -> i -> a -> Nomex ()
putArrayVar_ mv i a = void $ putArrayVar mv i a       

