{-# LANGUAGE TemplateHaskell, OverloadedStrings, GADTs, ScopedTypeVariables, DeriveDataTypeable,
             RecordWildCards, TypeFamilies, OverloadedStrings#-}

module Web.Common where


import Prelude hiding (div, catch)
import Text.Blaze.Html5 hiding (map, output, base)
import Text.Blaze.Html5.Attributes hiding (dir, id)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Routes.PathInfo
import Web.Routes.RouteT
import Web.Routes.TH (derivePathInfo)
import Control.Monad.State
import Control.Concurrent.STM
import Language.Nomyx.Expression
import Happstack.Server
import Types as T
import qualified Data.ByteString.Char8 as C
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Reform.Happstack()
import Text.Reform
import Text.Reform.Blaze.String()
import Text.Reform.Happstack()
import Text.Blaze.Internal
import qualified Text.Reform.Generalized as G
import Data.Text(Text, pack)
import Web.Routes.Happstack()
import Data.Time
--import Serialize hiding (update')
import Control.Exception           (bracket)
import Data.Acid.Local             (createCheckpointAndClose, openLocalStateFrom)
import Data.Maybe                  (fromMaybe)
import Happstack.Auth.Core.Auth    (AuthState       , initialAuthState)
import Happstack.Auth.Core.Profile (ProfileState    , initialProfileState)
--import ProfileData                 (ProfileDataState, initialProfileDataState)
import System.FilePath             ((</>))
import Happstack.Auth        (AuthState, ProfileState, UserId, getUserId)
import Data.Generics         (Data, Typeable)
import Data.SafeCopy         (base, deriveSafeCopy)
import           Data.IxSet  ((@=), getOne, inferIxSet, noCalcs)
import qualified Data.IxSet  as IxSet
import Control.Monad.Reader  (ask)
import Data.Acid             (AcidState, Update, Query, makeAcidic)
import Happstack.Auth (AuthProfileURL)
import Data.Acid.Advanced    (update', query')
import Data.Text             (Text)
import qualified Data.Text   as Text
import Control.Monad.Reader  (ask)
import Control.Monad.State   (get, put)
import Data.Acid             (AcidState, Update, Query, makeAcidic)
import Data.Acid.Advanced    (update', query')
import Data.Generics         (Data, Typeable)
import           Data.IxSet  ((@=), getOne, inferIxSet, noCalcs)
import qualified Data.IxSet  as IxSet
import Data.SafeCopy         (base, deriveSafeCopy)
import Data.Text             (Text)
import qualified Data.Text   as Text
import Happstack.Auth        (AuthState, ProfileState, UserId, getUserId)
import Happstack.Server      (Happstack, Response, internalServerError, ok, seeOther, toResponse)
--import Web.Routes.TH         (derivePathInfo)
default (Integer, Double, Data.Text.Text)



        -- | 'ProfileData' contains application specific
data ProfileData =
    ProfileData { dataFor    :: UserId -- ^ UserId associated with this profile data
                , profileMsg :: Text   -- ^ Some data to store in the profile
                }
    deriving (Eq, Ord, Read, Show, Typeable, Data)
$(deriveSafeCopy 1 'base ''ProfileData)

$(inferIxSet "ProfilesData" ''ProfileData 'noCalcs [''UserId, ''Text])

data ProfileDataState =
    ProfileDataState { profilesData :: ProfilesData }
    deriving (Eq, Ord, Read, Show, Typeable, Data)
$(deriveSafeCopy 1 'base ''ProfileDataState)

data ProfileDataURL
    = CreateNewProfileData
    | ViewProfileData UserId
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePathInfo ''ProfileDataURL)

-- | 'Acid' holds all the 'AcidState' handles for this site.
data Acid = Acid
    { acidAuth        :: AcidState AuthState
    , acidProfile     :: AcidState ProfileState
    , acidProfileData :: AcidState ProfileDataState
    }

data LoginPass = LoginPass { login :: PlayerName,
                             password :: PlayerPassword}
                             deriving (Show, Eq)


-- | associate a player number with a handle
data PlayerClient = PlayerClient PlayerNumber deriving (Eq, Show)

-- | A structure to hold the active games and players
data Server = Server [PlayerClient] deriving (Eq, Show)


data PlayerCommand =
                     U_HomePage
                   | U_AuthProfile AuthProfileURL
                   | U_ProfileData ProfileDataURL
                   | Login
                   | PostLogin
                   | NewPlayer       LoginPass
                   | NewPlayerLogin  LoginPass
                   | Noop            PlayerNumber
                   | ViewGame        PlayerNumber GameName
                   | JoinGame        PlayerNumber GameName
                   | LeaveGame       PlayerNumber GameName
                   | DoInputChoice   PlayerNumber EventNumber
                   | DoInputString   PlayerNumber String
                   | NewRule         PlayerNumber
                   | NewGame         PlayerNumber
                   | SubmitNewGame   PlayerNumber
                   | Upload          PlayerNumber
                   | PlayerSettings  PlayerNumber
                   | SubmitPlayerSettings  PlayerNumber
                   deriving (Show)

$(derivePathInfo ''PlayerCommand)
$(derivePathInfo ''LoginPass)

instance PathInfo Bool where
  toPathSegments i = [pack $ show i]
  fromPathSegments = pToken (const "bool") (checkBool . show)
   where checkBool str =
           case reads str of
             [(n,[])] -> Just n
             _ ->        Nothing

modDir :: FilePath
modDir = "modules"

type NomyxServer       = ServerPartT IO
type RoutedNomyxServer = RouteT PlayerCommand NomyxServer


evalCommand :: (TVar Session) -> StateT Multi IO a -> RoutedNomyxServer a
evalCommand tm sm = liftRouteT $ lift $ do
    (T.Session _ m) <- atomically $ readTVar tm
    evalStateT sm m


webCommand :: (TVar Session) -> PlayerNumber -> StateT Multi IO () -> RoutedNomyxServer ()
webCommand tm _ sm = liftRouteT $ lift $ do
   (T.Session sh m) <- atomically $ readTVar tm
   m' <- execStateT sm m
   atomically $ writeTVar tm (T.Session sh m')


blazeResponse :: Html -> Response
blazeResponse html = toResponseBS (C.pack "text/html;charset=UTF-8") $ renderHtml html

blazeForm :: Html -> Text -> Html
blazeForm html link =
    H.form ! A.action (toValue link)
         ! A.method "POST"
         ! A.enctype "multipart/form-data" $
            do html
               input ! A.type_ "submit" ! A.value "Submit"

-- | Create a group of radio elements without BR between elements
inputRadio' :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, ToMarkup lbl) =>
              [(a, lbl)]  -- ^ value, label, initially checked
           -> (a -> Bool) -- ^ isDefault
           -> Form m input error Html () a
inputRadio' choices isDefault =
    G.inputChoice isDefault choices mkRadios
    where
      mkRadios nm choices' = mconcat $ concatMap (mkRadio nm) choices'
      mkRadio nm (i, val, lbl, checked) =
          [ ((if checked then (! A.checked "checked") else id) $
             input ! A.type_ "radio" ! A.id (toValue i) ! A.name (toValue nm) ! A.value (toValue val))
          , " ", H.label ! A.for (toValue i) $ toHtml lbl]

mainPage :: Html -> Html -> Html -> Bool -> RoutedNomyxServer Html
mainPage body title header footer = do
   ok $ H.html $ do
      H.head $ do
        H.title title
        H.link ! rel "stylesheet" ! type_ "text/css" ! href "/static/css/nomyx.css"
        H.meta ! A.httpEquiv "Content-Type" ! content "text/html;charset=utf-8"
        H.meta ! A.name "keywords" ! A.content "Nomyx, game, rules, Haskell, auto-reference"
        H.script ! A.type_ "text/JavaScript" ! A.src "/static/nomyx.js" $ ""
      H.body $ do
        H.div ! A.id "container" $ do
           H.div ! A.id "header" $ header
           body
           when footer $ H.div ! A.id "footer" $ "Copyright Corentin Dupont 2012-2013"

withAcid :: Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe "_state" mBasePath in
    bracket (openLocalStateFrom (basePath </> "auth")        initialAuthState)        (createCheckpointAndClose) $ \auth ->
    bracket (openLocalStateFrom (basePath </> "profile")     initialProfileState)     (createCheckpointAndClose) $ \profile ->
    bracket (openLocalStateFrom (basePath </> "profileData") initialProfileDataState) (createCheckpointAndClose) $ \profileData ->
        f (Acid auth profile profileData)

initialProfileDataState :: ProfileDataState
initialProfileDataState = ProfileDataState { profilesData = IxSet.empty }


-- | set 'ProfileData' for UserId
setProfileData :: UserId -> Text -> Update ProfileDataState ProfileData
setProfileData uid msg =
    do pds@(ProfileDataState {..}) <- get
       let profileData = ProfileData uid msg
       put $ pds { profilesData = IxSet.updateIx uid profileData profilesData }
       return profileData

-- | get 'ProfileData' associated with 'UserId'
askProfileData :: UserId -> Query ProfileDataState (Maybe ProfileData)
askProfileData uid =
    do ProfileDataState{..} <- ask
       return $ getOne $ profilesData @= uid

-- | create the profile data, but only if it is missing
newProfileData :: UserId -> Text -> Update ProfileDataState ProfileData
newProfileData uid msg =
    do pds@(ProfileDataState {..}) <- get
       case IxSet.getOne (profilesData @= uid) of
         Nothing -> do let profileData = ProfileData uid msg
                       put $ pds { profilesData = IxSet.updateIx uid profileData profilesData }
                       return profileData
         (Just profileData) -> return profileData


$(makeAcidic ''ProfileDataState
                [ 'setProfileData
                , 'askProfileData
                , 'newProfileData
                ]
 )



--handleProfileData :: (Happstack m)
--                  => AcidState AuthState
--                  -> AcidState ProfileState
--                  -> AcidState ProfileDataState
--                  -> ProfileDataURL
--                  -> m Response
--handleProfileData authStateH profileStateH profileDataStateH url =
--    case url of
--      CreateNewProfileData ->
--          do mUserId <- getUserId authStateH profileStateH
--             case mUserId of
--               Nothing -> internalServerError $ toResponse $ string "not logged in."
--               (Just userId) ->
--                   do update' profileDataStateH (NewProfileData userId (Text.pack "this is the default message."))
--                      seeOther (string "/") (toResponse $ string "/")
--      (ViewProfileData uid) ->
--          do mProfileData <- query' profileDataStateH (AskProfileData uid)
--             ok $ toResponse $ show mProfileData
