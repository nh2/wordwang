{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Object where

import           Control.Applicative ((<$>))
import           Control.Monad (mzero)
import           Data.String (IsString(..))
import           GHC.Generics (Generic)

import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Text (Text)

import           Data.Aeson (FromJSON(..), ToJSON(..), Value)
import           Data.Aeson.Types (Parser)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap

import           Common ()

-------------------------------------------------------------------------------
-- Types concerning the state of the server

type Id = Int

-- | Blocks are the units that make up a story.
--
--   @{"blockId": 3, "content": "hello"}@
data Block = Block
    { blockId :: BlockId
    , content :: BlockContent
    } deriving (Eq, Show, Generic)
type BlockId = Id
data BlockContent = StringBlock Text
    deriving (Eq, Show, Generic)

instance FromJSON Block
instance ToJSON Block
instance FromJSON BlockContent
instance ToJSON BlockContent

instance IsString BlockContent where
    fromString = StringBlock . fromString

-- | A 'Cloud' stores the candidates for the next 'Block' in the story.  The
--   @'Set' ['UserId']@ stores the upvotes.
--
--   @{"3": [{"blockId": 3, "content": "hello"}, [5]]}@
newtype Cloud = Cloud (Map BlockId (Block, Set UserId))
    deriving (Eq, Show, Generic)

instance FromJSON Cloud
instance ToJSON Cloud

-- | @{"userId": 5, "userName": "francesco"}@
data User = User
    { userId   :: UserId
    , userName :: UserName
    } deriving (Eq, Ord, Show, Generic)
type UserId = Id
type UserName = Text

instance FromJSON User
instance ToJSON User

-- | The 'Group' is the room people write stories in.
--
-- @
-- { "groupStory": []
-- , "groupCloud": {"3": [{"blockId": 3, "content": "hello"}, [5]]}
-- , "groupUsers": {"5": {"userId": 5, "userName": "francesco"}}
-- , "groupId": 7
-- }
-- @
data Group = Group
    { groupId    :: GroupId
    , groupUsers :: Map UserId User
    , groupStory :: Story
    , groupCloud :: Cloud
    } deriving (Eq, Show, Generic)
type GroupId = Id
type Story = [Block]

instance FromJSON Group
instance ToJSON Group

-------------------------------------------------------------------------------
-- Messages received/sent by the server

-- | Messages sent by the client to the server.
--
--   @{"cmd": "send", "args": "hello"}@
--   @{"cmd": "upvote", "args": 3}@
--   @{ "cmd": "join"
--    , "args": {"joinGroupId": null, "joinUserName": "francesco"}
--    }
--   @
data ClientCmd
    = Send BlockContent             -- ^ Send a new 'Block' candidate
    | Upvote BlockId                -- ^ Upvote a candidate
    | Join JoinPayload              -- ^ Join a channel if the 'GroupId' is
                                    --   provided, or create a new one.
    deriving (Eq, Show)

data JoinPayload = JoinPayload
    { joinUserName :: UserName
    , joinGroupId  :: Maybe GroupId
    } deriving (Eq, Show, Generic)

instance FromJSON JoinPayload
instance ToJSON JoinPayload

cmdJSON :: ToJSON a => Text -> a -> Value
cmdJSON name args =
    Aeson.object [("cmd", Aeson.String name), ("args", toJSON args)]

jsonCmd :: Value -> Parser (Text, Value)
jsonCmd (Aeson.Object hm) =
    case (HashMap.lookup "cmd" hm, HashMap.lookup "args" hm) of
        (Just (Aeson.String name), Just args) -> (name,) <$> parseJSON args
        _ -> mzero
jsonCmd _ = mzero

instance ToJSON ClientCmd where
    toJSON (Send bc)    = cmdJSON "send" bc
    toJSON (Upvote bid) = cmdJSON "upvote" bid
    toJSON (Join jpay)  = cmdJSON "join" jpay

instance FromJSON ClientCmd where
    parseJSON js =
        do (name, args) <- jsonCmd js
           case name of
               "send"   -> Send   <$> parseJSON args
               "upvote" -> Upvote <$> parseJSON args
               "join"   -> Join   <$> parseJSON args
               _        -> mzero

-- | Messages sent by the server to the client
--
-- @
-- { "args": { "groupStory": []
--           , "groupCloud": {"3":[{"blockId":3,"content":"hello"},[5]]}
--           , "groupUsers": {"5":{"userId":5,"userName":"francesco"}}
--           , "groupId":7
--           }
-- , "cmd": "refresh"
-- }
-- @
data ServerCmd = Refresh Group  -- ^ After any event change, the server sends
                                --   back the 'Group' to all the partecipants to
                                --   that they can update the page.
    deriving (Eq, Show, Generic)

instance ToJSON ServerCmd where
    toJSON (Refresh group) = cmdJSON "refresh" group

instance FromJSON ServerCmd where
    parseJSON js =
        do (name, args) <- jsonCmd js
           case name of
               "refresh" -> Refresh <$> parseJSON args
               _         -> mzero
