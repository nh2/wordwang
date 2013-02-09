{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Objects where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (mzero)
import           Data.Maybe (fromJust)
import           Data.String (IsString(..))
import           GHC.Generics (Generic)

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)

import           Data.Aeson (FromJSON(..), ToJSON(..), Value)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import           Data.HashSet (HashSet)
import           Data.Hashable (Hashable)

import           Common ()

-------------------------------------------------------------------------------
-- Types concerning the state of the server

type Id = Int

-- | Blocks are the units that make up a story.
--
--   @{"blockId": 3, {"blockType": "string", "content": "hello"}}@
--   @{"blockId": 3, {"blockType": "close"}}@
data Block = Block
    { blockId :: BlockId
    , content :: BlockContent
    } deriving (Eq, Show, Generic)
type BlockId = Id
data BlockContent
    = StringBlock Text
    | CloseBlock
    deriving (Eq, Show, Generic)

instance Hashable BlockContent
instance Hashable Block

instance FromJSON Block where
instance ToJSON Block

instance ToJSON BlockContent where
    toJSON (StringBlock s) = toJSON2 "blockType" ("string" :: Text) "blockContent" s
    toJSON CloseBlock      = Aeson.object [("blockType", Aeson.String "close")]

instance FromJSON BlockContent where
    parseJSON (Aeson.Object js) =
        case (HashMap.lookup "blockType" js, HashMap.lookup "blockContent" js) of
            (Just "string", Just s) -> StringBlock <$> parseJSON s
            (Just "close", Nothing) -> return CloseBlock
            _                       -> mzero
    parseJSON _ = mzero

instance IsString BlockContent where
    fromString = StringBlock . fromString

-- | A 'Cloud' stores the candidates for the next 'Block' in the story.  The
--   @'Set' ['UserId']@ stores the upvotes.
--
--   @{"3": [{"blockId": 3, "content": "hello"}, [5]]}@

data Cloud = Cloud (Map BlockId CloudItem)
                   (HashSet Block)
    deriving (Eq, Show, Generic)

data CloudItem
    = CloudItem { cloudBlock :: Block
                , cloudUids :: Set UserId
                }
      deriving (Eq, Show, Generic)

instance FromJSON CloudItem
instance ToJSON CloudItem

newCloud :: Cloud
newCloud = Cloud Map.empty HashSet.empty

insertBlock :: Block -> UserId -> Cloud -> Cloud
insertBlock b@(Block bid _) uid cloud@(Cloud votes hs) =
    if HashSet.member b hs
    then fromJust (upvoteBlock bid uid cloud)
    else case Map.lookup bid votes of
             Nothing -> Cloud (Map.insert bid (CloudItem b $ Set.fromList [uid]) votes)
                              (HashSet.insert b hs)
             Just _ -> error "insertBlock: the impossible happened!"

upvoteBlock :: BlockId -> UserId -> Cloud -> Maybe Cloud
upvoteBlock bid uid (Cloud votes hs) =
    case Map.lookup bid votes of
        Nothing -> Nothing
        Just (CloudItem b voters) ->
            Just (Cloud (Map.insert bid (CloudItem b $ Set.insert uid voters) votes) hs)

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
--   @{"cmd": "send", "args": {"blockType": "string", "blockContent": "hello"}}@
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
        do (name, args)  <- jsonCmd js
           case name of
               "refresh" -> Refresh <$> parseJSON args
               _         -> mzero

-- Utils

toJSON2 :: (ToJSON a, ToJSON b) => Text -> a -> Text -> b -> Value
toJSON2 ix1 v1 ix2 v2 = Aeson.object [(ix1, toJSON v1), (ix2, toJSON v2)]

fromJSON2 :: (FromJSON a, FromJSON b) => Text -> Text -> Value -> Parser (a, b)
fromJSON2 ix1 ix2 (Aeson.Object hm) =
    case (HashMap.lookup ix1 hm, HashMap.lookup ix2 hm) of
        (Just v1, Just v2) -> (,) <$> parseJSON v1 <*> parseJSON v2
        _ -> mzero
fromJSON2 _ _ _ = mzero

cmdJSON :: ToJSON a => Text -> a -> Value
cmdJSON name args = toJSON2 "cmd" name "args" args

jsonCmd :: Value -> Parser (Text, Value)
jsonCmd = fromJSON2 "cmd" "args"
