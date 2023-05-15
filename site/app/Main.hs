{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Colonnade
import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)
import Data.String (IsString(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Lucid
import Options.Generic
import qualified Siphon
import qualified Siphon as SI
import qualified Streaming as SR
import qualified Streaming.ByteString as BS
import qualified Streaming.Prelude as SR
import qualified System.IO as IO
import System.Process
import qualified Data.Map.Strict as Map
import Data.Char (isAlphaNum)

data Tweet = Tweet
  { tweet_Id :: TweetId,
    tweet_URL :: Text,
    tweet_Posted_Time :: Text,
    tweet_Content :: Text,
    tweet_Type :: TweetType,
    client :: Text,
    retweets_received :: Text,
    likes_received :: Text,
    user_Id :: Text,
    name :: Text,
    username :: Text,
    verified_or_NonVerified :: Text,
    profile_URL :: Text,
    protected_or_Not_Protected :: Text
  }
  deriving (Show, Eq, Generic, NFData)

data Media = Media
  { media_TweetId :: TweetId
  , media_MediaId :: Text
  , media_MediaURL :: Text
  , media_TweetURL :: Text
  , media_MediaType :: MediaType
  }
  deriving (Show, Eq, Generic, NFData)

data MediaType = MediaTypeImage | MediaTypeVideo
  deriving (Show, Eq, Generic, NFData)

data TweetType = TweetTypeTweet | TweetTypeRetweet | TweetTypeReply
  deriving (Show, Eq, Generic, NFData)

decodeTweetType :: (IsString s, Show s, Eq s) => s -> Maybe TweetType
decodeTweetType t = Just $ case t of
  "Tweet" -> TweetTypeTweet
  "Retweet" -> TweetTypeRetweet
  "Reply" -> TweetTypeReply
  x -> error $ show x

decodeFileName :: Text -> Maybe Text
decodeFileName url = case T.split (=='/') url of
  [] -> Nothing
  xs -> Just $ T.takeWhile (\x -> isAlphaNum x || x == '.' || x == '_' || x == '-') $ last xs

type TweetId = Int

decodeTweetId :: BC8.ByteString -> Maybe Int
decodeTweetId = Just . read . BC8.unpack . BC8.drop 3 . BC8.dropEnd 3

tweetsS :: SI.Siphon Headed BC8.ByteString Tweet
tweetsS =
  Tweet
    <$> Siphon.headed "Tweet Id" decodeTweetId
    <*> Siphon.headed "Tweet URL" decode
    <*> Siphon.headed "Tweet Posted Time" decode
    <*> Siphon.headed "Tweet Content" decodeContent
    <*> Siphon.headed "Tweet Type" decodeTweetType
    <*> Siphon.headed "Client" decode
    <*> Siphon.headed "Retweets received" decode
    <*> Siphon.headed "Likes received" decode
    <*> Siphon.headed "User Id" decode
    <*> Siphon.headed "Name" decodeContent
    <*> Siphon.headed "Username" decode
    <*> Siphon.headed "Verified or Non-Verified" decode
    <*> Siphon.headed "Profile URL" decode
    <*> Siphon.headed "Protected or Not Protected" decode
  where
    decode = Just . T.decodeUtf8
    decodeContent = Just . T.drop 1 . T.dropEnd 1 . T.decodeUtf8

mediaS :: MediaType -> SI.Siphon Headed BC8.ByteString Media
mediaS mt =
  Media
    <$> Siphon.headed "Tweet Id" decodeTweetId
    <*> Siphon.headed "Media Id" decode
    <*> Siphon.headed "Media URL" decode
    <*> Siphon.headed "Tweet URL" decode
    <*> pure mt
  where
    decode = Just . T.decodeUtf8

handleError :: SR.Of a (Maybe SI.SiphonError) -> IO a
handleError (a SR.:> m) = maybe (pure a) (fail . SI.humanizeSiphonError) m

mkBlock ::
  NFData a =>
  FilePath ->
  SI.Siphon Headed BC8.ByteString a ->
  IO (SR.Of [a] (Maybe SI.SiphonError))
mkBlock fp s =
  IO.withFile
    fp
    IO.ReadMode
    ( \handle -> do
        xs <- SR.toList $ SR.mapM (\a -> evaluate (rnf a) >> pure a) $ SI.decodeCsvUtf8 s (BS.toChunks $ BS.fromHandle handle)
        pure xs
    )

renderTweets :: Map.Map TweetId Media -> [Tweet] -> Html ()
renderTweets mediaMap tweets =
  div_ [class_ "tweets"] $ mconcat $ fmap renderTweet tweets
  where
    renderTweet :: Tweet -> Html ()
    renderTweet tweet =
      div_ [class_ "tweet"] $ do
        case tweet_Type tweet of
          TweetTypeRetweet -> sub_ $ do
              "mullet samurai Retweeted"
          _ -> pure ()
        p_ $ do
          span_ [class_ "displayname"] $ toHtml $ name tweet
          span_ [class_ "username"] $ do
            "@"
            toHtml $ username tweet
        toHtml $ tweet_Content tweet
        case Map.lookup (tweet_Id tweet) mediaMap of
          Nothing -> pure ()
          Just media -> case decodeFileName $ media_MediaURL media of
            Nothing -> img_ [src_ $ media_MediaURL media]
            Just localFile -> case media_MediaType media of
              MediaTypeImage -> img_   [ class_ "local", src_ $ "static/media/" <> localFile ]
              MediaTypeVideo -> video_ [ class_ "local", controls_ "", src_ $ "static/media/" <> localFile ] (pure ())

main :: IO ()
main = do
  -- system "scss public/static/main.scss public/static/main.css"

  tweets <- mkBlock "../tweets.csv" tweetsS >>= handleError

  images <- mkBlock "../images.csv" (mediaS MediaTypeImage) >>= handleError
  videos <- mkBlock "../videos.csv" (mediaS MediaTypeVideo) >>= handleError
  let mediaMap = Map.fromList $
             fmap (\x -> (media_TweetId x, x)) images
          <> fmap (\x -> (media_TweetId x, x)) videos

  renderToFile "public/index.html" $ html_ $ do
    head_ $ do
      link_ [rel_ "stylesheet", href_ "static/main.css"]
    body_ $ do
      main_ $ do
        header_ $ do
          img_ [class_ "banner", src_ "static/banner.jpg"]
          figure_ [class_ "image is-128x128 avatar"] $ do
            img_ [class_ "is-rounded", src_ "static/avatar.jpg"]
          span_ [class_ "displayname"] "mullet samurai"
          span_ [class_ "username"] "@eatskulls"
          p_ "almighty satan! destroy those who love god"
          p_ "📍 Atlanta, GA 📅 Joined January 2017"
          span_ "462 Following"
          " "
          span_ "66 Followers"

        renderTweets mediaMap tweets

