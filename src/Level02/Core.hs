{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp, app) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody, rawPathInfo)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text, pack)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)

import           Level02.Types            (ContentType(..), Error(..), RqType(..),
                                           mkCommentText, mkTopic,
                                           renderContentType)

-- |-------------------------------------------|
-- |- Don't start here, go to Level02.Types!  -|
-- |-------------------------------------------|

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse status content body =
  responseLBS status [("Content-Type", ct)] body
  where 
    ct = renderContentType content

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 =
  mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 =
  mkResponse status404

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 =
  mkResponse status400

-- |----------------------------------------------------------------------------------
-- These next few functions will take raw request information and construct         --
-- one of our types.                                                                --
--                                                                                  --
-- By breaking out these smaller functions, we're able to isolate our               --
-- validation requirements into smaller components that are simpler to maintain     --
-- and verify. It also allows for greater reuse and it also means that              --
-- validation is not duplicated across the application, maybe incorrectly.          --
--------------------------------------------------------------------------------------

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest txt bs = do
  -- lift2 (\t c -> AddReq t c) topic comment
  t <- topic
  c <- comment
  Right $ AddRq t c
  where
    topic = mkTopic txt
    comment = mkCommentText $ lazyByteStringToStrictText bs
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest txt = do
  topic <- mkTopic txt
  Right $ ViewRq topic

mkListRequest
  :: Either Error RqType
mkListRequest =
  Right $ ListRq

-- |----------------------------------
-- end of RqType creation functions --
--------------------------------------

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse TopicEmpty = resp400 PlainText "Empty Topic Text"
mkErrorResponse CommentTextEmpty = resp400 PlainText "Empty Comment Text"
mkErrorResponse (UnknownLink txt) = resp404 PlainText (LBS.fromStrict $ encodeUtf8 $ "Unknown link: " <> txt)

-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest request =
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.
  case (requestMethod request, pathInfo request) of
    -- POST /<topic>/add
    ("POST", [topic, "add"]) -> 
      (strictRequestBody request) >>= (\body -> pure $ mkAddRequest topic body)
    -- GET /<topic>/view
    ("GET", [topic, "view"]) -> pure $ mkViewRequest topic
    -- GET /list
    ("GET", ["list"]) -> pure $ mkListRequest
    -- otherwise
    _ -> pure $ Left $ UnknownLink $ pathToText $ pathInfo request  -- rawPathInfo request

pathToText :: [Text] -> Text
pathToText (x:xs) = x <> "; " <> pathToText xs
pathToText _ = ""

-- | If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest (AddRq _ _) = Right $ resp200 PlainText "AddRq not implemented yet"
handleRequest (ViewRq _) = Right $ resp200 PlainText "ViewRq not implemented yet"
handleRequest (ListRq) = Right $ resp200 PlainText "ListRq not implemented yet"

-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
app
  :: Application
app req respond = do
  eith <- mkRequest req
  either 
    (\err -> respond $ mkErrorResponse err)
    (\res -> respond res)
    (eith >>= (\reqtype -> handleRequest reqtype))

runApp :: IO ()
runApp = run 3000 app
