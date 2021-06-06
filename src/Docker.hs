module Docker where

import Data.Aeson ((.:))
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json.Types
import qualified Network.HTTP.Simple as Http
import RIO
import qualified Socket

data Service = Service
  { createContainer :: ContainerOptions -> IO ContainerId,
    startContainer :: ContainerId -> IO (),
    containerStatus :: ContainerId -> IO ContainerStatus
  }

makeService :: IO Service
makeService =
  Socket.makeManager "/var/run/docker.sock" <&> \manager ->
    let makeReq path =
          Http.defaultRequest
            & Http.setRequestPath (encodeUtf8 $ dockerApiVersion <> path)
            & Http.setRequestManager manager
     in Service
          { createContainer = createContainer_ makeReq,
            startContainer = startContainer_ makeReq,
            containerStatus = containerStatus_ makeReq
          }

dockerApiVersion = "/v1.41"

createContainer_ :: MakeRequest -> ContainerOptions -> IO ContainerId
createContainer_ makeReq opts = do
  manager <- Socket.makeManager "/var/run/docker.sock"
  let body =
        Json.object
          [ ("Image", Json.toJSON $ unImage $ image opts),
            ("Tty", Json.toJSON True),
            ("Labels", Json.object [("quad", "")]),
            ("Entrypoint", Json.toJSON [Json.String "/bin/sh", "-c"]),
            ("Cmd", "echo \"$QUAD_SCRIPT\" | /bin/sh"),
            ("Env", Json.toJSON ["QUAD_SCRIPT=" <> script opts])
          ]
  let req =
        makeReq "/containers/create"
          & Http.setRequestMethod "POST"
          & Http.setRequestBodyJSON body
  let parser = Json.withObject "create-container" \o -> fmap ContainerId (o .: "Id")
  res <- Http.httpBS req
  --traceShowIO res
  parseResponse res parser

startContainer_ :: MakeRequest -> ContainerId -> IO ()
startContainer_ makeReq container =
  void $ Http.httpBS req
  where
    path = "/containers/" <> unContainerId container <> "/start"
    req = makeReq path & Http.setRequestMethod "POST"

containerStatus_ :: MakeRequest -> ContainerId -> IO ContainerStatus
containerStatus_ makeReq container = do
  let parser = Json.withObject "container-inspect" $ \o -> do
        state <- o .: "State"
        status <- state .: "Status"
        case status of
          "running" -> pure ContainerRunning
          "exited" -> do
            code <- state .: "ExitCode"
            pure $ ContainerExited (ContainerExitCode code)
          other -> pure $ ContainerOther other
  let req = makeReq $ "/containers/" <> unContainerId container <> "/json"
  res <- Http.httpBS req
  parseResponse res parser

parseResponse ::
  Http.Response ByteString ->
  (Json.Value -> Json.Types.Parser a) ->
  IO a
parseResponse resp parser =
  case result of
    Left e -> throwString e
    Right res -> pure res
  where
    result = do
      value <- Json.eitherDecodeStrict (Http.getResponseBody resp)
      Json.Types.parseEither parser value

type MakeRequest = Text -> Http.Request

data ContainerOptions = ContainerOptions
  { image :: Image,
    script :: Text
  }
  deriving (Eq, Show)

newtype Image = Image {unImage :: Text}
  deriving (Eq, Show)

newtype ContainerId = ContainerId {unContainerId :: Text}
  deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode {unExitCode :: Int}
  deriving (Eq, Show)

data ContainerStatus
  = ContainerRunning
  | ContainerExited ContainerExitCode
  | ContainerOther Text
  deriving (Eq, Show)
