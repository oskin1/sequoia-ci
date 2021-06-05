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

makeService :: Service
makeService =
  Service
    { createContainer = createContainer_,
      startContainer = startContainer_,
      containerStatus = undefined
    }

createContainer_ :: ContainerOptions -> IO ContainerId
createContainer_ opts = do
  manager <- Socket.makeManager "/var/run/docker.sock"
  let body =
        Json.object
          [ ("Image", Json.toJSON $ unImage $ image opts),
            ("Tty", Json.toJSON True),
            ("Labels", Json.object [("quad", "")]),
            ("Cmd", "echo hello"),
            ("Entrypoint", Json.toJSON [Json.String "/bin/sh", "-c"])
          ]
  let req =
        Http.defaultRequest
          & Http.setRequestManager manager
          & Http.setRequestPath "/v1.41/containers/create"
          & Http.setRequestMethod "POST"
          & Http.setRequestBodyJSON body
  let parser = Json.withObject "create-container" \o -> fmap ContainerId (o .: "Id")
  res <- Http.httpBS req
  --traceShowIO res
  parseResponse res parser

startContainer_ :: ContainerId -> IO ()
startContainer_ container = do
  manager <- Socket.makeManager "/var/run/docker.sock"
  let path = "/v1.40/containers/" <> unContainerId container <> "/start"
  let req =
        Http.defaultRequest
          & Http.setRequestManager manager
          & Http.setRequestPath (encodeUtf8 path)
          & Http.setRequestMethod "POST"
  void $ Http.httpBS req

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

newtype ContainerOptions = ContainerOptions {image :: Image}
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
