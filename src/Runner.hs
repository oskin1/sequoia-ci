module Runner where

import Core
import qualified Docker
import RIO

data Service = Service
  { runBuild :: Build -> IO Build,
    prepareBuild :: Pipeline -> IO Build
  }

makeService :: Docker.Service -> Service
makeService docker =
  Service
    { runBuild = runBuild_ docker,
      prepareBuild = prepareBuild_ docker
    }

runBuild_ :: Docker.Service -> Build -> IO Build
runBuild_ docker build =
  Core.progress docker build >>= \newBuild ->
    case state newBuild of
      BuildFinished _ -> pure newBuild
      _ -> threadDelay runInterval >> runBuild_ docker newBuild

prepareBuild_ :: Docker.Service -> Pipeline -> IO Build
prepareBuild_ docker pipeline = do
  pure
    Build
      { pipeline = pipeline,
        state = BuildReady,
        completedSteps = mempty
      }

runInterval = 1000 * 1000
