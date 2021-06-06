module Core where

import qualified Docker
import RIO
import qualified RIO.List as List
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NonEmpty
import qualified RIO.Text as Text

progress :: Docker.Service -> Build -> IO Build
progress docker build =
  case state build of
    BuildReady ->
      case buildHasNextStep build of
        Left res -> pure build {state = BuildFinished res}
        Right step -> do
          let script = Text.unlines $ ["set -ex"] <> NonEmpty.toList (commands step)
          let opts = Docker.ContainerOptions (image step) script
          containerId <- Docker.createContainer docker opts
          let s = BuildRunningState {step = name step, containerId = containerId}
          Docker.startContainer docker containerId
          pure build {state = BuildRunning s}
    BuildRunning state ->
      Docker.containerStatus docker (containerId state) >>= \case
        Docker.ContainerRunning -> pure build
        Docker.ContainerExited code ->
          pure build {completedSteps = Map.insert (step state) res (completedSteps build)}
          where
            res = exitCodeToStepResult code
        Docker.ContainerOther other ->
          pure build {state = BuildFinished s}
          where
            s = BuildUnexpectedState other
    BuildFinished _ -> pure build

data Build = Build
  { pipeline :: Pipeline,
    state :: BuildState,
    completedSteps :: Map StepName StepResult
  }
  deriving (Eq, Show)

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build =
  if allSucceeded
    then case nextStep of
      Just step -> Right step
      Nothing -> Left BuildSucceeded
    else Left BuildFailed
  where
    allSucceeded = List.all (== StepSucceeded) $ completedSteps build
    nextStep = List.find nonCompeled $ steps $ pipeline build
    nonCompeled step = not $ Map.member (name step) (completedSteps build)

data BuildState
  = BuildRunning BuildRunningState
  | BuildReady
  | BuildFinished BuildResult
  deriving (Eq, Show)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  | BuildUnexpectedState Text
  deriving (Eq, Show)

exitCodeToStepResult :: Docker.ContainerExitCode -> StepResult
exitCodeToStepResult exitCode =
  case Docker.unExitCode exitCode of
    0 -> StepSucceeded
    _ -> StepFailed exitCode

newtype Pipeline = Pipeline {steps :: NonEmpty Step} deriving (Eq, Show)

data StepResult
  = StepFailed Docker.ContainerExitCode
  | StepSucceeded
  deriving (Eq, Show)

data Step = Step
  { name :: StepName,
    commands :: NonEmpty Text,
    image :: Docker.Image
  }
  deriving (Eq, Show)

data BuildRunningState = BuildRunningState
  { step :: StepName,
    containerId :: Docker.ContainerId
  }
  deriving (Eq, Show)

newtype StepName = StepName {unStepName :: Text}
  deriving (Eq, Show, Ord)
