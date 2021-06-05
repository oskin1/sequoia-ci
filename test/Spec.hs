module Main where

import Core
import qualified Docker
import RIO
import qualified RIO.Map as Map
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial
import qualified Runner
import qualified System.Process.Typed as Process
import Test.Hspec (beforeAll, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec do
  let docker = Docker.makeService
  let runner = Runner.makeService docker
  beforeAll cleanupDocker $ describe "Sequoia CI" do
    it "should run a build (success)" do
      testRunSuccess runner
    it "should run a build (failure)" do
      testRunFailure runner

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
  build <- Runner.prepareBuild runner testPipeline
  res <- Runner.runBuild runner build
  state res `shouldBe` BuildFinished BuildSucceeded
  Map.elems (completedSteps res) `shouldBe` [StepSucceeded, StepSucceeded]

testRunFailure :: Runner.Service -> IO ()
testRunFailure runner = do
  build <-
    Runner.prepareBuild runner $
      makePipeline [makeStep "Should fail" "ubuntu" ["exit 1"]]
  result <- Runner.runBuild runner build
  state result `shouldBe` BuildFinished BuildFailed
  Map.elems (completedSteps result) `shouldBe` [StepFailed (Docker.ContainerExitCode 1)]

cleanupDocker :: IO ()
cleanupDocker = void do
  Process.readProcessStdout "docker rm -f $(docker ps -aq --filter \"label=quad\")"

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands =
  Step
    { name = StepName name,
      image = Docker.Image image,
      commands = NonEmpty.Partial.fromList commands
    }

makePipeline :: [Step] -> Pipeline
makePipeline steps =
  Pipeline {steps = NonEmpty.Partial.fromList steps}

testPipeline =
  makePipeline
    [ makeStep "First" "ubuntu" ["date"],
      makeStep "Second" "ubuntu" ["uname", "-r"]
    ]

testBuild =
  Build
    { pipeline = testPipeline,
      state = BuildReady,
      completedSteps = mempty
    }
