-- |

module Main where

import Relude
import Routes
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger

main :: IO ()
main = do
  putStrLn "Starting course-manager server"
  app <- newApp
  runEnv 3000 (logStdoutDev app)
