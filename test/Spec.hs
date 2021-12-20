import Control.Concurrent
import Data.Aeson.QQ
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Network.Wai.Handler.Warp
import Relude
import Routes
import Test.Hspec

baseRequest :: Request
baseRequest = "http://localhost:3000/courses"

main = do
  serverThread <- forkIO (run 3000 =<< newApp)
  hspec do
    describe "course manager server" do
      it "should start with no courses" do
        getResponseBody <$> httpJSON baseRequest
          `shouldReturn` [aesonQQ|{"courses":[]}|]
      it "should add one course" do
        getResponseStatus
          <$> httpNoBody
            ( setRequestBodyJSON
                [aesonQQ|{"name": "Linux for children", "status": "available"}|]
                (setRequestMethod "POST" baseRequest)
            )
          `shouldReturn` created201
      it "should fail to add the same course twice" do
        getResponseStatus
          <$> httpNoBody
            ( setRequestBodyJSON
                [aesonQQ|{"name": "Linux for children", "status": "available"}|]
                (setRequestMethod "POST" baseRequest)
            )
          `shouldReturn` badRequest400
      it "should add a second course successfully" do
        getResponseStatus
          <$> httpNoBody
            ( setRequestBodyJSON
                [aesonQQ|{"name": "MacOS for tots", "status": "available"}|]
                (setRequestMethod "POST" baseRequest)
            )
          `shouldReturn` created201
      it "should return two courses" do
        getResponseBody <$> httpJSON baseRequest
          `shouldReturn` [aesonQQ|{"courses":[{"id":1,"name":"Linux for children"},{"id":2, "name":"MacOS for tots"}]}|]
