import System.Environment (getEnv)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (pack)
import qualified Data.Conduit as C
import Data.Conduit.List as CL
import Web.Twitter.Conduit
import Web.Twitter.Conduit.Stream

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings

  accessToken <- getEnv "TWITTER_ACCESS_TOKEN"
  accessSecret <- getEnv "TWITTER_ACCESS_SECRET"
  consumerKey <- getEnv "TWITTER_CONSUMER_KEY"
  consumerSecret <- getEnv "TWITTER_CONSUMER_SECRET"
  twitterFeed <- getEnv "TWITTER_FEED"

  let potus = statusesFilter [Follow [(read twitterFeed :: Integer)]]

  let credential = Credential [(pack "oauth_token", pack accessToken), (pack "oauth_token_secret", pack accessSecret)]

  let tokens = twitterOAuth {oauthConsumerKey = pack consumerKey, oauthConsumerSecret = pack consumerSecret}

  let twInfo = setCredential tokens credential def

  runResourceT $ do
    src <- stream twInfo manager potus
    C.runConduit $ src C..| CL.mapM_ (liftIO . print)
