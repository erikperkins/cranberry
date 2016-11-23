module Application where

import Api.Core (Api(Api))
import Control.Lens
import Snap.Snaplet

data App = App { _api :: (Snaplet Api) }
api :: Lens' App (Snaplet Api)
api = lens _api (\a b -> a { _api = b })

type AppHandler = Handler App App
