-- Define application state type and alias for handler monad
module Application where

import Api.Core (Api(Api))
import Control.Lens (lens, Lens')
import Snap.Snaplet (Handler, Snaplet)

data App = App { _api :: Snaplet Api }
api :: Lens' App (Snaplet Api)
api = lens _api (\a b -> a { _api = b })

type AppHandler = Handler App App
