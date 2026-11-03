module Handler.Health
  ( getHealthR
  ) where

import App
import Yesod.Core

getHealthR :: Handler Value
getHealthR = returnJson $ object ["ok" .= True]
