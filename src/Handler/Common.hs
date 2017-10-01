{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- | Common handler functions.
module Handler.Common where

import           Data.FileEmbed (embedFile)
import           Import
import qualified Network.MPD as MPD

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")



withMPD   :: MPD.MPD a -> Handler a
withMPD a = do
    hst <- getsYesod $ mpdHost . appSettings
    prt <- getsYesod $ mpdPort . appSettings
    pw  <- getsYesod $ mpdPassword . appSettings
    r   <- liftIO $ MPD.withMPDEx hst prt (fromMaybe "" pw) a
    case r of
      Left er -> error "TODO"
      Right x -> pure x
