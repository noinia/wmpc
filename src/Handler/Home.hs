{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import           Data.Maybe (fromJust,isJust)
import           Handler.Common
import           Import
import           Network.MPD (Metadata(..))
import qualified Network.MPD as MPD

--------------------------------------------------------------------------------


withMPD   :: MPD.MPD a -> Handler a
withMPD a = do
    hst <- getsYesod $ mpdHost . appSettings
    prt <- getsYesod $ mpdPort . appSettings
    pw  <- getsYesod $ mpdPassword . appSettings
    r   <- liftIO $ MPD.withMPDEx hst prt (fromMaybe "" pw) a
    case r of
      Left er -> do setMessage (toHtml $ show er)
                    error "TODO"
                    -- redirectWith status503 HomeR
      Right x -> pure x

withMPD'   :: MPD.MPD a -> Handler Html
withMPD' a = withMPD a >> redirect HomeR

getHomeR :: Handler Html
getHomeR = do
    (songs,mCurrent) <- withMPD $ (,) <$> MPD.playlistInfo Nothing
                                      <*> MPD.currentSong
    let isCurrent s = MPD.sgId s == (mCurrent >>= MPD.sgId)
        currentPos  = fromMaybe 0 (mCurrent >>= MPD.sgIndex)
        -- songs = catMaybes $ (replicate 1000 mCurrent :: [Maybe MPD.Song])
    defaultLayout $ do
        $(widgetFile "homepage")
  where
    showPosition = maybe "?" show . MPD.sgIndex
    get' s t = maybe "" MPD.toText $ lookupTagOf s t


    hasPos = isJust   . MPD.sgIndex
    posOf  = fromJust . MPD.sgIndex

    duration s = let (m,secs) = quotRem (MPD.sgLength s) 60
                 in mconcat [show m, ":", showPad secs]

showPad   :: Show a => a -> String
showPad x = let s = show x
            in if length s == 1 then '0':s else s



getPlayR :: Handler Html
getPlayR = withMPD' $ MPD.play Nothing

getPlayNRR   :: MPD.Position -> Handler Html
getPlayNRR p = withMPD' $ MPD.play (Just p)

getPauseR :: Handler Html
getPauseR = withMPD' $ MPD.pause True -- no idea where the true comes from

getToggleR :: Handler Html
getToggleR = withMPD' $ do
                          s <- MPD.stState <$> MPD.status
                          let b = case s of
                                    MPD.Playing -> True
                                    _           -> False
                          MPD.pause b


getStopR :: Handler Html
getStopR = withMPD' MPD.stop

getPrevR :: Handler Html
getPrevR = withMPD' MPD.previous

getNextR :: Handler Html
getNextR = withMPD' MPD.next
