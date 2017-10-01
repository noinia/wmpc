{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Actions where

import Handler.Common
import Import
import qualified Network.MPD as MPD

getPlayR :: Handler ()
getPlayR = withMPD $ MPD.play Nothing

getPlayNRR   :: MPD.Position -> Handler ()
getPlayNRR p = withMPD $ MPD.play (Just p)

getPauseR :: Handler ()
getPauseR = withMPD $ MPD.pause True -- no idea where the true comes from


getToggleR :: Handler ()
getToggleR = withMPD $ do
                         s <- MPD.stState <$> MPD.status
                         let b = case s of
                                   MPD.Playing -> True
                                   _           -> False
                         MPD.pause b

getStopR :: Handler ()
getStopR = withMPD MPD.stop

getPrevR :: Handler ()
getPrevR = withMPD MPD.previous

getNextR :: Handler ()
getNextR = withMPD MPD.next
