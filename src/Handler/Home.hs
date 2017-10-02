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

getHomeR :: Handler Html
getHomeR = do
    (songs,mCurrent) <- withMPD $ (,) <$> MPD.playlistInfo Nothing
                                      <*> MPD.currentSong
    let isCurrent s = MPD.sgId s == (mCurrent >>= MPD.sgId)
        currentPos  = fromMaybe 0 (mCurrent >>= MPD.sgIndex)
    defaultLayout $ do
        $(widgetFile "homepage")
  where
    showPosition = maybe "?" show . MPD.sgIndex
    get' s t = maybe "" MPD.toText $ lookupTagOf s t


    hasPos = isJust   . MPD.sgIndex
    posOf  = fromJust . MPD.sgIndex




    duration s = let (m,secs) = quotRem (MPD.sgLength s) 60
                 in mconcat [show m, ":", showPad secs]


showPad x = let s = show x
            in if length s == 1 then '0':s else s



-- postHomeR :: Handler Html
-- postHomeR = do
--     ((result, formWidget), formEnctype) <- runFormPost sampleForm
--     let handlerName = "postHomeR" :: Text
--         submission = case result of
--             FormSuccess res -> Just res
--             _ -> Nothing

--     defaultLayout $ do
--         let (commentFormId, commentTextareaId, commentListId) = commentIds
--         aDomId <- newIdent
--         setTitle "Welcome To Yesod!"
--         $(widgetFile "homepage")

-- sampleForm :: Form FileForm
-- sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
--     <$> fileAFormReq "Choose a file"
--     <*> areq textField textSettings Nothing
--     -- Add attributes like the placeholder and CSS classes.
--     where textSettings = FieldSettings
--             { fsLabel = "What's on the file?"
--             , fsTooltip = Nothing
--             , fsId = Nothing
--             , fsName = Nothing
--             , fsAttrs =
--                 [ ("class", "form-control")
--                 , ("placeholder", "File description")
--                 ]
--             }

-- commentIds :: (Text, Text, Text)
-- commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
