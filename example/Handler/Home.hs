{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.DataTables
import Data.Aeson as J
import Text.Julius
-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
userDataTable :: DataTable User
userDataTable = DataTable {
}

dataTablesWidget :: DataTable val -> Widget
dataTablesWidget dt = do
    widgetId <- lift newIdent    
    addStylesheet $ StaticR css_jquery_dataTables_css
    addScript $ StaticR js_jquery_1_9_0_min_js        
    addScript $ StaticR js_datatables_js
    $(widgetFile "dataTablesWidget")

getDataTableR :: Handler RepJson
getDataTableR = do
    jsonToRepJson $ J.object [ "ok" .= (1 :: Int) ]

getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing
