{-# LANGUAGE TupleSections, OverloadedStrings #-}



module Handler.Home where

import Import
import Yesod.DataTables
import Data.Aeson as J
import Text.Julius
import Database.Persist.Store
import qualified Data.Text as T
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
likeFilter :: EntityField v Text -> Text -> Filter v
likeFilter fld search = Filter fld (Left $ 
                             T.concat ["%"::Text, search::Text, "%"::Text])
                             (BackendSpecificFilter "LIKE")

userFieldSort :: (ColumnName,SortDir) -> [SelectOpt User]
userFieldSort ("ident", SortAsc) = [Asc UserIdent]
userFieldSort ("ident", SortDesc) = [Desc UserIdent]
userFieldSort ("name", SortAsc) = [Asc UserLastName, Asc UserFirstName]
userFieldSort ("name", SortDesc) = [Desc UserLastName, Desc UserLastName]
userFieldSort ("age", SortAsc) = [Asc UserAge]
userFieldSort ("age", SortDesc) = [Desc UserAge]
userFieldSort _ = []

userColSearch :: ColumnName -> Text -> RegexFlag -> [Filter User]
userColSearch "ident" search _ = [likeFilter UserIdent search]
userColSearch "name" search _ = [likeFilter UserFirstName search,
                                 likeFilter UserLastName search] 
userColSearch "age" search _ = [likeFilter UserIdent search]
userColSearch _ _ _ = []

readMaybe :: (Read a) => Text -> Maybe a
readMaybe s = case reads (T.unpack s) of
              [(x, "")] -> Just x
              _ -> Nothing

maybeUserAgeSearch :: Text -> [Filter User] -> [Filter User]
maybeUserAgeSearch search filters = maybe filters (\a -> filters ||. [UserAge ==. a]) (readMaybe search)

userValueByCol :: ColumnName -> Entity User -> Text
userValueByCol "id" (Entity (Key (PersistInt64 val)) _) = T.pack $ show val
userValueByCol "ident" (Entity _ u) = userIdent u
userValueByCol "name" (Entity _ u) = T.concat [userFirstName u, " ", userLastName u]
userValueByCol "age" (Entity _ u) = T.pack $ show $ userAge u
userValueByCol _ _ = ""

userDataTable :: DataTable User
userDataTable = DataTable {
    dtGlobalSearch = \search _ ->  maybeUserAgeSearch search
                [(likeFilter UserIdent search)]
                ||. [(likeFilter UserFirstName search)]
                ||. [(likeFilter UserLastName search)],
    dtSort = concatMap userFieldSort,
    dtColumnSearch = userColSearch,
    dtFilters = [],
    dtValue = userValueByCol,
    dtRowId = userValueByCol "id"


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
    httpReq <- getRequest
    let request = parseRequest (reqGetParams httpReq)
    if isJust request
        then do
            reply <- runDB $ dataTableSelect userDataTable (fromJust request)
            liftIO $ putStrLn $ B.unpack $ B.concat $ BL.toChunks $ J.encode $ formatReply reply
            jsonToRepJson $ formatReply reply
        else
            jsonToRepJson $ J.object [ "error" .= ("could not parse request" :: Text)]

getHomeR :: Handler RepHtml
getHomeR = do
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod.DataTables example!"
        $(widgetFile "homepage")
