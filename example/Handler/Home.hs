{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Handler.Home where

import Import
import Yesod.DataTables
import Data.Aeson as J
import Text.Julius
import qualified Data.Text as T
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.String.Utils as SU


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
userValueByCol "id" (Entity key _)  = T.pack $ show key
userValueByCol "ident" (Entity _ u) =  userIdent u
userValueByCol "name" (Entity _ u) = T.concat [userFirstName u, " ", userLastName u]
userValueByCol "age" (Entity _ u) = T.pack $ show $ userAge u
userValueByCol "email" (Entity _ _) = ""


userValueByCol _ _ =  ""

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


dataTablesWidget _ = do
    widgetId <- newIdent    
    addStylesheet $ StaticR css_jquery_dataTables_css
    addScript $ StaticR js_jquery_1_9_0_min_js        
    addScript $ StaticR js_datatables_js
    $(widgetFile "dataTablesWidget")

getDataTableR :: Handler Value
getDataTableR = do
    httpReq <- getRequest
    let request = parseRequest (reqGetParams httpReq)
    if isJust request
        then do
            reply <- runDB $ dataTableSelect userDataTable (fromJust request)
            liftIO $ putStrLn $ B.unpack $ B.concat $ BL.toChunks $ J.encode $ formatReply reply
            returnJson $ formatReply reply
        else
            returnJson $ J.object [ "error" .= ("could not parse request" :: Text)]
getHomeR :: Handler Html
getHomeR = do
    

    maybeUser <- runDB $ (selectFirst [] [Asc UserFirstName] ) 
    runDB $ if isNothing maybeUser 
        then mapM_ (\user -> do
                        userKey <- insert user
                        _ <- insert $ Email (T.concat [ 
                                        T.toLower $ userFirstName user, "@", 
                                        T.toLower $ userLastName user,
                                        ".com" ])
                            (Just userKey)  Nothing
                        insert $ Email (T.concat [ "info@", 
                                        T.toLower $ userLastName user,
                                        ".com" ])
                            (Just userKey)  Nothing)
                   exampleUsers
        else return ()
    aDomId <- newIdent
    defaultLayout $ do
        setTitle "Welcome To Yesod.DataTables example!"
        $(widgetFile "homepage")

    


exampleUsers :: [User] 
exampleUsers = [
        User "lupita51" "Lupita" "Alday" 51 Nothing,
        User "elizbeth23" "Elizbeth" "Bosco" 23 Nothing,
        User "leilani28" "Leilani" "Cooper" 28 Nothing,
        User "ward14" "Ward" "Galland" 14 Nothing,
        User "berna90" "Berna" "Foston" 90 Nothing,
        User "bruna59" "Bruna" "Axford" 59 Nothing,
        User "roxana85" "Roxana" "Booth" 85 Nothing,
        User "heather17" "Heather" "Bolivar" 17 Nothing,
        User "scarlett78" "Scarlett" "Morrisey" 78 Nothing,
        User "ryann19" "Ryann" "Chenoweth" 19 Nothing,
        User "tami58" "Tami" "Bautch" 58 Nothing,
        User "lashawna5" "Lashawna" "Berrier" 5 Nothing,
        User "abbie13" "Abbie" "Quail" 13 Nothing,
        User "edna53" "Edna" "Waynick" 53 Nothing,
        User "gertie44" "Gertie" "Forbus" 44 Nothing,
        User "lemuel56" "Lemuel" "Everette" 56 Nothing,
        User "claris50" "Claris" "Lentz" 50 Nothing,
        User "carie44" "Carie" "Squire" 44 Nothing,
        User "kimbery31" "Kimbery" "Clewis" 31 Nothing,
        User "deloras59" "Deloras" "Profitt" 59 Nothing,
        User "carolee62" "Carolee" "Woodford" 62 Nothing,
        User "mickey82" "Mickey" "Aguayo" 82 Nothing,
        User "alissa32" "Alissa" "Hausman" 32 Nothing,
        User "alanna71" "Alanna" "Detrick" 71 Nothing,
        User "lasonya64" "Lasonya" "Hammers" 64 Nothing,
        User "isaac48" "Isaac" "Heineman" 48 Nothing,
        User "mariana85" "Mariana" "Stansell" 85 Nothing,
        User "emmett59" "Emmett" "Newcomer" 59 Nothing,
        User "grant22" "Grant" "Lacey" 22 Nothing,
        User "joanie4" "Joanie" "Roberge" 4 Nothing,
        User "odell63" "Odell" "Shunk" 63 Nothing,
        User "yasuko44" "Yasuko" "Kerbs" 44 Nothing,
        User "mozella55" "Mozella" "Stalker" 55 Nothing,
        User "tommy58" "Tommy" "Peaden" 58 Nothing,
        User "rigoberto60" "Rigoberto" "Asaro" 60 Nothing,
        User "ciara63" "Ciara" "Ducan" 63 Nothing,
        User "adrian74" "Adrian" "Greeson" 74 Nothing,
        User "sheri78" "Sheri" "Sickels" 78 Nothing,
        User "sharita14" "Sharita" "Custard" 14 Nothing,
        User "margurite75" "Margurite" "Mok" 75 Nothing,
        User "deann69" "Deann" "Sacks" 69 Nothing,
        User "farrah46" "Farrah" "Diggs" 46 Nothing,
        User "ashlee84" "Ashlee" "Downer" 84 Nothing,
        User "tuyet86" "Tuyet" "Wagaman" 86 Nothing,
        User "leonor7" "Leonor" "Asuncion" 7 Nothing,
        User "herbert77" "Herbert" "Hennis" 77 Nothing,
        User "joleen69" "Joleen" "Starkey" 69 Nothing,
        User "hyman73" "Hyman" "Swanigan" 73 Nothing,
        User "denese45" "Denese" "Cleveland" 45 Nothing,
        User "devona10" "Devona" "Espinosa" 10 Nothing
    ]
