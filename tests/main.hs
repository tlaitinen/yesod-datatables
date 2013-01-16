{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts, EmptyDataDecls #-}
import Prelude 
import Yesod.DataTables.Request
import Yesod.DataTables.Reply 
import qualified Yesod.DataTables.Query as DT
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Aeson as J
import Data.Attoparsec as P
import Data.ByteString as B
import Data.ByteString.Lazy as BL
import Data.Text as T
import Data.Maybe
import Database.Persist
import Database.Persist.Store
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.Trans.Resource (runResourceT)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Person
    name Text
    age Int Maybe
    deriving Show
|]


main :: IO ()
main = defaultMain tests

testRequest :: Request
testRequest = Request {
        reqDisplayStart = 0,
        reqDisplayLength = 10,
        reqSearch = "foo",
        reqSearchRegex = False,
        reqColumns = [ 
            Column {
                colSearchable = True,
                colSearch = "bar.*baz",
                colSearchRegex = True,
                colSortable = False,
                colName = "id"
            },
            Column {
                colSearchable = False,
                colSearch = "quux",
                colSearchRegex = False,
                colSortable = True,
                colName = "name"
            }        
        ],
        reqSort = [("name",SortAsc), ("id", SortDesc)],
        reqEcho = 1,
        reqParams = J.object []
    }


requestProperty :: Bool
requestProperty = parseRequest [("iDisplayStart", "0"),
                              ("iDisplayLength", "10"),
                               ("iColumns", "2"),
                               ("sSearch", "foo"),
                               ("bRegex", "0"),
                               ("bSearchable_0", "1"),
                               ("bSearchable_1", "0"),
                               ("bSearch_0", "bar.*baz"),
                               ("bSearch_1", "quux"),
                               ("bRegex_0", "1"),
                               ("bRegex_1", "0"),
                               ("bSortable_0", "0"),
                               ("bSortable_1", "1"),
                               ("mDataProp_0", "id"),
                               ("mDataProp_1", "name"),
                               ("iSortingCols", "2"),
                               ("iSortCol_0", "1"),
                               ("sSortDir_0", "asc"),
                               ("iSortCol_1", "0"),
                               ("sSortDir_1", "desc"),
                               ("sEcho", "1"),
                               ("fnServerParams", "{}")
                             ] == Just testRequest
                                                                          

replyProperty :: Bool
replyProperty = (Just expectedReply) == (P.maybeResult parsedReply)
    where
        records = J.toJSON [
                    J.object [
                        "id" .= (4321 :: Int),
                        "name" .= ("row 1"::Text)
                    ],
                    J.object [
                        "id" .= (2468 :: Int),
                        "name" .= ("row 2"::Text)
                    ]
                ]

        formattedReply = formatReply (Reply {
                replyNumRecords = 123,
                replyNumDisplayRecords = 64,
                replyRecords = records,
                replyEcho = 1
            }) 
        parsedReply = P.parse J.json ((B.concat . BL.toChunks) formattedReply)
        expectedReply = J.object [
                "iTotalRecords" .= (123 :: Int),
                "iTotalDisplayRecords" .= (64 :: Int),
                "sEcho" .= (1 :: Int),
                "aaData" .= records
            ]

queryTestRequest1 :: Request
queryTestRequest1 = Request {
        reqDisplayStart = 1,
        reqDisplayLength = 2,
        reqSearch = "",
        reqSearchRegex = False,
        reqColumns = [ 
            Column {
                colSearchable = False,
                colSearch = "",
                colSearchRegex = False,
                colSortable = True,
                colName = "id"
            },
            Column {
                colSearchable = False,
                colSearch = "",
                colSearchRegex = False,
                colSortable = True,
                colName = "name"
            },
            Column {
                colSearchable = False,
                colSearch = "",
                colSearchRegex = False,
                colSortable = True,
                colName = "age"
            }        
        ],
        reqSort = [("name",SortAsc)],
        reqEcho = 1
    }
queryTestReply1 :: Key val -> Key val -> Key val -> Key val -> Reply
queryTestReply1 johnId janeId jackId jillId = Reply {
        replyNumRecords = 3,
        replyNumDisplayRecords = 3,
        replyRecords = J.toJSON [
                J.object [
                    "id" .= ("2" :: Text),
                    "name" .= ("Jane Doe" :: Text),
                    "age" .= ("" :: Text)
                ],
                J.object [
                    "id" .= ("3" :: Text),
                    "name" .= ("Jeff Black" :: Text),
                    "age" .= ("24" :: Text)
                ]
            ],
        replyEcho = 1                
    }


queryTestDataTable :: DT.DataTable Person
queryTestDataTable = DT.DataTable {
        DT.dtGlobalSearch = \text regexFlag -> [],
        DT.dtSort = \sorts -> [],
        DT.dtColumnSearch = \cname search regex -> [],
        DT.dtFilters = \_ -> [ PersonName <-. [ "John Doe", 
                                          "Jane Doe", 
                                          "Jeff Black" ] ],
        DT.dtValue = myDtValue 
    }
    where 
        myDtValue cname (Entity (Key (PersistInt64 key)) value)
            | cname == "id"   = T.pack $ show key
            | cname == "name" = personName value
            | cname == "age"  = T.pack $ maybe "" show $ personAge value
            | otherwise = ""
        myDtValue _ _ = ""
queryTest :: IO ()
queryTest = do
    Prelude.putStrLn $ show $ parseRequest [("iDisplayStart", "0"),
                              ("iDisplayLength", "10"),
                               ("iColumns", "2"),
                               ("sSearch", "foo"),
                               ("bRegex", "0"),
                               ("bSearchable_0", "1"),
                               ("bSearchable_1", "0"),
                               ("bSearch_0", "bar.*baz"),
                               ("bSearch_1", "quux"),
                               ("bRegex_0", "1"),
                               ("bRegex_1", "0"),
                               ("bSortable_0", "0"),
                               ("bSortable_1", "1"),
                               ("mDataProp_0", "id"),
                               ("mDataProp_1", "name"),
                               ("iSortingCols", "2"),
                               ("iSortCol_0", "1"),
                               ("sSortDir_0", "asc"),
                               ("iSortCol_1", "0"),
                               ("sSortDir_1", "desc"),
                               ("sEcho", "1"),
                               ("fnServerParams", "")]
                   
    (reply, johnId, janeId, jackId, jillId) <- runResourceT 
            $ withSqliteConn ":memory:" $ runSqlConn $ do
        runMigration migrateAll
        johnId <- insert $ Person "John Doe" $ Just 35
        janeId <- insert $ Person "Jane Doe" Nothing
        jackId <- insert $ Person "Jeff Black" $ Just 24
        jillId <- insert $ Person "Jill Black" Nothing
        reply <- DT.select queryTestDataTable queryTestRequest1
        return (reply, johnId, janeId, jackId, jillId)
    reply @=? (queryTestReply1 johnId janeId jackId jillId)
    Prelude.putStrLn (show reply)



tests :: [Test.Framework.Test]
tests = [
        testGroup "request" $ [
            testProperty "request-property" requestProperty
        ],
        testGroup "reply" $ [
            testProperty "reply-property" replyProperty
        ],
        testGroup "query" $ [
            testCase "query-test" queryTest
        ]
    ]
