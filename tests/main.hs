import Prelude 
import Yesod.DataTables.Request
import Yesod.DataTables.Reply 
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.Aeson as J
import Data.Attoparsec as P
import Data.ByteString as B
import Data.ByteString.Lazy as BL
import Data.Text (Text)
main :: IO ()
main = defaultMain tests

testRequest :: Req
testRequest = Req {
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
                colDataProp = "id"
            },
            Column {
                colSearchable = False,
                colSearch = "quux",
                colSearchRegex = False,
                colSortable = True,
                colDataProp = "name"
            }        
        ],
        reqSort = [(0,SortAsc), (1, SortDesc)],
        reqEcho = 1
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
                               ("iSortCol_0", "0"),
                               ("sSortDir_0", "asc"),
                               ("iSortCol_1", "1"),
                               ("sSortDir_1", "desc"),
                               ("sEcho", "1")
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

        formattedReply = formatReply testRequest (Reply {
                replyNumRecords = 123,
                replyNumDisplayRecords = 64,
                replyRecords = records
            }) 
        parsedReply = P.parse J.json ((B.concat . BL.toChunks) formattedReply)
        expectedReply = J.object [
                "iTotalRecords" .= (123 :: Int),
                "iTotalDisplayRecords" .= (64 :: Int),
                "sEcho" .= (1 :: Int),
                "aaData" .= records
            ]
        
tests :: [Test]
tests = [
        testGroup "request" $ [
            testProperty "request-property" requestProperty
        ],
        testGroup "reply" $ [
            testProperty "reply-property" replyProperty
        ]
    ]
