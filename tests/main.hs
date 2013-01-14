import Yesod.DataTables.Request as R
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
main :: IO ()
main = defaultMain tests

requestProperty :: Bool
requestProperty = R.parseRequest [("iDisplayStart", "0"),
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
                             ] == Just R.Req {
                                 R.reqDisplayStart = 0,
                                 R.reqDisplayLength = 10,
                                 R.reqSearch = "foo",
                                 R.reqSearchRegex = False,
                                 R.reqColumns = [ 
                                    R.Column {
                                        R.colSearchable = True,
                                        R.colSearch = "bar.*baz",
                                        R.colSearchRegex = True,
                                        R.colSortable = False,
                                        R.colDataProp = "id"
                                    },
                                    R.Column {
                                        R.colSearchable = False,
                                        R.colSearch = "quux",
                                        R.colSearchRegex = False,
                                        R.colSortable = True,
                                        R.colDataProp = "name"
                                    }        
                                 ],
                                 R.reqSort = [(0,R.SortAsc), (1, R.SortDesc)],
                                 R.reqEcho = 1
                             }
                                              


tests :: [Test]
tests = [
        testGroup "request" $ [
            testProperty "request-property" requestProperty
        ]
    ]
