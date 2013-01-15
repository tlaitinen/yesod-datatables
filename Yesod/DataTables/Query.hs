module Yesod.DataTables.Query (DataTable(..), 
                               RegexFlag,
                               ColumnName,
                               select) where

import Yesod.DataTables.Request
import Yesod.DataTables.Reply

import Database.Persist as D
import Data.Aeson as J
type RegexFlag = Bool

type DataTable val m = DataTable {
        dtGlobalSearch :: Text -> RegexFlag -> [Filter val],
        dtSort         :: [(ColumnName,SortDir)] -> [SelectOpt val],
        dtColumnSearch :: ColumnName -> Text -> RegexFlag -> [Filter val],
        dtFilters      :: [Filter val]
        dtValue        :: ColumnName -> Entity val -> Text
    }
    


select :: (PersistEntity val, 
               PersistQuery m, 
               PersistEntityBackend val) 
           => DataTable val -> Req -> m Reply
select dt req = do
    totalCount   <- D.count (dtFilters dt)
    displayCount <- D.count filters
    entities     <- D.selectList filters selectOpts
    return $ Reply {
        replyNumRecords = totalCount,
        replyNumDisplayRecords = displayCount,
        replyRecords = J.toJSON $ map formatEntity entities,
        replyEcho = reqEcho req
    }
    where
        filters = dtFilters dt ++ colSearchFilters ++ globalSearchFilters
        colSearchFilters = map (\(c,s,r) -> dtColumnSearch c s r) 
                               [ (colName c,
                                  colSearch c,
                                  colSearchRegex c)
                                  | c <- reqColumns req, colSortable c ]
        globalSearchFilters = dtGlobalSearch (reqSearch req) 
                                             (reqSearchRegex req)
        formatEntity entity = J.object $ map (formatColumn entity) 
                                          [ colName c | c <- reqColumns req]
        formatColumn entity colName = colName .= (dtValue colName entity)
        selectOpts = [OffsetBy (reqDisplayStart req),
                      LimitTo (reqDisplayLength req)] 
                     ++ dtSort dt (reqSort req)

