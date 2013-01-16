{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
module Yesod.DataTables.Query (DataTable(..), 
                               RegexFlag,
                               ColumnName,
                               select) where

import Prelude
import Yesod.DataTables.Request
import Yesod.DataTables.Reply
import Data.Text

import Database.Persist as D
import Data.Aeson as J
type RegexFlag = Bool

data DataTable val = DataTable {
        -- | mapping global search field to filters
        dtGlobalSearch :: Text -> RegexFlag -> [Filter val],

        -- | mapping sorting instructions to select options
        dtSort         :: [(ColumnName,SortDir)] -> [SelectOpt val],

        -- | mapping a column search to filters
        dtColumnSearch :: ColumnName -> Text -> RegexFlag -> [Filter val],

        -- | filters that are always applied
        dtFilters      :: [Filter val],

        -- | mapping column name and entity to a textual value
        dtValue        :: ColumnName -> Entity val -> Text
    }
    


select :: (PersistEntity val, 
               PersistQuery m, 
               PersistEntityBackend val ~ PersistMonadBackend m) 
           => DataTable val -> Request -> m Reply
select dt req = do
    totalCount   <- D.count $ (dtFilters dt) 
    displayCount <- D.count (filters dt)
    entities     <- D.selectList (filters dt) (selectOpts dt)
    return $ Reply {
        replyNumRecords = fromIntegral totalCount,
        replyNumDisplayRecords = displayCount,
        replyRecords = J.toJSON $ Prelude.map formatEntity entities,
        replyEcho = reqEcho req
    }
    where
        filters :: DataTable val -> [Filter val]
        filters dt = ((dtFilters dt) ) ++ colSearchFilters dt ++ globalSearchFilters dt
        colSearchFilters :: DataTable val -> [Filter val]
        colSearchFilters dt = Prelude.concatMap (\(c,s,r) -> (dtColumnSearch dt) c s r) 
                                       [ (colName c, 
                                          colSearch c, 
                                          colSearchRegex c)
                                       | c <- reqColumns req, colSortable c ]
        globalSearchFilters :: DataTable val -> [Filter val]                                       
        globalSearchFilters dt = (dtGlobalSearch dt) (reqSearch req) 
                                                  (reqSearchRegex req)
        formatEntity entity = J.object $ Prelude.map (formatColumn entity) 
                                          [ colName c | c <- reqColumns req]
        formatColumn entity cn = cn .= (dtValue dt cn entity)
        selectOpts dt = [OffsetBy (reqDisplayStart req),
                      LimitTo (reqDisplayLength req)] 
                     ++ (dtSort dt) (reqSort req)

