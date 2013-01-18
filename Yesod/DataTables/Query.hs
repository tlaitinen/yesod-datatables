{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Yesod.DataTables.Query (DataTable(..), 
                               RegexFlag,
                               ColumnName,
                               dataTableSelect) where

import Prelude
import Yesod.DataTables.Request
import Yesod.DataTables.Reply
import Data.Text

import Database.Persist as D
import Data.Aeson as J
type RegexFlag = Bool

data DataTable val = forall m. DataTable {
        -- | mapping global search field to filters
        dtGlobalSearch :: Text -> RegexFlag -> [Filter val],

        -- | mapping sorting instructions to select options
        dtSort         :: [(ColumnName,SortDir)] -> [SelectOpt val],

        -- | mapping a column search to filters
        dtColumnSearch :: ColumnName -> Text -> RegexFlag -> [Filter val],

        -- | filters that are always applied
        dtFilters      :: [Filter val],

        -- | mapping column name and entity to a textual value
        dtValue        :: (PersistEntity val, 
                           PersistQuery m, 
                           PersistEntityBackend val ~ PersistMonadBackend m)
                       => ColumnName -> Entity val -> Text,

        -- | mapping entity to a row identifier
        dtRowId        :: (PersistEntity val, 
                           PersistQuery m, 
                           PersistEntityBackend val ~ PersistMonadBackend m)
                       => Entity val -> Text
    }


dataTableSelect :: (PersistEntity val, 
               PersistQuery m, 
               PersistEntityBackend val ~ PersistMonadBackend m) 
           => DataTable val -> Request -> m Reply
dataTableSelect (DataTable dtGlobalSearch' dtSort' dtColumnSearch' dtFilters' dtValue' dtRowId') req = do
    totalCount   <- D.count $ dtFilters'
    let filters = dtFilters' ++ colSearchFilters ++ globalSearchFilters
    displayCount <- D.count filters
    entities     <- D.selectList filters selectOpts
    records      <- mapM formatEntity entities
    return $ Reply {
        replyNumRecords = fromIntegral totalCount,
        replyNumDisplayRecords = displayCount,
        replyRecords = J.toJSON $ records,
        replyEcho = reqEcho req
    }
    where
        colSearchFilters = Prelude.concatMap (\(c,s,r) -> dtColumnSearch'  c s r) 
                                       [ (colName c, 
                                          colSearch c, 
                                          colSearchRegex c)
                                       | c <- reqColumns req, colSortable c ]
        globalSearchFilters = dtGlobalSearch' (reqSearch req) 
                                              (reqSearchRegex req)
        
        formatEntity entity = do
            rowId   <- dtRowId' entity
            columns <- mapM (formatColumn entity) [colName c | c <- reqColumns req]
            return $ J.object $ [ "DT_RowId" .= rowId ] ++ columns
        formatColumn entity cn = do
            value <- dtValue' cn entity
            return $ cn .= value
        selectOpts  = [OffsetBy (reqDisplayStart req),
                      LimitTo (reqDisplayLength req)] 
                     ++ dtSort' (reqSort req)

