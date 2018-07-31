{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
-- | This module is used to make database queries based on the
-- DataTables request. 
module Yesod.DataTables.Query (DataTable(..), 
                               RegexFlag,
                               ColumnName,
                               dataTableSelect) where

import Prelude
import Yesod.DataTables.Request
import Yesod.DataTables.Reply
import Data.Text
import Control.Monad (liftM)
import Database.Persist as D
import Data.Aeson as J
import Control.Monad.IO.Class
import Control.Monad.Reader

-- | Type synonym for indicating whether a search string is a regular
-- expression.
type RegexFlag = Bool

-- | The functions in a DataTable define how search strings, column sorting,
-- filtering and value fetching is implemented.
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
        dtValue        :: ColumnName -> Entity val ->  Text,

        -- | mapping entity to a row identifier
        dtRowId        :: Entity val -> Text
    }

getJustEntity
  :: (PersistEntityBackend record ~ BaseBackend backend
     ,MonadIO m
     ,PersistEntity record
     ,PersistStoreRead backend)
  => Key record -> ReaderT backend m (Entity record)
getJustEntity key = do
  record <- getJust key
  return $
    Entity
    { entityKey = key
    , entityVal = record
    }



dataTableSelectEntities :: (PersistEntityBackend record ~ BaseBackend backend
     ,MonadIO m
     ,PersistEntity record
     ,PersistStoreRead backend, PersistQueryRead backend) => DataTable record -> Request ->  ReaderT backend m (Int, Int, [Entity record]) 
dataTableSelectEntities (DataTable dtGlobalSearch' dtSort' dtColumnSearch' dtFilters' dtValue' dtRowId') req = do
    totalCount   <- D.count $ dtFilters'
    let filters = dtFilters' ++ colSearchFilters ++ globalSearchFilters
    displayCount <- D.count filters
    entities     <- D.selectList filters selectOpts
    return (totalCount, displayCount, entities)
  where
        colSearchFilters = Prelude.concatMap (\(c,s,r) -> dtColumnSearch'  c s r) 
                                       [ (colName c, 
                                          colSearch c, 
                                          colSearchRegex c)
                                       | c <- reqColumns req, colSortable c ]
        globalSearchFilters = dtGlobalSearch' (reqSearch req) 
                                              (reqSearchRegex req)
        
        selectOpts  = [OffsetBy (reqDisplayStart req),
                      LimitTo (reqDisplayLength req)] 
                     ++ dtSort' (reqSort req)
  

-- | selects records from database and populates the grid columns using 
-- callback functions (which can issue follow-up queries)
dataTableSelect  :: (PersistEntityBackend record ~ BaseBackend backend
     ,MonadIO m
     ,PersistEntity record
     ,PersistStoreRead backend, PersistQueryRead backend) => DataTable record -> Request ->  ReaderT backend m Reply

dataTableSelect (DataTable dtGlobalSearch' dtSort' dtColumnSearch' dtFilters' dtValue' dtRowId') req = do
    (totalCount, displayCount,entities) <- dataTableSelectEntities (DataTable dtGlobalSearch' dtSort' dtColumnSearch' dtFilters' dtValue' dtRowId') req
    let records     =  Prelude.map formatEntity entities
    return $ Reply {
        replyNumRecords = totalCount,
        replyNumDisplayRecords = displayCount,
        replyRecords = J.toJSON $ records,
        replyEcho = reqEcho req
    }
    where
      formatEntity entity = let rowId   =  dtRowId' entity in  let columns = Prelude.map (formatColumn entity) [colName c | c <- reqColumns req] in J.object $ [ "DT_RowId" .= rowId ] ++ columns
      formatColumn entity cn = let value = dtValue' cn entity in  cn .= value


        


