{-# LANGUAGE OverloadedStrings #-}
module Yesod.DataTables.Request (Req(..), parse) where
import Prelude
import Data.Aeson as J
import Data.Attoparsec (parse, maybeResult)
import Data.List as L
import Data.Maybe
import Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text.Encoding as E
type ParamName  = Text
type ParamValue = Text

data SortInfo = SortNone | SortAsc | SortDesc deriving (Eq, Show)

type ColumnId = Int

data Column = Column {
    colSearchable  :: Bool,
    colSearch      :: Text,
    colSearchRegex :: Bool,
    colSortable    :: Bool,
    colDataProp    :: J.Value
} deriving (Show)

data Req = Req {
    reqDisplayStart  :: Int,
    reqDisplayLength :: Int,
    reqSearch        :: Text,
    reqColumns       :: [Column],
    reqRegex         :: Bool,
    reqSort          :: [(ColumnId,SortInfo)],
    reqEcho          :: Int 
} deriving (Show)

readMaybe :: (Read a) => Maybe Text -> Maybe a
readMaybe (Just s) = case reads (unpack s) of
              [(x, "")] -> Just x
              _ -> Nothing
readMaybe _ = Nothing

parseColumn ::  Text
            ->  Text
            ->  Text
            ->  Text
            ->  Text
            -> Maybe Column
parseColumn searchable'
            search
            regex'
            sortable'
            dataPropS = do

            searchable <- readMaybe $ Just searchable'
            regex      <- readMaybe $ Just regex'
            sortable   <- readMaybe $ Just sortable'
            
            dataProp <- maybeResult (parse json (BS8.pack (T.unpack dataPropS)))
            return $ Column {
                colSearchable  = searchable > 0,
                colSearch      = search,
                colSearchRegex = regex > 0,
                colSortable    = sortable > 0,
                colDataProp    = dataProp
            }
    
checkColumns :: [Maybe Column] -> Int -> Maybe [Column]
checkColumns mcolumns nColumns= let
    columns = catMaybes mcolumns
    in  
        if L.length columns == nColumns
            then Just columns
            else Nothing

readSortDir :: Text -> Maybe SortInfo
readSortDir "asc" = Just SortAsc
readSortDir "desc" = Just SortDesc
readSortDir _ = Nothing

parseSortInfo :: Text -> Text -> Maybe (ColumnId, SortInfo)
parseSortInfo idStr sortDir = do
    idNum <- readMaybe (Just idStr)
    dir <- readSortDir sortDir
    return (idNum, dir)


tester :: [(ParamName, ParamValue)] -> Maybe Text
tester params = do
    param "sSearch"
    where
        param :: ParamName -> Maybe ParamValue
        param key = lookup key params
        manyParams :: ParamName -> Int -> Maybe [ParamValue]
        manyParams key num = let
            values = catMaybes $ L.map param
                                 [ T.concat [key, pack $ show n] 
                                   | n <- [0..num-1] ]
            numValues = L.length values
            in
                if numValues == num 
                    then Just values
                    else Nothing  

parseRequest :: [(ParamName, ParamValue)] -> Maybe Req
parseRequest params = do
    displayStart   <- readMaybe $ param "iDisplayStart" 
    displayLength  <- readMaybe $ param "iDisplayLength"
    nColumns       <- readMaybe $ param "iColumns"
    search         <- param "sSearch"
    regex          <- readMaybe $ param "bRegex"
    cSearchable    <- manyParams "bSearchable_" nColumns 
    cSearch        <- manyParams "bSearch_" nColumns
    cRegex         <- manyParams "bRegex_" nColumns
    cSortable      <- manyParams "bSortable_" nColumns
    cDataProp      <- manyParams "mDataProp_" nColumns

    let columnData = L.zipWith5 parseColumn 
                               cSearchable
                               cSearch
                               cRegex
                               cSortable
                               cDataProp
    columns        <- checkColumns columnData nColumns

    nSortingCols   <- readMaybe $ param "iSortingCols"

    sortingCols    <- manyParams "iSortCol_" nSortingCols
    sortingColsDir <- manyParams "sSortDir_" nSortingCols
    echo           <- readMaybe $ param "sEcho"

    let sortInfo   = catMaybes $ L.zipWith parseSortInfo 
                                           sortingCols sortingColsDir

    return $ Req {
        reqDisplayStart  = displayStart,
        reqDisplayLength = displayLength,
        reqSearch        = search,
        reqRegex         = regex > 0,
        reqColumns       = columns,
        reqSort          = sortInfo,
        reqEcho          = echo
    }
    where
        param :: ParamName -> Maybe ParamValue
        param key = lookup key params
        manyParams :: ParamName -> Int -> Maybe [ParamValue]
        manyParams key num = let
            values = catMaybes $ L.map param
                                 [ T.concat [key, pack $ show n] 
                                   | n <- [0..num-1] ]
            numValues = L.length values
            in
                if numValues == num 
                    then Just values
                    else Nothing  
        

