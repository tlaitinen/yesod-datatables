{-# LANGUAGE OverloadedStrings #-}
module Yesod.DataTables.Request (Req(..), Column(..), SortDir(..),
                                 parseRequest) where
import Prelude
import Data.Attoparsec (parse, maybeResult)
import Data.List as L
import Data.Maybe
import Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text.Encoding as E
type ParamName  = Text
type ParamValue = Text

data SortDir= SortAsc | SortDesc deriving (Eq, Show)

type ColumnId = Int

data Column = Column {
    colSearchable  :: Bool,
    colSearch      :: Text,
    colSearchRegex :: Bool,
    colSortable    :: Bool,
    colDataProp    :: Text
} deriving (Show, Eq)

data Req = Req {
    reqDisplayStart  :: Int,
    reqDisplayLength :: Int,
    reqSearch        :: Text,
    reqSearchRegex   :: Bool,
    reqColumns       :: [Column],
    reqSort          :: [(ColumnId,SortDir)],
    reqEcho          :: Int 
} deriving (Show, Eq)

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
            dataProp = do

            searchable <- readMaybe $ Just searchable'
            regex      <- readMaybe $ Just regex'
            sortable   <- readMaybe $ Just sortable'
            
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

readSortDir :: Text -> Maybe SortDir
readSortDir "asc" = Just SortAsc
readSortDir "desc" = Just SortDesc
readSortDir _ = Nothing

parseSortDir :: Text -> Text -> Maybe (ColumnId, SortDir)
parseSortDir idStr sortDir = do
    idNum <- readMaybe (Just idStr)
    dir <- readSortDir sortDir
    return (idNum, dir)


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

    let sortInfo   = catMaybes $ L.zipWith parseSortDir 
                                           sortingCols sortingColsDir

    return $ Req {
        reqDisplayStart  = displayStart,
        reqDisplayLength = displayLength,
        reqSearch        = search,
        reqSearchRegex   = regex > 0,
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
        

