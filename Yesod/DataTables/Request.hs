{-# LANGUAGE OverloadedStrings #-}
module Yesod.DataTables.Request (Request(..), Column(..), ColumnName, SortDir(..),
                                 parseRequest) where
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

-- | enum for sSortDir_(int) 
data SortDir = SortAsc | SortDesc deriving (Eq, Show)

type ColumnName = Text

-- | information about grid column 
data Column = Column {
    -- | whether searching is enabled at client-side
    colSearchable  :: Bool,

    -- | column-specific search query  
    colSearch      :: Text,

    -- | whether search query should be interpreted as a regular expression
    colSearchRegex :: Bool,

    -- | whether sorting is enabled at client-side
    colSortable    :: Bool,

    -- | column name (client-side also expects the data in a field with the
    -- same name 
    colName        :: Text
} deriving (Show, Eq)


-- | DataTables grid server-side request
-- (see http://datatables.net/usage/server-side)
data Request = Request {
    -- | Display start point in the current data set.
    reqDisplayStart  :: Int,

    -- | Number of records that the table can display in the current draw. It is expected that the number of records returned will be equal to this number, unless the server has fewer records to return.
    reqDisplayLength :: Int,

    -- | Global search field
    reqSearch        :: Text,

    -- | True if the global filter should be treated as a regular expression for advanced filtering, false if not.
    reqSearchRegex   :: Bool,

    -- | columns that the client-side knows about
    reqColumns       :: [Column],

    -- | result set sorting instructions 
    reqSort          :: [(ColumnName,SortDir)],

    -- | Information for DataTables to use for rendering (do not alter).
    reqEcho          :: Int
} deriving (Show, Eq)

readMaybe :: (Read a) => Maybe Text -> Maybe a
readMaybe (Just s) = case reads (unpack s) of
              [(x, "")] -> Just x
              _ -> Nothing
readMaybe _ = Nothing

readBool :: Maybe Text -> Maybe Bool
readBool (Just "true") = Just True
readBool (Just "false") = Just False
readBool _ = Nothing

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

            searchable <- readBool $ Just searchable'
            regex      <- readBool $ Just regex'
            sortable   <- readBool $ Just sortable'
            
            return $ Column {
                colSearchable  = searchable,
                colSearch      = search,
                colSearchRegex = regex,
                colSortable    = sortable,
                colName        = dataProp
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

parseSortDir :: [Column] -> Text -> Text -> Maybe (ColumnName, SortDir)
parseSortDir columns idStr sortDir = do
    idNum <- readMaybe (Just idStr)
    name <- maybeColumnName idNum
    dir <- readSortDir sortDir
    return (name, dir)
    where
        maybeColumnName colId 
            | colId < 0 = Nothing
            | colId >= L.length columns = Nothing
            | otherwise = Just $ colName (columns !! colId)


-- | Tries to parse DataTables request
parseRequest :: [(ParamName, ParamValue)] -> Maybe Request
parseRequest params = do
    displayStart   <- readMaybe $ param "iDisplayStart" 
    displayLength  <- readMaybe $ param "iDisplayLength"
    nColumns       <- readMaybe $ param "iColumns"
    search         <- param "sSearch"
    regex          <- readBool $ param "bRegex"
    cSearchable    <- manyParams "bSearchable_" nColumns 
    cSearch        <- manyParams "sSearch_" nColumns
    cRegex         <- manyParams "bRegex_" nColumns
    cSortable      <- manyParams "bSortable_" nColumns
    cName          <- manyParams "mDataProp_" nColumns
    let columnData = L.zipWith5 parseColumn 
                               cSearchable
                               cSearch
                               cRegex
                               cSortable
                               cName
    columns        <- checkColumns columnData nColumns

    nSortingCols   <- readMaybe $ param "iSortingCols"

    sortingCols    <- manyParams "iSortCol_" nSortingCols
    sortingColsDir <- manyParams "sSortDir_" nSortingCols

    echo           <- readMaybe $ param "sEcho"

    let sortInfo   = catMaybes $ L.zipWith (parseSortDir columns)
                                           sortingCols sortingColsDir

    return $ Request {
        reqDisplayStart  = displayStart,
        reqDisplayLength = displayLength,
        reqSearch        = search,
        reqSearchRegex   = regex,
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
        

