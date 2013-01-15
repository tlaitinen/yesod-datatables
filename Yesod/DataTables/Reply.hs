module Yesod.DataTables.Reply (Reply(..), formatReply) where
import Prelude
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Aeson as J

data Reply = Reply {
    -- |Total records, before filtering 
    -- (i.e. the total number of records in the database).
    replyNumRecords          :: Int64,

    -- |Total records, after filtering (i.e. the total number of records 
    -- after filtering has been applied - not just the number of records 
    -- being returned in this result set).                             
    replyNumDisplayRecords   :: Int64,

    -- |An array of JSON objects, one for each record.
    replyRecords             :: J.Value,

    -- |An unaltered copy of 'sEcho' sent from the client side. 
    replyEcho                :: Int64
} deriving (Eq)

formatReply :: Reply -> L.ByteString
formatReply reply = encode $ J.object [
        "iTotalRecords" .= replyNumRecords reply,
        "iTotalDisplayRecords" .= replyNumDisplayRecords reply,
        "sEcho" .= replyEcho reply,
        "aaData" .= replyRecords reply
    ]
