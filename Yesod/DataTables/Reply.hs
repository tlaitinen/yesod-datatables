module Yesod.DataTables.Reply (Reply(..), formatReply) where
import Prelude
import qualified Data.ByteString.Lazy as L
import Data.Int
import qualified Yesod.DataTables.Request as R
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
    replyRecords    :: J.Value
} deriving (Eq)

formatReply :: R.Req -> Reply -> L.ByteString
formatReply request reply = encode $ J.object [
        "iTotalRecords" .= replyNumRecords reply,
        "iTotalDisplayRecords" .= replyNumDisplayRecords reply,
        "sEcho" .= R.reqEcho request,
        "aaData" .= replyRecords reply
    ]
