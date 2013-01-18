module Yesod.DataTables.Reply (Reply(..), formatReply) where
import Prelude
import qualified Data.ByteString.Lazy as L
import Data.Aeson as J

data Reply = Reply {
    -- |Total records, before filtering 
    -- (i.e. the total number of records in the database).
    replyNumRecords          :: Int,

    -- |Total records, after filtering (i.e. the total number of records 
    -- after filtering has been applied - not just the number of records 
    -- being returned in this result set).                             
    replyNumDisplayRecords   :: Int,

    -- |An array of JSON objects, one for each record.
    replyRecords             :: J.Value,

    -- |An unaltered copy of 'sEcho' sent from the client side. 
    replyEcho                :: Int
} deriving (Eq, Show)

-- |Translates the reply object to a JSON value that DataTables javascript
-- plugin expects.
formatReply :: Reply -> J.Value
formatReply reply = J.object [
        "iTotalRecords" .= replyNumRecords reply,
        "iTotalDisplayRecords" .= replyNumDisplayRecords reply,
        "sEcho" .= replyEcho reply,
        "aaData" .= replyRecords reply
    ]
