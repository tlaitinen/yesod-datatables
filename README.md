yesod-datatables
=============

DataTables (http://datatables.net) is a capable plugin for jQuery Javascript
library. This Haskell library contains routines for implementing server-side
processing (request parsing, database querying, and response formatting) for
DataTables with Yesod platform. 

See the example at http://yesod-datatables-example.herokuapp.com .

The relevant bits are in:
 * [Handler](yesod-datatables/blob/master/example/Handler/Home.hs)
 * [hamlet-template](yesod-datatables/blob/master/example/templates/dataTablesWidget.hamlet)
 * [julius-template](yesod-datatables/blob/master/example/templates/dataTablesWidget.julius)

The reference documentation is available in [Hackage](http://hackage.haskell.org/package/yesod-datatables)
