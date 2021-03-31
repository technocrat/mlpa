connect = function() {
  DBI::dbConnect(RMariaDB::MariaDB(),
                 host = "localhost",
                 group = "rs-dbi",   # in ~/.my.cnf
                 dbname = "dlf")
}

get_fields = function(x) RMariaDB::dbGetQuery(connect(), paste("describe",x))[,1:2]

mk_query_str = function(x) glue::glue( "SELECT * FROM y7 where deal = 'LBMLT 2006-",as.character(x),"'")

query = function(x) RMariaDB::dbGetQuery(connect(), mk_query_str(x))

stratify_cont <- function(w,x,y) tableone::CreateContTable(vars = w$contVars, strata = y, data = x)

stratify_cat <- function(w,x,y) tableone::CreateCatTable(vars = w$catVars, strata = y, data = x)

stratify_int <- function(w,x,y) tableone::CreateCatTable(vars = w$intVars, strata = y, data = x)

stratify_dte <- function(w,x,y) tableone::CreateCatTable(vars = w$dteVars, strata = y, data = x)

