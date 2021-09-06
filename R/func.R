connect = function() {
  DBI::dbConnect(RMariaDB::MariaDB(),
                 host = "localhost",
                 group = "rs-dbi",   # in ~/.my.cnf
                 dbname = "dlf")
}

get_fields = function(x) RMariaDB::dbGetQuery(connect(), paste("describe",x))[,1:2]

get_mode <- function(x) {
  uniq <- unique(x)
  uniq[which.max(tabulate(match(x, uniq)))]
}

mk_case <- function() {
  case <- glue(skinny$metro[1],
               space,as.character(skinny$otype[1]),
               space,as.character(skinny$purpose[1]),
               space,as.character(skinny$ptype[1]),
               space,as.character(skinny$ltype[1],
                                  nrow(skinny)))
}

mk_query_str = function(x,y) glue::glue( "SELECT * FROM x where deal = 'LBMLT 2006-",as.character(y),"'")


mk_skinny_factors <- function() {
  skinny$dtype <- as.factor(skinny$dtype)
  skinny$otype <- as.factor(skinny$otype)
  skinny$purpose <- as.factor(skinny$purpose)
  skinny$status <- as.factor(skinny$status)
}

mk_skinny_status <- function() {
  skinny <- skinny %>% mutate(status = case_when(
    ctapeno %in% drops        ~ "payoff",
    ctapeno %in% perf$ctapeno ~ "current",
    ctapeno %in% dlq$ctapeno  ~ "default"
  ))
}

stratify_cont <- function(w,x,y) tableone::CreateContTable(vars = w$contVars, strata = y, data = x)

stratify_cat <- function(w,x,y) tableone::CreateCatTable(vars = w$catVars, strata = y, data = x)

stratify_int <- function(w,x,y) tableone::CreateCatTable(vars = w$intVars, strata = y, data = x)

stratify_dte <- function(w,x,y) tableone::CreateCatTable(vars = w$dteVars, strata = y, data = x)
