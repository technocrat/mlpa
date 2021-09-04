source(here::here("R/libr.R"))
source(here::here("R/func.R"))

mk_query_str1 <- function(x) glue("SELECT * FROM y6 where deal = 'LBMLT 2006-" , as.character(x),"'")

mk_query_str2 <- function(x) glue("SELECT zzip,metro FROM zcta")

query <- function(x) RMariaDB::dbGetQuery(connect(), mk_query_str1(x))

pool <- query(1) # 2006-01 fwp tape

query <- function(x) RMariaDB::dbGetQuery(connect(), mk_query_str2(x))

zcta <- query(1)

# add metro to y6

colnames(zcta) <- c("zip", "metro")

pool <- left_join(pool, zcta, by ="zip")

mk_query_str3 <- function(x) glue( "SELECT * FROM y7 where deal = 'LBMLT 2006-" , as.character(x),"'")

query <- function(x) RMariaDB::dbGetQuery(connect(), mk_query_str3(x))

d7 <- query(1)

t7 <- d7$ctapeno
t6 <- pool$ctapeno
drops <- setdiff(t6, t7)
liq <- pool[which(pool$ctapeno %in% drops), ]
dlq <- d7[which(d7$down > 2), ]
perf <- d7[which(d7$down < 3), ]

# clean out unneeded fields

pool %<>% dplyr::select(!c(cservno,pflag,deal,pmi,duedate,scap,oterm,oservno,rdate,rterm,aterm,obal,odate,oltv,lien,second,pmon,cbal,rterm,cdate,mdate,pexp,pptype,icode,status))

# classify performance status

pool %<>% mutate(status = case_when(
  ctapeno %in% drops ~ "payoff",
  ctapeno %in% perf$ctapeno ~ "perform",
  ctapeno %in% dlq$ctapeno ~ "default"
))

# factorize

pool$ptype   <- as.factor(pool$ptype)
pool$otype   <- as.factor(pool$otype)
pool$dtype   <- as.factor(pool$dtype)
pool$purpose <- as.factor(pool$purpose)
pool$ltype   <- as.factor(pool$ltype)
pool$status  <- as.factor(pool$status)




