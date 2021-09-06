source(here::here("R/pool.R"))
source(here::here("R/cons.R"))

# find pool modes for
# grade, ltype, ptype, otype, purpose, metro, fpd

modal <- pool %>% filter(
  grade == get_mode(grade),
  ltype == get_mode(ltype),
  ptype == get_mode(ptype),
  otype == get_mode(otype),
  purpose == get_mode(purpose),
  metro == get_mode(metro)
)

pool %>% filter(metro == get_mode(metro)) %>% head(.,1)

# Occur in the same metro

modal <- pool %>% filter(metro == get_mode(metro)) %>%
  filter(
    grade == get_mode(grade),
    otype == get_mode(otype),
    purpose == get_mode(purpose),
    ptype == get_mode(ptype)
  )

head(modal,1)

# find most common loan type
get_mode(modal$ltype)
modal %>% group_by(ltype) %>% count() %>% ungroup()

skinny <- modal %>% filter(ltype == get_mode(ltype))

# title of subset for use in plots

case <- mk_case()

# remove modal variables
modal_list <- c("city","st","fpd","grade","orate","metro","otype","purpose","ptype","ltype")

skinny %<>% dplyr::select(-modal_list)

# clean up intermediate objects

rm(d7, dlq,drops, t7, t6, liq, perf, modal_list,modal,zcta)

N = nrow(skinny)

# count of loans by dtype and status

ct_dtype_n <- with(skinny, table(dtype,status))
addmargins(ct_dtype_n, margin = seq_along(dim(ct_dtype_n)))

# percent of loans by dtype and status

ct_dtype_pct <- round(with(skinny, table(dtype, status)) / N * 100,2)
addmargins(ct_dtype_pct, margin = seq_along(dim(ct_dtype_pct)))

p1 <- ggplot(skinny, aes(sbal)) +
  ggtitle(case) +
  geom_histogram(color =  "black" , fill =  "lightblue" ) +
  facet_wrap(~ status) +
  theme_minimal()

p2 <- ggplot(skinny, aes(fico)) +
  ggtitle(case) +
  geom_histogram(color =  "black" , fill =  "lightblue" ) +
  facet_wrap(~ status) +
  theme_minimal()

p3 <- ggplot(skinny, aes(dti)) +
  ggtitle(case) +
  geom_histogram(color =  "black" , fill =  "lightblue" ) +
  facet_wrap(~ status) +
  theme_minimal()

p1 / p2 / p3
