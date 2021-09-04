source(here::here("R/pool.R"))


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

skinny <- modal %>% filter(ltype == '2/38 LIBOR')

# describe subset

case <- glue("LAX AP OO Purchase SF 2/38 LIBOR, N:",nrow(skinny))



# clean up intermediate objects

rm(d7, drops, t7, t6, liq, perf)

p1 <- ggplot(skinny, aes(sbal)) +
  ggtitle(case) +
  geom_histogram(color = "black", fill = "lightblue" ) +
  theme_minimal()

p2 <- ggplot(skinny, aes(fico)) +
  ggtitle(case) +
  geom_histogram(color =  "black" , fill =  "lightblue" ) +
  theme_minimal()

p3 <- ggplot(skinny, aes(dti)) +
  ggtitle(case) +
  geom_histogram(color =  "black" , fill =  "lightblue" ) +
  theme_minimal()

p1 / p2 / p3


mod <- glm(status ~ fico, data = skinny)
summary(mod)

res_aov <- aov(fico ~ status, data = skinny)

resids <- data.frame(.resid = res_aov$residuals)

p4 <- ggplot(resids, aes(.resid)) +
  geom_histogram(color =  black , fill =  grey ) +
  theme_minimal()

p5 <- ggplot(resids, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal()

p4 + p5

shapiro.test(res_aov$residuals)

leveneTest(fico ~ status, data = skinny)

aggregate(fico ~ status,
  data = skinny,
  function(x) round(c(mean = mean(x), sd = sd(x)), 2)
)

group_by(skinny, status) %>%
  summarise(
    mean = mean(fico, na.rm = TRUE),
    sd = sd(fico, na.rm = TRUE)
  )

oneway.test(fico ~ status,
  data = skinny,
  var.equal = TRUE # assuming equal variances
)

res_aov <- aov(fico ~ status,
  data = skinny
)

summary(res_aov)

oneway.test(fico ~ dtype,
  data = skinny,
  var.equal = FALSE # assuming unequal variances
)

post_test <- glht(res_aov,
  linfct = mcp(status =  Tukey )
)

summary(post_test)

# par(mar = c(3, 8, 3, 3))
plot(post_test)

TukeyHSD(res_aov)

plot(TukeyHSD(res_aov))

# Dunnett's test:
post_test <- glht(res_aov,
  linfct = mcp(dtype =  Dunnett )
)

summary(post_test)

# par(mar = c(3, 8, 3, 3))
plot(post_test)

skinny$status <- relevel(skinny$status, ref =  current )
levels(skinny$status)

res_aov2 <- aov(fico ~ status,
  data = skinny
)

# Dunnett's test:
post_test <- glht(res_aov2,
  linfct = mcp(status =  Dunnett )
)

post_test
# par(mar = c(3, 8, 3, 3))
plot(post_test)

pairwise.t.test(skinny$fico, skinny$dtype,
  p.adjust.method =  holm
)

# Edit from here
x <- which(names(skinny) ==  status ) # name of grouping variable
y <- which(
  names(skinny) ==  fico  # names of variables to test
)
method1 <-  anova  # one of  anova  or  kruskal.test
method2 <-  t.test  # one of  wilcox.test  or  t.test
my_comparisons <- list(c( current ,  default ), c( current ,  payoff ), c( default ,  payoff )) # comparisons for post-hoc tests
# Edit until here

# Edit at your own risk

for (i in y) {
  for (j in x) {
    p <- ggboxplot(skinny,
      x = colnames(skinny[j]), y = colnames(skinny[i]),
      color = colnames(skinny[j]),
      legend =  none ,
      palette =  npg ,
      add =  jitter
    )
    print(
      p + stat_compare_means(aes(label = paste0(..method..,  , p-value =  , ..p.format..)),
        method = method1, label.y = max(skinny[, i], na.rm = TRUE)
      )
      + stat_compare_means(comparisons = my_comparisons, method = method2, label =  p.format ) # remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
    )
  }
}

ggplot(skinny) +
  aes(x = status, y = fico, color = status) +
  geom_jitter(width = 0.25, size = 2) +
  theme(legend.position =  none ) +
  theme_minimal()

