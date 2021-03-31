
# convert decimal representation of payment history to string
# binary(2526)
badun <- "1{3}$"
str_detect(binary(2495), badun)
fpd <- ymd("2005-10-01")
fpd + months(3)
# predict default at last payment date
ggpairs(res[, 2:5])

CreateTableOne(colnames(res), strata = c("ltype"), data = res)

# To find: How is FICO gamed?

p <- ggplot(res, aes(sample = sbal))
p + stat_qq() + stat_qq_line() +
  facet_wrap(~deal) + theme_minimal()

# Use fitdistr from MASS to estimate distribution params
params <- as.list(MASS::fitdistr(res$fico, "t")$estimate)

ggplot(res, aes(sample = fico)) +
  stat_qq(distribution = qt, dparams = params["df"]) +
  stat_qq_line(distribution = qt, dparams = params["df"]) +
  facet_wrap(~deal) +
  theme_minimal()

# null is that there is no difference in variance
leveneTest(res$fico, res$deal)

ggplot(res, aes(fico, colour = deal)) +
  geom_density() +
  facet_wrap(~deal) +
  theme_minimal()

ggplot(res, aes(fico, colour = deal)) +
  geom_boxplot() +
  facet_wrap(~deal) +
  coord_flip() +
  theme_minimal()

res %>%
  group_by(ltype) %>%
  describe() %>%
  select(ltype, n, se_mean, IQR, skewness, kurtosis)

# no difference in the degree of normality
res %>%
  group_by(ltype) %>%
  normality(fico) %>%
  arrange(desc(p_value))

p1 <- ggplot(res) +
  aes(x = ltype, y = fico, color = deal) +
  geom_jitter() +
  theme(legend.position = "none") +
  theme_minimal()

p2 <- ggplot(res) +
  aes(x = ltype, y = fico, color = deal) +
  geom_violin() +
  theme(legend.position = "none") +
  theme_minimal()

p3 <- ggplot(res) +
  aes(fico, fill = ltype) +
  geom_dotplot(method = "histodot", binwidth = 1.5) +
  theme(legend.position = "none") +
  theme_minimal()

p1 + p2 + p3

res_aov <- aov(fico ~ ltype, data = res)

resids <- data.frame(.resid = res_aov$residuals)

# histogram
p4 <- ggplot(resids, aes(.resid)) +
  geom_histogram(color = "black", fill = "grey") +
  theme_minimal()

p5 <- ggplot(resids, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal()

p4 + p5
shapiro.test(res_aov$residuals)
leveneTest(fico ~ deal, data = res)

aggregate(fico ~ ltype,
  data = res,
  function(x) round(c(mean = mean(x), sd = sd(x)), 2)
)

group_by(res, deal) %>%
  summarise(
    mean = mean(fico, na.rm = TRUE),
    sd = sd(fico, na.rm = TRUE)
  )


oneway.test(flipper_length_mm ~ species,
  data = dat,
  var.equal = TRUE # assuming equal variances
)


oneway.test(fico ~ deal,
  data = res,
  var.equal = TRUE # assuming equal variances
)

res_aov <- aov(fico ~ deal,
  data = res
)

summary(res_aov)

oneway.test(fico ~ deal,
  data = res,
  var.equal = FALSE # assuming unequal variances
)

post_test <- glht(res_aov,
  linfct = mcp(deal = "Tukey")
)

summary(post_test)

par(mar = c(3, 8, 3, 3))
plot(post_test)

TukeyHSD(res_aov)
plot(TukeyHSD(res_aov))

post_test <- glht(res_aov,
  linfct = mcp(deal = "Dunnett")
)

summary(post_test)
par(mar = c(3, 8, 3, 3))
plot(post_test)

# Change reference category:
res$deal <- relevel(res$deal, ref = "LBMLT 2006-1")
levels(res$deal)

res_aov2 <- aov(fico ~ deal,
  data = res
)

post_test <- glht(res_aov2,
  linfct = mcp(deal = "Dunnett")
)

summary(post_test)
par(mar = c(3, 8, 3, 3))
plot(post_test)


chisq.test(res$deal, res$fico)

m <- svm(species ~ ., train)

predRes2 <- predict(m, test)
test$predRes2 <- predRes2

ggplot(res, aes(x = deal, y = fico, color = color)) +
  geom_jitter(size = 3) +
  scale_color_manual(values = pCol)

table(test$species, test$predRes2)
