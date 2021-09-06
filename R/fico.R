source(here::here("R/skin.R"))

# NOW do fico ~ dtype, fico ~ margin, fico ~ loan group
# then sbal ~ and dti ~ cltv ~
########################################################################

subdeck <- "fico ~ status residuals"

res_aov <- aov(fico ~ status, data = skinny)

resids <- data.frame(.resid = res_aov$residuals)

p1 <- ggplot(resids, aes(.resid)) +
  labs(title = case, subtitle = subdeck) +
  geom_histogram(color =  "black" , fill =  "grey" ) +
  theme_minimal()

p2 <- ggplot(resids, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal()

p1+ p2

# normality of residuals assumed
shapiro.test(res_aov$residuals)

# homogeneity of variance across groups
# null is that variance across groups is null
leveneTest(fico ~ status, data = skinny)

# Conclusion, assumptions satisfied

summary(res_aov)

oneway.test(fico ~ status,
            data = skinny,
            var.equal = FALSE # assuming uequal variances
)

# Conclusion, at least one group has a different mean
# TODO: Sure doesn't look like it from #s
aggregate(fico ~ status,
          data = skinny,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2)
)


post_test <- glht(res_aov,linfct = mcp(status = "Tukey"))
post-test
summary(post_test)
plot(post_test)

TukeyHSD(res_aov)

plot(TukeyHSD(res_aov))

# Dunnett's test:
post_test <- glht(res_aov,linfct = mcp(status = "Dunnett"))

summary(post_test)

plot(post_test)


# Edit from here
x <- which(names(skinny) == "status") # name of grouping variable
y <- which(
  names(skinny) == "fico" # names of variables to test
)
method1 <-  "anova"  # one of  anova  or  kruskal.test
method2 <-  "t.test"  # one of  wilcox.test  or  t.test
my_comparisons <- list(c("current","default"), c("current","payoff"), c( "default","payoff")) # comparisons for post-hoc tests
# Edit until here

# Edit at your own risk

for (i in y) {
  for (j in x) {
    p <- ggboxplot(skinny,
                   x = colnames(skinny[j]), y = colnames(skinny[i]),
                   color = colnames(skinny[j]),
                   legend = "none",
                   palette = "npg",
                   add = "jitter"
    )
    print(
      p + stat_compare_means(aes(label = paste0(..method.., ", p-value = ", ..p.format..)),
                             method = method1, label.y = max(skinny[, i], na.rm = TRUE)
      )
      # + stat_compare_means(comparisons = my_comparisons, method = method2, label = "p.format") # remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
    )
  }
}

ggplot(skinny) +
  aes(x = status, y = dtype, color = dti) +
  geom_jitter(width = 0.25, size = 2.5) +
  theme(legend.position = "none") +
  scale_fill_distiller(type = 'seq',
                       palette = 'YlOrRd',
                       direction = 1,
                       guide = 'colourbar',
                       aesthetics = 'colour') +
  theme_minimal()


ggplot(skinny) +
  aes(x = status, y = dtype, color = fico) +
  geom_jitter(width = 0.25, size = 2.5) +
  theme(legend.position = "none") +
  scale_fill_distiller(type = 'seq',
                       palette = 'YlOrRd',
                       direction = -1,
                       guide = 'colourbar',
                       aesthetics = 'colour') +
  theme_minimal()

ggplot(skinny) +
  aes(x = status, y = dtype, color = sbal) +
  geom_jitter(width = 0.25, size = 2.5) +
  theme(legend.position = "none") +
  scale_fill_distiller(type = 'seq',
                       palette = 'YlOrRd',
                       direction = 1,
                       guide = 'colourbar',
                       aesthetics = 'colour') +
  theme_minimal()

ggplot(skinny) +
  aes(x = status, y = dtype, color = cltv) +
  geom_jitter(width = 0.25, size = 2.5) +
  theme(legend.position = "none") +
  scale_fill_distiller(type = 'seq',
                       palette = 'YlOrRd',
                       direction = 1,
                       guide = 'colourbar',
                       aesthetics = 'colour') +
  theme_minimal()

pal <- c("#004080","#008000","#cc66ff")

ggplot(skinny) +
  aes(x = status, y = dtype, color = as.factor(margin)) +
  geom_jitter(width = 0.25, size = 2.5) +
  theme(legend.position = "none") +
  scale_colour_brewer(type = 'seq',
                      palette = 'Set1',
                      direction = 1,
                      aesthetics = 'colour') +
  theme_minimal()
