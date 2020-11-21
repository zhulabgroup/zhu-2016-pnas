# Model assessment
chk.dat <- NULL
for (grp.sel in grp.nm) {
  chk.dat <- lme.dat %>%
    filter(grp == grp.sel) %>%
    mutate(lobs = log(bio)) %>%
    select(yr, plt, tmp.std, ppt.std, co2.std, ntg.std, f03, b11, grp, lobs) %>%
    mutate(lfit = predict(lme.fit[[grp.sel]], .)) %>%
    mutate(lres = lobs - lfit) %>%
    rbind(chk.dat, .)
}
chk.dat <- mutate(chk.dat, grp = factor(grp, grp.nm, grp.nm))

chk.sum <- chk.dat %>%
  group_by(yr, tmp.std, ppt.std, co2.std, ntg.std, f03, b11, grp) %>%
  summarize(
    lobs.mean = mean(lobs, na.rm = T), lobs.se = std.error(lobs, na.rm = T),
    lfit.mean = mean(lfit, na.rm = T), lfit.se = std.error(lfit, na.rm = T)
  ) %>%
  mutate(
    obs.mean = exp(lobs.mean), obs.lwr = exp(lobs.mean - lobs.se), obs.upr = exp(lobs.mean + lobs.se),
    fit.mean = exp(lfit.mean), fit.lwr = exp(lfit.mean - lfit.se), fit.upr = exp(lfit.mean + lfit.se)
  )

chk.sum %>%
  group_by(grp) %>%
  summarize(cor = cor(obs.mean, fit.mean, use = "complete"))

# Observed vs. modeled
ggplot(chk.sum, aes(obs.mean, fit.mean,
                    xmin = obs.lwr, xmax = obs.upr,
                    ymin = fit.lwr, ymax = fit.upr, col = grp
)) +
  geom_errorbar() +
  geom_errorbarh() +
  geom_abline(intercept = 0, slope = 1, col = "black") +
  facet_wrap(~grp, scales = "free") +
  scale_color_manual(values = grp.col) +
  guides(col = F) +
  labs(
    x = expression(Observed ~ primary ~ production ~ (g ~ m^-2 ~ yr^-1)),
    y = expression(Modeled ~ primary ~ production ~ (g ~ m^-2 ~ yr^-1))
  )
ggsave(paste0(out.dir, "S - Obs vs. fit.pdf"), w = 11, h = 4, useDingbats = F)

# Residual diagnostics
chk.dat %>%
  ggplot(aes(factor(yr), lres, col = grp)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, col = "gray") +
  facet_wrap(~grp) +
  scale_color_manual(values = grp.col) +
  guides(col = F) +
  labs(x = "Year", y = "Residual")
ggsave(paste0(out.dir, "S - Res boxplot.pdf"), w = 11, h = 4, useDingbats = F)

chk.dat %>%
  ggplot(aes(lres, col = grp)) +
  geom_histogram(fill = "white") +
  facet_wrap(~grp) +
  xlim(c(-3, 3)) +
  scale_color_manual(values = grp.col) +
  guides(col = F) +
  labs(x = "Residual", y = "Frequency")
ggsave(paste0(out.dir, "S - Res histogram.pdf"), w = 11, h = 4, useDingbats = F)
