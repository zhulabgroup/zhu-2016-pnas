# Exploratory analysis
# ANOVA
aov.dat <- full_join(trt.dat, bio.dat) %>%
  filter(complete.cases(.)) %>%
  dlply(
    .(grp, yr),
    function(dat) {
      lm(log(bio) ~ (tmp + ppt + co2 + ntg)^4, data = dat)
    }
  ) %>%
  ldply(function(obj) {
    lm.coef <- coef(obj)
    lm.pval <- summary(obj)$coefficients[, "Pr(>|t|)"]
    tibble(coef = names(lm.coef), est = lm.coef, pval = lm.pval)
  }) %>%
  as_tibble()

coef.nm <- c(
  "(Intercept)", "tmp", "ppt", "co2", "ntg",
  "tmp:ppt", "tmp:co2", "tmp:ntg", "ppt:co2", "ppt:ntg", "co2:ntg",
  "tmp:ppt:co2", "tmp:ppt:ntg", "tmp:co2:ntg", "ppt:co2:ntg",
  "tmp:ppt:co2:ntg"
)
coef.lab <- c(
  "I", "T", "P", "C", "N",
  "TP", "TC", "TN", "PC", "PN", "CN",
  "TPC", "TPN", "TCN", "PCN",
  "TPCN"
)

aov.dat %>%
  mutate(
    grp = factor(grp, grp.nm, grp.nm),
    coef = factor(coef, coef.nm, coef.lab)
  ) %>%
  filter(coef != "I") %>%
  ggplot(aes(yr, est, group = grp, col = grp, alpha = ifelse(pval < .05, 1, .5))) +
  geom_point(aes(shape = grp), position = position_dodge(width = .5)) +
  scale_shape(solid = FALSE) +
  geom_hline(yintercept = 0, col = "gray") +
  facet_wrap(~coef) +
  scale_color_manual(values = grp.col) +
  guides(alpha = F, shape = F) +
  labs(x = "Year", y = "ANOVA coefficient", col = "Group")
ggsave(paste0(out.dir, "S - ANOVA.pdf"), w = 10, h = 8, useDingbats = F)

# Pairwise plots
eda.dat <- trt.dat %>%
  select(yr, plt, tmp, ppt, co2, ntg) %>%
  gather(var, trt, -yr, -plt) %>%
  mutate(var = factor(var)) %>%
  full_join(env.dat) %>%
  full_join(bio.dat) %>%
  filter(complete.cases(.)) %>%
  mutate(
    lbio = log(bio),
    grp = factor(grp, grp.nm, grp.nm),
    trt = factor(trt, 0:1, c("Ambient", "Elevated")),
    var = factor(var, env.nm, env.lab)
  ) %>%
  group_by(yr, var, trt, grp) %>%
  summarize(env = mean(env), lbio.mean = mean(lbio, na.rm = T), lbio.se = std.error(lbio, na.rm = T)) %>%
  mutate(bio.mean = exp(lbio.mean), bio.upper = exp(lbio.mean + lbio.se), bio.lower = exp(lbio.mean - lbio.se)) %>%
  select(yr, var, trt, env, grp, bio.mean, bio.upper, bio.lower)

ggplot(eda.dat, aes(env, bio.mean, col = grp)) +
  geom_point(aes(shape = trt), alpha = .5) +
  scale_shape_manual(values = c(1, 17)) +
  geom_linerange(aes(ymin = bio.lower, ymax = bio.upper), alpha = .3) +
  geom_smooth(data = filter(eda.dat, var %in% env.lab[c("tmp", "ppt")]), method = "lm", formula = y ~ poly(x, 2), se = F, alpha = .5, fullrange = T) +
  geom_smooth(data = filter(eda.dat, var %in% env.lab[c("co2", "ntg")]), method = "lm", formula = y ~ x, se = F, alpha = .5, fullrange = T) +
  facet_wrap(~var, ncol = 2, scales = "free_x", labeller = label_parsed) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)) +
  annotation_logticks(sides = "l") +
  labs(
    x = "", y = expression(Primary ~ production ~ (g ~ m^-2 ~ yr^-1)),
    col = "Group", shape = "Treatment"
  ) +
  scale_color_manual(values = grp.col)
ggsave(paste0(out.dir, "1 - EDA.pdf"), w = 8, h = 7)
