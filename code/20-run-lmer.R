# Joint modeling, run linear mixed effects model
lme.dat <- trt.dat %>%
  gather(var, trt, -yr, -plt, -f03, -b11) %>%
  mutate(var = factor(var)) %>%
  inner_join(env.dat) %>%
  select(-trt) %>%
  spread(var, env) %>%
  full_join(bio.dat) %>%
  mutate(tmp.std = scale(tmp), ppt.std = scale(ppt), co2.std = scale(co2), ntg.std = scale(ntg)) %>% # standardize environmental covariates
  mutate(
    f03 = factor(f03, c("No", "Yes", "Amb"), c("No", "Yes", "Amb")),
    b11 = factor(b11, c("No", "Yes", "Amb"), c("No", "Yes", "Amb"))
  ) %>%
  select(
    yr, plt,
    tmp, ppt, co2, ntg,
    tmp.std, ppt.std, co2.std, ntg.std,
    f03, b11, grp, bio
  )

lme.fit <- lme.dat %>%
  dlply(
    .(grp),
    function(dat) {
      lmer(log(bio) ~ I(tmp.std^2) + I(ppt.std^2) +
             (tmp.std + ppt.std + co2.std + ntg.std)^4 + f03 + b11 + # fixed effect
             (1 | plt), # random effect
           data = dat
      )
    }
  )

# model comparison: linear vs. nonlinear
lme.fit2 <- lme.dat %>%
  dlply(
    .(grp),
    function(dat) {
      lmer(log(bio) ~ (tmp.std + ppt.std + co2.std + ntg.std)^4 + f03 + b11 + # fixed effect
             (1 | plt), # random effect
           data = dat
      )
    }
  )

ldply(lme.fit, extractAIC) # AIC (2nd term) for nonlinear model
ldply(lme.fit2, extractAIC) # AIC for linear model

# Coefficient plot
coef.nm <- c(
  "I(tmp.std^2)", "I(ppt.std^2)",
  "tmp.std", "ppt.std", "co2.std", "ntg.std",
  "tmp.std:ppt.std", "tmp.std:co2.std", "tmp.std:ntg.std", "ppt.std:co2.std", "ppt.std:ntg.std", "co2.std:ntg.std",
  "tmp.std:ppt.std:co2.std", "tmp.std:ppt.std:ntg.std", "tmp.std:co2.std:ntg.std", "ppt.std:co2.std:ntg.std",
  "tmp.std:ppt.std:co2.std:ntg.std", "f03Yes", "b11Yes"
)
coef.lab <- c(
  "T2", "P2", "T", "P", "C", "N",
  "TP", "TC", "TN", "PC", "PN", "CN",
  "TPC", "TPN", "TCN", "PCN",
  "TPCN", "F03", "B11"
)
coef.dat <- NULL
for (grp in grp.nm) {
  coef.dat <- rbind(coef.dat, data.frame(grp, fixef(lme.fit[[grp]])[coef.nm], confint(lme.fit[[grp]])[coef.nm, ]))
}
colnames(coef.dat) <- c("grp", "mean", "lwr", "upr")
coef.dat <- coef.dat %>%
  cbind(coef = coef.nm, .) %>%
  as_tibble() %>%
  mutate(coef = factor(coef, coef.nm, coef.lab)) %>%
  mutate(sig = ifelse(lwr * upr > 0, T, F)) # significant or not

coef.dat %>%
  filter(coef != "F03", coef != "B11") %>%
  ggplot(aes(coef, mean, ymin = lwr, ymax = upr, col = grp, alpha = ifelse(sig, 1, .5))) +
  geom_pointrange(position = position_dodge(width = .5)) +
  geom_hline(yintercept = 0, col = gray(.5)) +
  labs(y = "Standardized coefficient", x = "", col = "Group") +
  guides(alpha = F) +
  scale_color_manual(values = grp.col)
ggsave(paste0(out.dir, "2 - Coef.pdf"), w = 10, h = 8, useDingbats = F)

n.dig <- 3
coef.dat %>%
  mutate(val = paste0(
    formatC(round(mean, n.dig), format = "f", digits = n.dig),
    " (",
    formatC(round(lwr, n.dig), format = "f", digits = n.dig),
    ", ",
    formatC(round(upr, n.dig), format = "f", digits = n.dig),
    ")"
  )) %>%
  select(coef, grp, val) %>%
  spread(grp, val) %>%
  write.csv(file = paste0(out.dir, "S1 - Coef.csv"), row.names = F)
