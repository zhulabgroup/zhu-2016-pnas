# Response surface analysis
env.stat <- env.dat %>%
  group_by(var) %>%
  summarize(
    min = min(env, na.rm = T), max = max(env, na.rm = T), rng = diff(range(env, na.rm = T)),
    lwr = quantile(env, .25, na.rm = T), upr = quantile(env, .75, na.rm = T),
    mean = mean(env, na.rm = T), sd = sd(env, na.rm = T)
  ) %>%
  as.data.frame()
rownames(env.stat) <- env.stat$var
env.stat <- as.matrix(env.stat[, -1])

env.grid <- expand.grid(
  tmp.act = seq(env.stat["tmp", "min"] - .05 * env.stat["tmp", "rng"], env.stat["tmp", "max"] + .05 * env.stat["tmp", "rng"], len = 100),
  ppt.act = seq(env.stat["ppt", "min"] - .05 * env.stat["ppt", "rng"], env.stat["ppt", "max"] + .05 * env.stat["ppt", "rng"], len = 100),
  co2.act = c(env.stat["co2", "lwr"], env.stat["co2", "upr"]),
  ntg.act = c(env.stat["ntg", "lwr"], env.stat["ntg", "upr"])
) %>%
  mutate(
    tmp.std = (tmp.act - env.stat["tmp", "mean"]) / env.stat["tmp", "sd"],
    ppt.std = (ppt.act - env.stat["ppt", "mean"]) / env.stat["ppt", "sd"],
    co2.std = (co2.act - env.stat["co2", "mean"]) / env.stat["co2", "sd"],
    ntg.std = (ntg.act - env.stat["ntg", "mean"]) / env.stat["ntg", "sd"],
    f03 = "Amb", b11 = "Amb"
  )

lbio.fit <- ldply(lme.fit, predict, env.grid, re.form = ~0)
rownames(lbio.fit) <- lbio.fit$grp
lbio.fit <- t(lbio.fit[, -1])

pred.surf <- cbind(env.grid, lbio.fit) %>%
  as_tibble() %>%
  gather(grp, lbio, contains("NPP")) %>%
  mutate(
    bio = exp(lbio),
    co2 = factor(co2.act, levels = quantile(co2.act, c(.25, .75)), labels = c("ambient CO2", "elevated CO2")),
    ntg = factor(ntg.act, levels = quantile(ntg.act, c(.75, .25)), labels = c("elevated nitrogen", "ambient nitrogen"))
  )

obs.surf <- select(lme.dat, yr, plt, grp, bio, tmp.act = tmp, ppt.act = ppt) %>%
  full_join(select(trt.dat, yr, plt, co2, ntg)) %>%
  mutate(
    co2 = factor(co2, 0:1, c("ambient CO2", "elevated CO2")),
    ntg = factor(ntg, 0:1, c("ambient nitrogen", "elevated nitrogen"))
  ) %>%
  group_by(grp, yr, tmp.act, ppt.act, co2, ntg) %>%
  summarize(bio = mean(bio, na.rm = T))

grp.sel <- "NPP" # change to ANPP, BNPP for others
ggplot(
  filter(pred.surf, grp == grp.sel),
  aes(tmp.act, ppt.act, z = bio, fill = bio)
) +
  geom_tile() +
  scale_fill_gradientn(colours = col.pal) +
  stat_contour(col = "gray") +
  geom_point(data = filter(obs.surf, grp == grp.sel), aes(size = bio), shape = 1) +
  scale_radius() +
  geom_vline(xintercept = 9.94, col = "gray", linetype = "dashed") +
  geom_hline(yintercept = 590, col = "gray", linetype = "dashed") +
  facet_wrap(ntg ~ co2, labeller = labeller(.multi_line = F)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    x = expression(Temperature ~ (degree * C)),
    y = "Precipitation (mm)", title = grp.sel,
    fill = expression(Modeled ~ (g ~ m^-2 ~ yr^-1)),
    size = expression(Observed ~ (g ~ m^-2 ~ yr^-1))
  )
ggsave(paste0(out.dir, "3 - Surf NPP.pdf"), useDingbats = F, w = 8, h = 6)

grp.sel <- "ANPP"
ggplot(
  filter(pred.surf, grp == grp.sel),
  aes(tmp.act, ppt.act, z = bio, fill = bio)
) +
  geom_tile() +
  scale_fill_gradientn(colours = col.pal) +
  stat_contour(col = "gray") +
  geom_point(data = filter(obs.surf, grp == grp.sel), aes(size = bio), shape = 1) +
  scale_radius() +
  geom_vline(xintercept = 9.94, col = "gray", linetype = "dashed") +
  geom_hline(yintercept = 590, col = "gray", linetype = "dashed") +
  facet_wrap(ntg ~ co2, labeller = labeller(.multi_line = F)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    x = expression(Temperature ~ (degree * C)),
    y = "Precipitation (mm)", title = grp.sel,
    fill = expression(Modeled ~ (g ~ m^-2 ~ yr^-1)),
    size = expression(Observed ~ (g ~ m^-2 ~ yr^-1))
  )
ggsave(paste0(out.dir, "S - Surf ANPP.pdf"), useDingbats = F, w = 8, h = 6)

grp.sel <- "BNPP" # change to ANPP, BNPP for others
ggplot(
  filter(pred.surf, grp == grp.sel),
  aes(tmp.act, ppt.act, z = bio, fill = bio)
) +
  geom_tile() +
  scale_fill_gradientn(colours = col.pal) +
  stat_contour(col = "gray") +
  geom_point(data = filter(obs.surf, grp == grp.sel), aes(size = bio), shape = 1) +
  scale_radius() +
  geom_vline(xintercept = 9.94, col = "gray", linetype = "dashed") +
  geom_hline(yintercept = 590, col = "gray", linetype = "dashed") +
  facet_wrap(ntg ~ co2, labeller = labeller(.multi_line = F)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    x = expression(Temperature ~ (degree * C)),
    y = "Precipitation (mm)", title = grp.sel,
    fill = expression(Modeled ~ (g ~ m^-2 ~ yr^-1)),
    size = expression(Observed ~ (g ~ m^-2 ~ yr^-1))
  )
ggsave(paste0(out.dir, "S - Surf BNPP.pdf"), useDingbats = F, w = 8, h = 6)
