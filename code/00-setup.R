# load packages, set global variables, load data
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())
library(plotrix)
library(lme4)

in.dir <- "data/"
out.dir <- "figures/"

grp.nm <- c("NPP", "ANPP", "BNPP")
grp.col <- c("black", "red", "blue")
env.nm <- c("tmp", "ppt", "co2", "ntg")
env.lab <- c("Temperature~(degree*C)", "Precipitation~(mm)", "CO[2]~(mu*mol~mol^-1)", "Nitrogen~(g~m^-2~yr^-1)")
names(env.lab) <- env.nm
col.pal <- c(
  "#00008F", "#0000EA", "#0015FF", "#003DFF", "#0051FF", "#0070FF",
  "#0084FF", "#0098FF", "#00ACFF", "#00B6FF", "#00CBFF", "#00DFFF",
  "#00E9FF", "#00F4FF", "#08FFF7", "#13FFEC", "#1DFFE2", "#31FFCE",
  "#3CFFC3", "#46FFB9", "#50FFAF", "#5AFFA5", "#64FF9B", "#6FFF90",
  "#79FF87", "#8CFF73", "#96FF69", "#96FF68", "#A0FF5F", "#ABFF54",
  "#B5FF4A", "#BFFF40", "#C9FF36", "#D3FF2C", "#DEFF21", "#E7FF18",
  "#F2FF0D", "#F2FF0D", "#FDFF02", "#FFF900", "#FFED00", "#FFE300",
  "#FFE300", "#FFD900", "#FFCF00", "#FFC500", "#FFBB00", "#FFBB00",
  "#FFB000", "#FFA600", "#FFA600", "#FF9C00", "#FF9200", "#FF8800",
  "#FF8800", "#FF7F00", "#FF7400", "#FF7400", "#FF6A00", "#FF6000",
  "#FF6000", "#FF5600", "#FF4C00", "#FF4C00", "#FF4100", "#FF4100",
  "#FF3700", "#FF2D00", "#FF2D00", "#FF2300", "#FF1900", "#FF1900",
  "#FF0E00", "#FF0E00", "#FF0300", "#FF0300", "#FB0000", "#EF0000",
  "#EF0000", "#E50000", "#E50000", "#DB0000", "#DB0000", "#D00000",
  "#C60000", "#C60000", "#BC0000", "#BC0000", "#B20000", "#B20000",
  "#A80000", "#A80000", "#9E0000", "#9E0000", "#930000", "#930000",
  "#890000", "#890000", "#800000", "#800000"
)

bio.dat <- read.csv(paste0(in.dir, "Biomass.csv")) %>% as_tibble()
trt.dat <- read.csv(paste0(in.dir, "Treatment.csv")) %>% as_tibble()
env.dat <- read.csv(paste0(in.dir, "Environment.csv")) %>% as_tibble()
