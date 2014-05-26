library(plyr)
library(reshape2)
library(GGally)
library(ggplot2)
library(mgcv)
library(nlme)
library(Hmisc)
library(gridExtra)
library(corrgram)
options(scipen = 20)
dir_kate <- "~/Dropbox/kate_termpaper"
setwd(dir_kate)
cg <- read.csv("CG.csv", header = T, sep = ";")
means_cg <- sapply(cg[, -1], mean)
sds_cg <- sapply(cg[, -1], sd)
mins_cg <- means_cg - sds_cg
maxs_cg <- means_cg + sds_cg
