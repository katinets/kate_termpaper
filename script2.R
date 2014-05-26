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
dir_graphs <- file.path(dir_kate, "graphs")
dir.create(dir_graphs, showWarnings = F)

d2 <- read.csv("tabl2.csv", header = T, sep = ";")
x_names <- c("SSPG", paste0("SSP", 1:7))
y_names <- c("Code",
             "Relev", paste0("r", 1:7),
             "Function", paste0("f", 1:4))
#              "LiveG")
d2 <- d2[, c(x_names, y_names)]
d2_x <- d2[, x_names]
d2_melted <- melt(d2, id.vars = y_names)
# graphs
# RELEV, rX, FUNCTION and fX against the SSPXs
for (i in 2:length(y_names)) {
y <- d2_melted[, y_names[i]]
ggplot(d2_melted, aes(x = value, y = y)) +
  geom_point(size = 5,
             alpha = 0.3) + 
  geom_smooth(method = "gam") +
  xlab("") +
  ylab(y_names[i]) +
  theme_bw() +
  facet_wrap(~ variable, scales="free")
ggsave(filename = paste0(y_names[i], ".png"),
       path = dir_graphs,
       width = 7, height = 7)
# dev.off()
}

means_d2 <- sapply(d2[, y_names[-1]], mean)
t <- as.data.frame(rbind(means_d2, means_cg))
t$groupnames <- c("group", "control_group")
t_melted <- melt(t, id.vars = "groupnames")
ggplot(t_melted , aes(x = groupnames, y = value)) +
  geom_bar(stat = "identity") + 
  theme_bw() +
  facet_wrap(~ variable, scales="free")
ggsave(filename = paste0("freq_by_groups", ".png"),
       path = dir_graphs,
       width = 10, height = 10)

diff <- means_d2 - means_cg


#CORRELATIONS
png(filename =  file.path(dir_graphs, "pair_plot.png"),
    width = 500, height = 500)
pairs(d_x)
dev.off()

source("/Users/egorufimtsev/Dropbox/Egor/progs/R/functions/corrgram.panels.R")
png(filename =  file.path(dir_graphs, "corrgram.png"),
    width = 500, height = 500)
corrgram(d_x, type = "data", 
         lower.panel = panel.shadeNtext, 
         upper.panel = NULL)
dev.off()


#PCA
pca <- princomp(d_x, 
                scores = T,
                na.action = na.omit)
par(mfrow = c(1, 2))
pca.variance.explained <- pca$sdev^2 / sum(pca$sdev^2)
# plot percentage of variance explained for each principal component    
png(filename =  file.path(dir_graphs, "var_explained.png"),
    width = 500, height = 500)
barplot(100 * pca.variance.explained,
        las = 2, xlab = '', ylab = '% Variance Explained')
dev.off()
png(filename =  file.path(dir_graphs, "biplot.png"),
    width = 500, height = 500)
biplot(pca)
dev.off()

load <- with(pca, unclass(pca$loadings))
aload <- abs(load) ## save absolute values
contributions <- sweep(aload, 2, colSums(aload), "/")

# plot(d$Relev, pca$scores[, 1])
# plot(d$Function, pca$scores[, 1])
# plot(d$LiveG, pca$scores[, 1])
# predict(pca)[, 1]


# d1 <- d[, c(y_names[1], x_names)]
# pca1 <- princomp(~ ., scores = T,
#                  d1, na.action = na.omit)
# d1 <- d[, c(y_names[2], x_names)]
# pca2 <- princomp(~ .,
#                  data = d1, na.action = na.omit)
# d1 <- d[, c(y_names[3], x_names)]
# pca3 <- princomp(d1[, 1] ~ .,
#                  data = d1, na.action = na.omit)


## Default S3 method:
t.test(x, y = NULL,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95, ...)


