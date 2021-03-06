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
dir_kate <- "/Users/egorufimtsev/Dropbox/kate_termpaper"
setwd(dir_kate)
dir_graphs <- file.path(dir_kate, "graphs")
dir.create(dir_graphs, showWarnings = F)

d <- read.csv("tabl1.csv", header = T, sep = ";")
x_names <- c("Code", "SSPG", paste0("SSP", 1:7))
y_names <- c("Relev", "Function", "LiveG")
d <- d[, c(x_names, y_names)]
d_x <- d[, x_names]
d_melted <- melt(d, id.vars = c("Code", "Relev", "Function", "LiveG"))
#RELEV and FUNCTION against the SSPXs
ggplot(d_melted, aes(x = value, y = Relev)) +
  geom_point(size = 5,
             alpha = 0.3) + 
  geom_smooth(method = "gam") +
  xlab("") +
  theme_bw() +
  facet_wrap(~ variable, scales="free")
ggsave(filename = "relev.png",
       path = dir_graphs,
       width = 7, height = 7)
dev.off()
ggplot(d_melted, aes(x = value, y = Function)) +
  geom_point(size = 5,
             alpha = 0.3) + 
  geom_smooth(method = "gam") +
  xlab("") +
  theme_bw() +
  facet_wrap(~variable, scales="free")
ggsave(filename = "function.png",
       path = dir_graphs,
       width = 7, height = 7)
dev.off()

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

#REGRESSION
m_relev <- lm(log(Relev) ~ 0 + SSPG + SSP1 + SSP2 + SSP3 + SSP4 + SSP5 + SSP6 + SSP7, data = d)
summary(m_relev)
m_func <- lm(Function ~ 0 + SSPG + SSP1 + SSP2 + SSP3 + SSP4 + SSP5 + SSP6 + SSP7, data = d)
summary(m_func)
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


