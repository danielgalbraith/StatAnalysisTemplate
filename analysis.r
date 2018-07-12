# Necessary libraries:
library(plyr)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(lme4)
library(e1071)
library(anytime)
theme_set(theme_bw())

# This file contains several examples of typical statistical analysis functions,
# can be used as a template for other R scripts.
#
# Assumes that the dataset contains the following:
#
# depvar => dependent variable, trying to predict increase (assume ratio scaled here)
# bin-indvar => binary independent variable: categorial, bin-indvar-1 = True, bin-indvar-0 = False
# cont-indvar-A => continuous independent variable A: ratio scale
# cont-indvar-B => continuous independent variable B: ratio scale
#
# Also assume that there are zero values/NAs for the dependent variable

setwd("") # Insert path to working directory
source("helpers.R")
d = read.csv(file="") # Insert csv filename for data
summary(d)
head(d)

# Create dataframe with depvar NAs as zero
d$depvar[is.na(d$depvar)] <- 0

# Return number of rows
nrow(d)

# Make table of all rows excluding zero depvar
d_no_zero <- d %>%
  filter(depvar != 0)
nrow(d_no_zero)

# Make table of all rows only zero depvar
d_only_zero <- d %>%
  filter(depvar == 0)
nrow(d_only_zero)

# Make table consisting only of bin-indvar-1 rows
bin-indvar-1_table <- d %>%
  filter(bin-indvar-1 == 1)
bin-indvar-1_table$depvar[is.na(bin-indvar-1_table$depvar)] <- 0 # Change NAs in depvar column to 0
nrow(bin-indvar-1_table)

# Make table of bin-indvar-1 excluding zeros
bin-indvar-1_no_zero <- bin-indvar-1_table %>%
  filter(depvar != 0)
nrow(bin-indvar-1_no_zero)

# Make table of bin-indvar-1 only zeros
bin-indvar-1_only_zero <- bin-indvar-1_table %>%
  filter(depvar == 0)
nrow(bin-indvar-1_only_zero)

# Make table consisting only of bin-indvar-0 rows
bin-indvar-0_table <- d %>%
  filter(bin-indvar-1 == 0)
bin-indvar-0_table$depvar[is.na(bin-indvar-0_table$depvar)] <- 0 # Change NAs in depvar column to 0
nrow(bin-indvar-0_table)

# Make table of bin-indvar-0 excluding zeros
bin-indvar-0_no_zero <- bin-indvar-0_table %>%
  filter(depvar != 0)
nrow(bin-indvar-0_no_zero)

# Make table of bin-indvar-0 only zeros
bin-indvar-0_only_zero <- bin-indvar-0_table %>%
  filter(depvar == 0)
nrow(bin-indvar-0_only_zero)

# Get max, min, mean and median depvar from each subset:
summary(d_no_zero)
summary(bin-indvar-1_no_zero)
summary(bin-indvar-0_no_zero)

# Get st. dev. and IQR for depvar of each subset:
sd(d_no_zero$depvar)
sd(bin-indvar-1_no_zero$depvar)
sd(bin-indvar-0_no_zero$depvar)
IQR(d_no_zero$depvar)
IQR(bin-indvar-1_no_zero$depvar)
IQR(bin-indvar-0_no_zero$depvar)

# Make density plot of depvar for each subset:
limit <- 1 # change this for data
d_depvar_dens_plot <- ggplot(d_no_zero, aes(x=depvar)) + 
  geom_density() +
  scale_x_continuous(limits = c(0, limit)) + 
  geom_vline(aes(xintercept=mean(depvar)), color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(depvar)), color="blue", linetype="dashed", size=1) +
  labs(x="depvar", y = "Density") + 
  theme(plot.title = element_text(hjust = 0.5, size=24), axis.text=element_text(size=22),
        axis.title=element_text(size=22,face="bold"))
d_depvar_dens_plot

bin-indvar-1_depvar_dens_plot <- ggplot(bin-indvar-1_no_zero, aes(x=depvar)) + 
  geom_density() +
  scale_x_continuous(limits = c(0, limit))  + 
  geom_vline(aes(xintercept=mean(depvar)), color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(depvar)), color="blue", linetype="dashed", size=1) +
  labs(x="depvar", y = "Density") + 
  theme(plot.title = element_text(hjust = 0.5, size=24), axis.text=element_text(size=22),
        axis.title=element_text(size=22,face="bold"))
bin-indvar-1_depvar_dens_plot

bin-indvar-0_depvar_dens_plot <- ggplot(bin-indvar-0_no_zero, aes(x=depvar)) + 
  geom_density() +
  scale_x_continuous(limits = c(0, limit))  + 
  geom_vline(aes(xintercept=mean(depvar)), color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(depvar)), color="blue", linetype="dashed", size=1) +
  labs(x="depvar", y = "Density") + 
  theme(plot.title = element_text(hjust = 0.5, size=24), axis.text=element_text(size=22),
        axis.title=element_text(size=22,face="bold"))
bin-indvar-0_depvar_dens_plot

# Compute skewness of each subset
skewness(d_no_zero$depvar)
skewness(bin-indvar-1_no_zero$depvar)
skewness(bin-indvar-0_no_zero$depvar)

# t-test for difference in depvar between bin-indvar-1 and bin-indvar-0
d_no_zero$log_depvar <- log(d_no_zero$depvar) # create log transform column of depvar
bin-indvar-1_no_zero$log_depvar <- log(bin-indvar-1_no_zero$depvar)
bin-indvar-0_no_zero$log_depvar <- log(bin-indvar-0_no_zero$depvar)

t.test(bin-indvar-1_no_zero$log_depvar, bin-indvar-0_no_zero$log_depvar, alternative = "greater")

# Create scatterplots of cont-indvar-A vs. log_depvar
cont-indvar-A_scatter_bin-indvar-1 <- ggplot(bin-indvar-1_no_zero, aes(x=cont-indvar-A, y=log_depvar)) + 
  geom_point(shape = 20, size = 1) +
  stat_smooth(method = lm) +
  labs(x="cont-indvar-A", y = "Log of depvar") + 
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=22,face="bold"))
cont-indvar-A_scatter_bin-indvar-1

cont-indvar-A_scatter_bin-indvar-0 <- ggplot(bin-indvar-0_no_zero, aes(x=cont-indvar-A, y=log_depvar)) + 
  geom_point(shape = 20, size = 1) +
  stat_smooth(method = lm) +
  labs(x="cont-indvar-A", y = "Log of depvar") + 
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=22,face="bold"))
cont-indvar-A_scatter_bin-indvar-0

# If distribution is long-tailed and there are several outliers:
# Create bin-indvar-1 set excluding outliers
outlier_threshold_upper <- 20 # change according to data
outlier_threshold_lower <- 12 # change according to data
bin-indvar-1_exclude_outliers <- bin-indvar-1_no_zero[bin-indvar-1_no_zero$log_depvar<outlier_threshold_upper & bin-indvar-1_no_zero$log_depvar>outlier_threshold_lower,]

# Test correlation of cont-indvar-A and depvar in bin-indvar-1 set
cor(bin-indvar-1_no_zero$cont-indvar-A, bin-indvar-1_no_zero$log_depvar)
cor(bin-indvar-1_exclude_outliers$cont-indvar-A, bin-indvar-1_exclude_outliers$log_depvar)

# Simple density plots for cont-indvar-B variable in each subset:
plot(density(d_no_zero$cont-indvar-B), main=NA, xlab="cont-indvar-B", cex.lab=2, cex.axis=2)
plot(density(bin-indvar-1_no_zero$cont-indvar-B), main=NA, xlab="cont-indvar-B", cex.lab=2, cex.axis=2)
plot(density(bin-indvar-0_no_zero$cont-indvar-B), main=NA, xlab="cont-indvar-B", cex.lab=2, cex.axis=2)

# Get modes for cont-indvar-B and depvar in split of bin-indvar-0 set:
cont-indvar-B_threshold <- 75 # change according to data
bin-indvar-0_longer <- bin-indvar-0_no_zero[bin-indvar-0_no_zero$cont-indvar-B>=cont-indvar-B_threshold,]
get.mode(bin-indvar-0_longer$cont-indvar-B)
get.mode(bin-indvar-0_longer$depvar)
# Get modes for cont-indvar-B and depvar, longer-time bin-indvar-1 set:
bin-indvar-1_longer <- bin-indvar-1_no_zero[bin-indvar-1_no_zero$cont-indvar-B>=cont-indvar-B_threshold,]
get.mode(bin-indvar-1_longer$cont-indvar-B)
get.mode(bin-indvar-1_longer$depvar)
# Get modal depvar increase:
modal_increase <- (get.mode(bin-indvar-1_longer$depvar) / get.mode(bin-indvar-1_longer$cont-indvar-B)) - (get.mode(bin-indvar-0_longer$depvar) / get.mode(bin-indvar-0_longer$cont-indvar-B))
modal_increase
# Get medians:
median(bin-indvar-0_longer$cont-indvar-B)
median(bin-indvar-0_longer$depvar)
median(bin-indvar-1_longer$cont-indvar-B)
median(bin-indvar-1_longer$depvar)
# Get median depvar increase:
median_increase <- (median(bin-indvar-1_longer$depvar) / median(bin-indvar-1_longer$cont-indvar-B)) - (median(bin-indvar-0_longer$depvar) / median(bin-indvar-0_longer$cont-indvar-B))
median_increase

# If a linear regression is appropriate:
# Build linear regression model: depvar as a function of fixed effects
model <- lm(log_depvar ~ cont-indvar-A * bin-indvar-1 * cont-indvar-B, data=d_no_zero)
summary(model)
# Q-Q plot of residuals:
plot(model,which=2,cex.lab=2, cex.axis=2)

# If residuals are not normally distributed, and polynomial model is appropriate:
# Polynomial model:
poly_model <- lm(log_depvar ~ bin-indvar-1 + polym(cont-indvar-A, degree=3) + polym(cont-indvar-B, degree=3), data = d_no_zero)
summary(poly_model)
plot(poly_model,which=2,cex.lab=2, cex.axis=2)

# Histogram of log of depvar
hist(log(d_no_zero$depvar), main=NA, xlab="Log of depvar", freq=F,cex.lab=2, cex.axis=2) 
legend("topright", c("Normal curve", "Empirical curve"), col=c("black", "red"), lwd=1)
lines(seq(10, 40, by=.5), 
      dnorm(seq(10, 40, by=.5), mean(log(d_no_zero$depvar)), sd(log(d_no_zero$depvar))))
lines(density(log(d_no_zero$depvar)),col="red")

# If histogram of log of depvar itself is still not normally distributed with heavy tails:
# Histogram excluding tails:
log_depvar_threshold_upper <- 19 # change according to data
log_depvar_threshold_lower <- 14 # change according to data
no_tails <- d_no_zero[d_no_zero$log_depvar >= log_depvar_threshold_lower & d_no_zero$log_depvar <= log_depvar_threshold_upper,]
hist(log(no_tails$depvar), main=NA, xlab="Log of depvar", freq=F,cex.lab=2, cex.axis=2)
lines(seq(10, 40, by=.5), 
      dnorm(seq(10, 40, by=.5), mean(log(no_tails$depvar)), sd(log(no_tails$depvar))))
legend("topright", c("Normal curve", "Empirical curve"), col=c("black", "red"), lwd=1)
lines(density(log(no_tails$depvar)),col="red")

# Linear model excluding tails:
no_tails_model <- lm(log_depvar ~ cont-indvar-A * bin-indvar-1 * cont-indvar-B, data=no_tails)
summary(no_tails_model)
no_tails_model_raw <- lm(depvar ~ cont-indvar-A * bin-indvar-1 * cont-indvar-B, data=no_tails)
summary(no_tails_model_raw)
plot(no_tails_model,which=2,cex.lab=2, cex.axis=2)

# Generate correlation matrices:
cor(bin-indvar-1_no_zero$cont-indvar-A, bin-indvar-1_no_zero$cont-indvar-B)
plot(bin-indvar-1_no_zero$cont-indvar-A, bin-indvar-1_no_zero$cont-indvar-B)

# Investigating subset of data with only NAs for depvar:
# Histogram for only zeros
hist(bin-indvar-1_only_zero$cont-indvar-A, freq=F, main=NA, xlab="cont-indvar-A", cex.lab=2, cex.axis=2, xlim=c(-1.5,20), ylim=c(0,15)) # change lims according to data
lines(density(bin-indvar-1_only_zero$cont-indvar-A), col="red")
hist(bin-indvar-1_no_zero$cont-indvar-A, freq=F, main=NA, xlab="cont-indvar-A", cex.lab=2, cex.axis=2, xlim=c(-1.5,20), ylim=c(0,15)) # change lims according to data
lines(density(bin-indvar-1_no_zero$cont-indvar-A), col="red")

# Polynomial regression incl zeros
model_incl_zeros <-lm(depvar ~ bin-indvar-1 + polym(cont-indvar-A, degree=3) + polym(cont-indvar-B, degree=3), data = d)
summary(model_incl_zeros)
