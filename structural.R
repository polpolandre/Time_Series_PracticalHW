library(tidyverse)
library(forecast)
library(urca)
library(tseries)
library(urca)
library(vars)
source("plot_irf.R")

gnp <- read.csv("GNP.csv", row.names = "DATE")
# format index to be a date
tail(rownames(gnp), n = 1)
head(rownames(gnp), n = 1)
GNP <- ts(gnp$GNP, start = c(1947, 1), end = c(2021, 3), frequency = 4)

unemployment <- read.csv("LRUN64TTUSQ156S.csv", row.names = "DATE")
#format index to be a date
rownames(unemployment) <- as.Date(rownames(unemployment), format = "%Y-%m-%d")
unemployment$LRUN64TTUSQ156S <- unemployment$LRUN64TTUSQ156S

# get first date and last 
tail(rownames(unemployment), n = 1)
head(rownames(unemployment), n = 1)
Unemployment <- ts(unemployment, start = c(1970, 1), end = c(2021, 3), frequency = 4)
Unemployment

#make both series the same length
GNP <- window(log(GNP), start = c(1983, 1), end = c(2021, 3))
Unemployment <- window(Unemployment, start = c(1983, 1), end = c(2021, 3))
autoplot(cbind(GNP, Unemployment), facets = TRUE)

#removing outliers
GNP_clean <- tsclean(GNP)
Unemployment_clean <- tsclean(Unemployment)

## Step 1
# Pretesting for time trends and unit roots
GNP_kpss <- ur.kpss(GNP, type = "tau")
summary(GNP_kpss)
# Y has a unit root

GNP_df <- ur.df(GNP, type = "trend", selectlags = "AIC")
summary(GNP_df)
# Y has a unit root 

# Make the sequences I(0)
GNP_random <- decompose(GNP_clean)$random
Unemployment_random <- decompose(Unemployment_clean)$random

# Test stationarity of decomposed variables
GNP_rand_kpss <- ur.kpss(GNP_random, type = "mu")
summary(GNP_rand_kpss)
# Stationary

GNP_rand_df <- ur.df(GNP_random, selectlags = "AIC")
summary(GNP_rand_df)
# Stationary

unemp_rand_kpss <- ur.kpss(Unemployment_random)
summary(unemp_rand_kpss)
# Stationary

unemp_rand_df <- ur.df(Unemployment_random, selectlags = "AIC")
summary(unemp_rand_df)
# Stationary

autoplot(cbind(GNP_random, Unemployment_random), facets = TRUE)

## Structural
u <- na.remove(Unemployment_random)
Q <- na.remove(GNP_random)
# Recursive ordering
library(svars)
svar_chol <- id.chol(var_bq)

irf_bq_QQ <- irf(svar_chol, n.ahead = 40, response = "Q", 
                 cumulative = TRUE,
                 impulse ="Q")
irf_bq_uQ <- irf(svar_chol, n.ahead = 40, response = "Q", 
                 cumulative = TRUE,
                 impulse = "u")
irf_bq_Qu <- irf(svar_chol, n.ahead = 40,
                 cumulative = TRUE,
                 response = "u",impulse ="Q")
irf_bq_uu <- irf(svar_chol, n.ahead = 40,
                 cumalative = TRUE,
                 response = "u",impulse = "u")

par(mfrow = c(2,2))
plot(irf_bq_QQ)
plot(irf_bq_uQ)
plot(irf_bq_Qu)
plot(irf_bq_uu)

fevd_chol <- fevd(svar_chol, h = 10)
plot(fevd_chol)
