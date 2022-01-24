library(tidyverse)
library(forecast)

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
GNP <- window(log(GNP), start = c(1983, 1))
Unemployment <- window(Unemployment, start = c(1983, 1))
autoplot(cbind(GNP, Unemployment), facets = TRUE)

#removing outliers
GNP_clean <- tsclean(GNP)
Unemployment_clean <- tsclean(Unemployment)

autoplot(cbind(GNP_clean, Unemployment_clean), facets = TRUE)
## Step 1

# Pretesting for time trends and unit roots
library(urca)
GNP_kpss <- ur.kpss(GNP_clean, type = "tau")
summary(GNP_kpss)
# Y has a unit root

GNP_df <- ur.df(GNP_clean, type = "trend", selectlags = "AIC")
summary(GNP_df)
# Y has a unit root 

Unemployment_kpss <- ur.kpss(Unemployment_clean, type = "mu")
summary(Unemployment_kpss)

# Make the sequences I(0)
GNP_random <- na.remove(decompose(GNP_clean)$random)
Unemployment_random <- na.remove(decompose(Unemployment_clean)$random)
autoplot(cbind(GNP_random, Unemployment_random), facets = TRUE)

# Test stationarity of decomposed variables
library(urca)
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

## Multivariate
library(vars)
u <- Unemployment_random
Q <- GNP_random
VARselect(cbind(Q, u), type = "const")
# We need to choose p = 5 in order to have a WN residuals at 5%
var1 <- VAR(cbind(Q, u), type = "const", p = 5)

#Residuals test
serial.test(var1, lags.pt = 10, type = "PT.asymptotic")
serial.test(var1, lags.bg = 10, type = "BG")

normality.test(var1)

summary(var1)

# Causality
causality(var1, "Q")
# GNP Granger causes Unemployment
causality(var1, "u")
# Unemployment Granger causes GNP

#IRF
irf_GNP_GNP <- irf(var1, impulse = "Q", response =
              c("Q"), n.ahead = 20,
            cumulative = TRUE)
plot(irf_GNP_GNP)


irf_Unemployment_GNP <- irf(var1, impulse = "Q", response =
                     c("u"), n.ahead = 20,
                   cumulative = TRUE)
plot(irf_Unemployment_GNP)

irf_GNP_Unemployment <- irf(var1, impulse = "u", response =
                     c("Q"), n.ahead = 20,
                   cumulative = TRUE)
plot(irf_GNP_Unemployment)

irf_Unemployment_Unemployment <- irf(var1, impulse = "u", response =
                              c("u"), n.ahead = 20,
                            cumulative = TRUE)
plot(irf_Unemployment_Unemployment)

fevd_var1 <- fevd(var1, n.ahead = 10)

plot(fevd_var1)

# Forecast
var_frcst <- forecast(var1, h = 8)
autoplot(var_frcst)
