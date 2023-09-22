# Virtual Eco
library(tidyverse)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lme4)
library(MuMIn)
#Read in csv
setwd("C:/Users/HP/OneDrive/Documents/terrestrial plant")
getwd()

#TIMESERIES
ts <- read.csv("timeseries.csv")
# Convert to Numeric
timeseries1 <- mutate_all(timeseries, function(x) as.numeric(as.character(x)))
#MODELS 1-5 FOR EACH SPP ABUNDANCE
model_1 <- lmer(sp_1 ~ refugia * ldd + (1|`time`), data = timeseries1)
model_2 <- lmer(sp_2 ~ refugia * ldd + (1|`time`), data = timeseries1)
model_3 <- lmer(sp_3 ~ refugia * ldd + (1|`time`), data = timeseries1)
model_4 <- lmer(sp_4 ~ refugia * ldd + (1|`time`), data = timeseries1)
model_5 <- lmer(sp_5 ~ refugia * ldd + (1|`time`), data = timeseries1)

#P VALUES + R-Squared
summary(model_1)$coefficients
r.squaredGLMM(model_1)

summary(model_2)$coefficients
r.squaredGLMM(model_2)

summary(model_3)$coefficients
r.squaredGLMM(model_3)

summary(model_4)$coefficients
r.squaredGLMM(model_4)

summary(model_5)$coefficients
r.squaredGLMM(model_5)

#TRANSECTS
t <- read_excel("transects.xlsx")
#Numeric 
t1 <- mutate_all(t, function(x) as.numeric(as.character(x)))
#Adding XCORR 
t1$xcorr <- seq(1,25)
#MODELS 1-5
mod_1 <- lm(sp_1_diff ~ refugia * ldd , data = t1)
mod_2 <- lm(sp_2_diff ~ refugia * ldd , data = t1)
mod_3 <- lm(sp_3_diff ~ refugia * ldd , data = t1)
mod_4 <- lm(sp_4_diff ~ refugia * ldd , data = t1)
mod_5 <- lm(sp_5_diff ~ refugia * ldd , data = t1)
# P- VALUES AND R SQUARED
summary(mod_1)
summary(mod_2)
summary(mod_3)
summary(mod_4)
summary(mod_5)