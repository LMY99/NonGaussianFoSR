suppressPackageStartupMessages({
  library(tidyverse)
  library(refund)
  library(magrittr)
  library(mvtnorm)
  library(mgcv)
  library(doParallel)
  library(e1071)
  library(gridExtra)
  library(patchwork)
  library(iterators)
})

source('simulation/simulation_datagen.R')
jpeg('WIT presentation 2/noise-cartoon.jpg')
plot(rnorm(14400)*sd_func(1:14400/10), type='l',
     ylab='',xlab='')
dev.off()

load('point_estimate.rda')
png('WIT presentation 2/bi_distribution.png', width=960, height=480)
(point_estimate$SStd %*% t(splines2::bsp(1:1440, knots=c(1:23)*60, Boundary.knots = c(0,1440),
                degree=3, intercept=TRUE, periodic=TRUE)))[1:1000,] |> t() |>
  matplot(type='l',col=scales::alpha('black',0.05),
          ylab='',xlab='Time',main='Fitted Random Effect from 1000 Individuals')
dev.off()

nh <- readRDS("nhanes_fda_with_r.rds")
nh <- nh %>% filter(!(education %in% c('Refused',"Don't know"))) %>%
  filter(!(CHD %in% c('Refused',"Don't know"))) %>%
  filter(complete.cases(.)) %>%
  mutate(education=factor(education)) %>%
  mutate(CHD=factor(CHD)) %>% mutate(MIMS=log1p(MIMS))
Y <- nh  %>% select(MIMS) %>% unclass() %>% .[[1]] %>%
  unclass() %>% `colnames<-`(1:1440)
X <- model.matrix(~age+gender+race+BMI+PIR+CHD+education, data=nh)
t <- 1:1440
rm('nh'); invisible(gc())
# Center the continuous variables for better interpretability
means <- c(age=mean(X[,'age']),
           PIR=mean(X[,'PIR']),
           BMI=mean(X[,'BMI']))
X[,'age'] <- scale(X[,'age'], center=TRUE, scale=FALSE)
X[,'PIR'] <- scale(X[,'PIR'], center=TRUE, scale=FALSE)
X[,'BMI'] <- scale(X[,'BMI'], center=TRUE, scale=FALSE)

png('WIT presentation 2/firststep_resid.png', width=960, height=480)
(Y-X%*%t(point_estimate$A))[1:3,] %>% t() %>% matplot(type='l',lty=1,main='Residuals from 3 indivduals after first step')
dev.off()

png('WIT presentation 2/twostep_resid.png', width=960, height=480)
(Y-X%*%t(point_estimate$A)-point_estimate$)[1:3,] %>% t() %>% matplot(type='l',lty=1,main='Residuals from 3 indivduals after first step')
dev.off()