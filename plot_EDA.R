library(tidyverse)
library(refund)
library(magrittr)
library(mvtnorm)
library(mgcv)
library(doParallel)
library(foreach)
library(patchwork)
library(reshape2)
# cl <- makeCluster(16)
# registerDoParallel(cl)
# Read Data ----

seed <- 7354981
set.seed(seed)
nh <- readRDS("nhanes_fda_with_r.rds")
nh <- nh %>% filter(!(education %in% c('Refused',"Don't know"))) %>%
  filter(!(CHD %in% c('Refused',"Don't know"))) %>%
  filter(complete.cases(.)) %>%
  mutate(education=factor(education)) %>%
  mutate(CHD=factor(CHD))
Y <- nh  %>% select(MIMS) %>% unclass() %>% .[[1]] %>%
  unclass() %>% `colnames<-`(1:1440)
X <- model.matrix(~age+gender+race+BMI+PIR+CHD+education, data=nh)
t <- 1:1440
# Center the continuous variables for better interpretability
# Save the means beforehand for recovery
means <- c(age=mean(X[,'age']),
           PIR=mean(X[,'PIR']),
           BMI=mean(X[,'BMI']))

X[,'age'] <- scale(X[,'age'], center=TRUE, scale=FALSE)
X[,'PIR'] <- scale(X[,'PIR'], center=TRUE, scale=FALSE)
X[,'BMI'] <- scale(X[,'BMI'], center=TRUE, scale=FALSE)

age_group <- cut(nh$age, c(18,35,50,65,80))
cross_group <- interaction(age_group, nh$gender, sep=' ')

set.seed(23949)
idd1 <- which(cross_group=='(35,50] Female') 
idd2 <- which(cross_group=='(65,80] Female')

png('sample-y.png',width=1000,height=600)
melt(Y[idd1,]) %>% data.frame() -> melt_Y_subset1
p1 <- ggplot() + 
  geom_line(aes(y=value,x=Var2), 
            data=filter(melt_Y_subset1, Var1==rownames(Y)[idd1[1]]),
            col='blue') +
  geom_boxplot(aes(x=Var2, y=value,group=(Var2)),
               data=filter(melt_Y_subset1, Var2%%120==0),
               alpha=0.3) +
  labs(x='Time',y='MIMS',title='MIMS for Females aged 35-50') +
  scale_x_continuous(breaks=(0:12)*120,
                     labels=paste(2*(0:12),c(':01',rep(':00',12)),sep=''))
melt(Y[idd2,]) %>% data.frame() -> melt_Y_subset2
p2 <- ggplot() + 
  geom_line(aes(y=value,x=Var2), 
            data=filter(melt_Y_subset2, Var1==rownames(Y)[idd2[1]]),
            col='red') +
  geom_boxplot(aes(x=Var2, y=value,group=(Var2)),
               data=filter(melt_Y_subset2, Var2%%120==0),
               alpha=0.3) +
  labs(x='Time',y='MIMS',title='MIMS for Females aged 65-80') +
  scale_x_continuous(breaks=(0:12)*120,
                     labels=paste(2*(0:12),c(':01',rep(':00',12)),sep=''))
print(p1+p2)
dev.off()

png('sample-y-log1p.png',width=1000,height=600)
melt(Y[idd1,]) %>% data.frame() %>% mutate(value=log1p(value)) -> melt_Y_subset1
p3 <- ggplot() + 
  geom_line(aes(y=value,x=Var2), 
            data=filter(melt_Y_subset1, Var1==rownames(Y)[idd1[1]]),
            col='blue') +
  geom_boxplot(aes(x=Var2, y=value,group=(Var2)),
               data=filter(melt_Y_subset1, Var2%%120==0),
               alpha=0.3) +
  labs(x='Time',y='MIMS',title='log(1+MIMS) for Females aged 35-50') +
  scale_x_continuous(breaks=(0:12)*120,
                     labels=paste(2*(0:12),c(':01',rep(':00',12)),sep=''))
melt(Y[idd2,]) %>% data.frame() %>% mutate(value=log1p(value)) -> melt_Y_subset2
p4 <- ggplot() + 
  geom_line(aes(y=value,x=Var2), 
            data=filter(melt_Y_subset2, Var1==rownames(Y)[idd2[1]]),
            col='red') +
  geom_boxplot(aes(x=Var2, y=value,group=(Var2)),
               data=filter(melt_Y_subset2, Var2%%120==0),
               alpha=0.3) +
  labs(x='Time',y='MIMS',title='log(1+MIMS) for Females aged 65-80') +
  scale_x_continuous(breaks=(0:12)*120,
                     labels=paste(2*(0:12),c(':01',rep(':00',12)),sep=''))
print(p3 + p4)
dev.off()