library(microbenchmark)
library(boot)
source("code/lmboot_Final.R")

library(dplyr)
testData <- fitData %>% select(Oxygen, everything())
set.seed(5763)

#Create Bootstrap subfunction to use in Boot function
bst <- function(formula, data, indices){
  d <- data[indices, ]
  fit <- lm(formula, data=d)
  return(coef(fit)) 
}

#Microbench mark was conducted for 100, 1000, 10000 and 100000 samples.

#Microbenchmark for lmBoot_par function
microbenchmark(lmBoot_par = lmBoot_par(fitData, 100),
               boot = boot(data = fitData, statistic = bst, R = 100, formula = Oxygen ~ .),
               times = 10)
microbenchmark(lmBoot_par = lmBoot_par(fitData, 1000),
               boot = boot(data = fitData, statistic = bst, R = 1000, formula = Oxygen ~ .),
               times = 10)
microbenchmark(lmBoot_par = lmBoot_par(fitData, 10000),
               boot = boot(data = fitData, statistic = bst, R = 10000, formula = Oxygen ~ .),
               times = 10)
microbenchmark(lmBoot_par = lmBoot_par(fitData, 100000),
               boot = boot(data = fitData, statistic = bst, R = 100000, formula = Oxygen ~ .),
               times = 10)
