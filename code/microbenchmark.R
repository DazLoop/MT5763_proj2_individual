library(microbenchmark)
library(boot)
#source("lmboot_attempt2.R")
set.seed(5763)


microbenchmark(lmBoot_4(fitData,10000),times = 10L)



bst <- function(formula, data, indices){
  d <- data[indices, ]
  fit <- lm(formula, data=d)
  return(coef(fit)) 
}




microbenchmark(boot(data = fitData, statistic = bst, R =10000, formula = Oxygen~.),times = 10L)



