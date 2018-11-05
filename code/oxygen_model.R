#Libraries#

# load data
fitness <- read.csv("data/fitness.csv")
head(fitness)

# define helper functions
lmBoot <- function(inputData, nBoot){
  
  bootResults <- array(dim=c(nBoot, 2))
  
  for(i in 1:nBoot){   #nBoot para 1000
    
    # resample our data with replacement
    bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
    
    # fit the model under this alternative reality
    bootLM <- lm(y ~ x, data = bootData)
    
    # store the coefs
    if(i == 1){
      
      bootResults <- matrix(coef(bootLM), ncol = 2)
      
    } else {
      
      bootResults<- rbind(bootResults, matrix(coef(bootLM), ncol = 2))
      
    }
    
    
  } # end of i loop
  
  bootResults
  
}

inputData <- fitness
bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]

df <- data.frame(y = fitness$RunTime, x = fitness$Weight)

bootLM <- lm(y ~ x, data = df)
coef(bootLM)

bootresults <- lmBoot(df, 100)
