# ORIGIONAL LMBOOT FUNCTION (CARL) ----------------------------------------
lmBoot <- function(inputData, nBoot){
  for(i in 1:nBoot){
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

# IMPROVEMENTS: ----------------------------------------------------------------

#Changes: 1. Allows for multiple covariates
#         2. Calculates the Beta coefficients using matrix notaion rather than lm
#         3. Uses a small function to carry out the bootstrap algorithm
#         4. Uses sapply instead of a for loop

#Create a function for bootstrapping algorithm to use inside other functions
bootLM <- function(samples, inputData, index){
  #Purpose: Generate the linear regression beta coefficients
  #Inputs: samples: a dataframe containing the indices for the bootstrap samples
  #        inputData: a dataframe containing the response variable, which must be 
  #        in the first column of the dataframe, and the covariates of interest
  #        index: the index of the position in the BootResults that Bootlm 
  #        should be applied to
  #Outputs: Beta: An matrix containing the parameter estimates from a linear 
  #         regression
  
  bootData <- inputData[samples[, index], ]
  Xmat <- bootData[, -1]
  Ymat <- bootData[, 1]
  beta <- solve(t(Xmat)%*%Xmat)%*%t(Xmat)%*%Ymat
  return(beta)
}

lmBoot_imp <- function(inputData, nBoot){
  #Purpose: Generate a large number of linear regression beta coefficients using
  #         bootstrap methods.
  #Inputs: inputData: a dataframe containing the response variable, which must be 
  #        in the first column of the dataframe, and the covariates of interest
  #        nBoot: the number of bootstrap samples to generate.
  #Outputs: BootResults: An arraycontaing the parameter estimates of each 
  #         each bootstrap sample.
  #         ConfidenceIntervals: A matrix containing 95% confidence intervals 
  #         for each parameter.
  
  #Calculate the number of observations in the dataset 
  nObs <- nrow(inputData)
  
  #Create a sample dataset with a column of 1s for the intercept
  sampleData <- as.matrix(cbind(inputData[, 1], 1, inputData[, -1]))
  
  # Create the a matrix of indices for the bootstrap samples
  bootSamples <- matrix(sample(1:nrow(inputData), nObs * nBoot, replace = T), 
                        nrow = nObs, ncol = nBoot)
  
  #Create an empty array to store results
  bootResults <- array(dim = c(nBoot, ncol(sampleData[, -1]))) 

  #Use sapply to apply bootLM to bootResults matrix
  bootResults <- sapply(1:nBoot, bootLM, inputData = sampleData, 
                        samples = bootSamples)
  bootResults <- t(as.matrix(bootResults))
  
  return(bootResults)
}
                                            
# Parallelisation: --------------------------------------------------------------
#install.packages(doParallel)
library(doParallel)
lmBoot_par <- function(inputData, nBoot){
  #Purpose: Generate a large number of linear regression beta coefficients using
  #         bootstrap methods.
  #Inputs: inputData: a dataframe containing the response variable, which must be 
  #        in the first column of the dataframe, and the covariates of interest
  #        nBoot: the number of bootstrap samples to generate.
  #Outputs: BootResults: An arraycontaing the parameter estimates of each 
  #         each bootstrap sample.
  #         ConfidenceIntervals: A matrix containing 95% confidence intervals 
  #         for each parameter.
  
  #Calculate the number of observations in the dataset 
  nObs <- nrow(inputData)
  
  #Create the sample data with 1s for the intercept
  sampleData <- as.matrix(cbind(inputData[, 1], 1, inputData[, -1]))
  
  #Set up parallisation
  nCores <- detectCores()
  myClust <- makeCluster(nCores - 1, type = "PSOCK")
  registerDoParallel(myClust)
  
  # Create the a matrix of indices for the bootstrap samples
  bootSamples <- matrix(sample(1:nrow(inputData), nObs * nBoot, replace = T), 
                        nrow = nObs, ncol = nBoot)
  
  #Use parallised sapply to apply bootLM to bootResults matrix
  bootResults <- matrix(NA, nBoot, ncol(sampleData[, -1]))
  bootResults <- parSapply(myClust, 1:nBoot, bootLM, inputData = sampleData, 
                           samples = bootSamples)

  #Close parallisation
  stopCluster(myClust)
  
  return(t(bootResults))
}

# Profiling ---------------------------------------------------------------
# install.packages("profvis")
library(profvis)

profvis({lmBoot(testData, 10000)})
profvis({lmBoot_3(testData, 10000)})
profvis({lmBoot_4(testData, 10000)})

