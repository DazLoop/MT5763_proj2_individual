

#-----Libraries-----------------------------------------------------------------
library(stats)
library(tidyverse)
library(ggplot2)

#-----Load Data-----------------------------------------------------------------

fitness <- read.csv("data/fitness.csv", header = T)
head(fitness)


#-----We will be looking at 3 Automated Model Selection Procedures--------------

#-----1. Backward Elimination Method--------------------------------------------
#-----Works with the most general model and drops variables one by one until the
#-----best model is reached.----------------------------------------------------
#
#-----using command step()------------------------------------------------------

step(lm(Oxygen ~ Age + Weight + RunTime + RestPulse + RunPulse + MaxPulse, 
        data = fitness), direction = "backward")

#-----2. Forward Step Method - Intercept of the Model --------------------------
#-----Starts with the simplest model of all and adds suitable variables, one by 
#-----one, until the best model is reached.-------------------------------------
#-----using command step()------------------------------------------------------

step(lm(Oxygen ~ 1, data = fitness), direction = "forward", 
     scope = ~ Age + Weight + RunTime + RestPulse + RunPulse + MaxPulse)


#-----3. Stepwise Method (both directions - Backwards and Forward---------------
#-----The Stepwise procedure combines the two previous methods (Forward and----- 
#-----Backwards Methods), where variables can be added and dropped.-------------
#-----In all these 3 methods, the AIC is used as the criteria to select tje model,
#-----which is based in the following rule: the lower the AIC the better the model.
#-----using command step()------------------------------------------------------
#-----starting the method with all the variables--------------------------------

step(lm(Oxygen ~ Age + Weight + RunTime + RestPulse + RunPulse + MaxPulse, 
        data = fitness), direction = "both")

#-----data set-------------------------------------------


dim(fitness)
head(fitness, 31)
length(fitness)

fitness[2:4, 5:7]
fitness[ , 1]
names(fitness)

fitness$Age


#-------------------------------------------------------------------------------
#
 

