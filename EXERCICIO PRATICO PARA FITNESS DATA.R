############################################################################

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

#-----data set------------------------------------------------------------------
# getwd()
# dim(fitness)
# head(fitness, 31)
# length(fitness)
# fitness[2:4, 5:7]
# fitness[ , 1]
# names(fitness)
# fitness$Age
# mean(fitness$Age)
# sd(fitness$Age)
# sum(fitness$Age)

#-------------------------------------------------------------------------------

w
summary(fitnessLM)

names(fitnessLM)
str(fitnessLM)
head(fitnessLM$x)

#-----Recalling y = XB----------------------------------------------------------

prediction <- fitnessLM$x%*%coef(fitnessLM)
head(prediction)
summary(fitnessLM)
coef(fitnessLM)

#-----Confidence Interval CI----------------------------------------------------

confint(fitnessLM)


#-----residuals-----------------------------------------------------------------


head(resid(fitnessLM))


#-----Checking the Assumptions--------------------------------------------------

#-----1. The adquacy of our model for the signal
#-----2. The adquacy of our model fro the noise

qqnorm(resid(fitnessLM))


#-----Shapiro-Wilk normality test-----------------------------------------------
#an approximate p-value < 0.1 - the test is adequate
shapiro.test(resid(fitnessLM))

fitnessResid <- resid(fitnessLM)
par(mfrow = c(2, 2))
plot(fitness$Age, fitnessResid)
plot(fitness$Weight, fitnessResid)
plot(fitness$Oxygen, fitnessResid)
plot(fitness$RunTime, fitnessResid)
plot(fitness$RestPulse, fitnessResid)
plot(fitness$RunPulse, fitnessResid)

plot(factor(fitness$Age), fitnessResid)
plot(factor(fitness$Weight), fitnessResid)
plot(factor(fitness$Oxygen), fitnessResid)
plot(factor(fitness$RunTime), fitnessResid)
plot(factor(fitness$RestPulse), fitnessResid)
plot(factor(fitness$RunPulse), fitnessResid)


plot(fitnessResid, fitted(fitnessLM))

AIC(fitnessLM)


par(mfrow = c(2, 2))
plot(fitnessLM)

#-----Backward Elimination Method-----------------------------------------------
step(fitnessLM)    


#-----Predictions--------------------------------------------------------------


table(fitness$Age)
table(fitness$Weight)
table(fitness$Oxygen)
table(fitness$RunTime)
table(fitness$RestPulse)
table(fitness$RunPulse)

