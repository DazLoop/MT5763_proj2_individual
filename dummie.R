##-----Build a model that predicts Oxygen intake rates(a measure of aerobic fitness)
#


##-----Library------------------------------------------------------------------
#
library(stats)
library(tidyverse)
library(tidyverse)

library(modelr)
options(na.action = na.warn)


##-----The Simple Model---------------------------------------------------------
#
#fitness <- read.csv("data/fitness.csv", header = T)
ggplot(sim1, aes(x, y)) +
  geom_point()


##-----geo_abline()-------------------------------------------------------------
#takes a slope and intercept as parameters--------------------------------------
#


models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)


ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point() 


##-----Turn model family into  function------------------ ----------------------


model1 <- function(a, data) {
  a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)


##-----Root MEan Squared Deviation----------------------------------------------


measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)

sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}


##-----Use purrr helper function to compute distance ---------------------------


sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models


##-----overlay of the ten best models-------------------------------------------


ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  )

##-----Models as observatios - scatterplot--------------------------------------


ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))


#-----Generate an evenly spaced grid of points (this is called a grid search)---


grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) 


#-----overlay the best 10 models back on the original data----------------------

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(grid, rank(dist) <= 10)
  )

# imagine iteratively making the grid finer and finer until you narrowed in on 
#the best model.There’s a better way to tackle that problem: a numerical 
#minimisation tool called Newton-Raphson search.#

best <- optim(c(0, 0), measure_distance, data = sim1)
best$par

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])


#-----a special case of a broader family: linear models-------------------------


sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

#---------------------------
sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 5.5 + 8 + rt(length(x), df = 2)
)
sim1a

#---------------------------



#---------------------------

#---------------------------

#---------------------------




