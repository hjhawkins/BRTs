
pacman::p_load(tidyverse, gbm, dismo, car, parallel)

setwd("D:/Documents and Settings/hhawkins/OneDrive - Conservation International Foundation/Data/Predators")
pred<- read.csv("Pred2012_2016_for BRT_minusS.csv")
names(pred)
view(pred)

#subset to only losses only due to predators
#pred <- pred[pred$Reason == "Predator", ]

help(logit)
pred <- mutate(pred, l.Losses = logit(pred$Prop_Smallstock))

pred$LandTenure <- as.factor(pred$LandTenure)
pred$Management <- as.factor(pred$Management)
pred$Livestock_type_died <- as.factor(pred$Livestock_type_died)

str(pred)
names(pred)

model_vars <- c(8,9,17,27,37)
set.seed(1234) #set the seed of R's random number generator (for trees), so can reproduce results (r2 etc)
#Use slow learning rate on data set
par(mfrow=c(1,1))
help(gbm.step)
Pred <- gbm.step(data=pred,
                 gbm.x = model_vars,
                 gbm.y = ncol(pred),
                 plot.main = TRUE,
                 family = "laplace",
                 step.size = 20,
                 tree.complexity = 30,
                 learning.rate = 0.001,
                 #cv.fold = 10,
                 n.fold = 10,
                 bag.fraction = 0.7)

warnings(Pred)
par(mfrow=c(1,1))
summary(Pred)

#Get model r2
(pseudo_r2_results<- 1-(Pred$cv.statistics$deviance.mean/Pred$self.statistics$mean.null))

#Plot the fitted values in relation to each of the predictors in model
gbm.plot.fits(Pred)
