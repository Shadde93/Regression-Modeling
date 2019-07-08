library(MASS)
#needed for studres?
library("TH.data")
data("bodyfat")
bodyfatwomen <- bodyfat
attach(bodyfatwomen)
modelFull <- lm(DEXfat ~ age  + hipcirc + elbowbreadth + kneebreadth + anthro3a + anthro3b + anthro3c +anthro4 ,data=bodyfatwomen)
#Creates a model with the all the data
summary(modelFull) 
#summaries the model. suggest hipcirc and kneebreadth is important
plot(modelFull)
#Plots residuals vs fit. Indicates residuals are not random normal distrubuted and a transformation may be necessary.
#Scale location indicated potential increase in variance but probably not.
anova(modelFull)


plot(DEXfat,studres(modelFull))
plot(age,studres(modelFull))
plot(waistcirc,studres(modelFull))
plot(hipcirc,studres(modelFull))
plot(elbowbreadth,studres(modelFull))
plot(kneebreadth,studres(modelFull))
plot(anthro3a,studres(modelFull))
plot(anthro3b,studres(modelFull))
plot(anthro3c,studres(modelFull))
plot(anthro4,studres(modelFull))
#Indicates Anthro3b,anthro4,anthro3a and hipcirc may be Heteroscedastic
pairs(bodyfatwomen)
#that's a mouthful... We can see that some variables are correalted, all the antrho variables are closely related
# as is wast- and hip circumferance.
#We also see that some variables seem to not be very closely related to the response, those being:
#age and elbowwidth.
#Because the scatterplot in general seem very closely interrelated however, it may be unwise to draw firm conlcusions from them so early.

modelPOIS <- lm(DEXfat ~ age  + sqrt(hipcirc) + elbowbreadth + kneebreadth + sqrt(anthro3a) + sqrt(anthro3b) + anthro3c + sqrt(anthro4) ,data=bodyfatwomen2)
#Well that did absolutely nothing.

modellogtrans <- lm(log(DEXfat) ~ age  + hipcirc + elbowbreadth + kneebreadth + anthro3a + anthro3b + anthro3c +anthro4 ,data=bodyfatwomen)
#Residual vs fit in full model indicates response should be log transformed. Better. Tail still present in Residual vs Fitted. QQ plot of transformed value indicated this may be because of outlier point #27 that should be further investigated.
#Constant variance in response.

summary(modellogtrans)
#R squared improved. Continues to suggest hipcirc and kneebreadth is important. As well as age,hipcirc, and anthro3c. Rest may not be (as) important.
plot(anthro3b,studres(modelFull))
plot(anthro3b,studres(modellogtrans))
plot(anthro4,studres(modelFull))
plot(anthro4,studres(modellogtrans))
plot(anthro3a,studres(modelFull))
plot(anthro3a,studres(modellogtrans))
plot(hipcirc,studres(modelFull))
plot(hipcirc,studres(modellogtrans))
#Variable plots indicate the problem of Heteroscedasticity was due to the needed logtransform and is now fixed. No further transformations seem required and we will use the transformed model for futher analyses.
library(car)
vif(modellogtrans)
#All of the 'anthro-' variables indicate some problem with mulicollinearity. anthro4 is especially high.


#Since the number of variables is relatively low, we may use All Possible regressions to see if we can confidently disperse with some unneeded variable.
library(leaps)
leaps <- regsubsets(log(DEXfat) ~ age  + hipcirc + elbowbreadth + kneebreadth + anthro3a + anthro3b + anthro3c +anthro4 ,data=bodyfatwomen)
plot(leaps,scale="adjr2")
#The various variables included plotted against the R2 value it produces. indicates that anthro3b, elbowwidth and anthroa3a may be superflous, and perhaps also age.
subsets(leaps, statistic="adjr2")
#The numbers of variabls against R2 demonstrate the same thing.
plot(leaps,scale="Cp")
#The marlow CP suggests the same as the combination as the one inferred from the R2 diagnostic, which is using Age, hipcirc, kneebreatdth,anthro3c and anthro4. With dropping the age variable as a close second.
plot(leaps,scale="bic")
#The Bic punishes not as important regressons more heavily than Cp and R2, and thus suggests to also drop the variable age.
#The optimal choice for variables appears to be between [Age, hipcirc, kneebreatdth,anthro3c,anthro4] and [hipcirc, kneebreatdth,anthro3c,anthro4].
#Which one to use could practically be decided be investigating if there is a reasonable physical reason to believe age to be an imortant varibale.
#In this case it seems doubtful and since the benifit of adding age is marginal even in the R2 and Cp sence, age is also excluded.
modellogtransreduced <- lm(log(DEXfat) ~  hipcirc  + kneebreadth + anthro3c +anthro4 ,data=bodyfatwomen)
summary(modellogtransreduced)
modellogtransreduced2 <- lm(log(DEXfat) ~  hipcirc  + kneebreadth + anthro3c + anthrocomb ,data=bodyfatwomen)
#We see that the R2 value is essentailly unchanged, which supports the decicion to remove the variables. All the variables can now also be seen to be highly important.
summary(modellogtrans)
summary(modellogtransreduced2)
vif(modellogtransreduced)
vif(modellogtransreduced2)
#All of the VIF are now below 10, indicating that the issue of mulicollinearity was resolved by dropping the unnecessary variables.
#No biased estimator in an attempt to reduce the variance seems necessary.
plot(modellogtransreduced)
#The residual plots still look mostly the same, and demonstrate mostly the same qualities as before.
#There is still a very defined outlier present in point 27 that disrupts the normal probability qualities of the residuals.
#That needs to be investigated.
#also points, 23,18 and 63 are suspect due to their large residuals.
#since we have 4 variables and 71 observations the hat diagonal value needs to exceed (2*p)/n=2*4/71=0.1126761
print(influence.measures(modellogtransreduced))
print(influence.measures(modellogtransreduced2))
#The hat values indicates that there are no leverage points, thought some are close.
#The coocks value are all within the limit. As is suggested by the residuals vs leverage plot. (So point 27 may not be as inflential as we thought?)
#The formal cutoff for COVRATIO is in this case 0.8309,1.169. 
#The points within this limit are, 2,9,18,23,27,30,36. With 27 being by far the worst offender (0.45)

##We remove point 27 and investigate the results.

modellogtransreducedremoved <- lm(log(DEXfat[-27]) ~  hipcirc[-27]  + kneebreadth[-27] + anthro3c[-27] +anthro4[-27] ,data=bodyfatwomen)
modellogtransreducedremoved2 <- lm(log(DEXfat[-27]) ~  hipcirc[-27]  + kneebreadth[-27] + anthro3c[-27] +anthrocomb[-27]  ,data=bodyfatwomen)
modellogtransreducedremovedage <- lm(log(DEXfat[-27]) ~  hipcirc[-27]  + kneebreadth[-27] + anthro3c[-27] +anthro4[-27] +age[-27] ,data=bodyfatwomen)
modellogtransreducedremovedage2 <- lm(log(DEXfat[-27]) ~  hipcirc[-27]  + kneebreadth[-27] + anthro3c[-27] +anthrocomb[-27] +age[-27],data=bodyfatwomen)
plot(modellogtransreducedremoved, main="variable 27 removed")

summary(modellogtransreducedremoved)
#Removing value 27 increases the R2 value significantly and reducing the Msq error, indicating removing the point increased the quality of the plot.
#Since there are several reasons a bad point could have happened (The person meassured displayed some characterstics that made his body unsuitable for meassure, physical disabilities etc.)
#we may with confidence deem it a "bad point" and remove it. The rest of the outliers weren't nearly as odd and are left in the model.
anova(modellogtransreducedremoved)
vif(modellogtransreducedremoved)
plot(modellogtransreduced2)
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(coef(fit)) 
}
results <- boot(data=bodyfatwomen, statistic=bs ,R=100, formula=log(DEXfat[-27]) ~  hipcirc[-27]  + kneebreadth[-27] + anthro3c[-27] +anthro4[-27])
results

boot.ci(results, type="bca", index=1)
boot.ci(results, type="bca", index=2)
boot.ci(results, type="bca", index=3)
boot.ci(results, type="bca", index=4)
boot.ci(results, type="bca", index=5)
confint(modellogtransreducedremoved)
#Since the data used contains relatively few data points it may not be accurate to assume it is nomrally distrubted, 
hist(age)
hist(hipcirc)
#we may therefor choose
#to simulate more using bootstrap methods in order to try and obtain more
#accurate estimations. We note that the CI for bootstrat are slightly wider than that
#of the normal simulated ones and with some bias.

resid <- resid(modellogtransreducedremoved)
pr <- resid(modellogtransreducedremoved)/(1 - lm.influence(modellogtransreducedremoved)$hat)
#Calculates the prediction error for the obversations.
resid2 <- resid(modellogtransreduced)
pr2 <- resid(modellogtransreduced)/(1 - lm.influence(modellogtransreduced)$hat)
#We also see here that the observation 27 deemed as "bad" has a very high PRESS residual, indicating poor predictions at this point.
PRESS(modellogtransreducedremoved)
PRESS(modellogtransreduced)
#We see that the PRESS statistic reduced heavily with removal of point 27. Reinofrcing our decicion to remove it
PRESS(modelFull)
PRESS(modellogtrans)
#.. And that the PRESS reduced with subsequent models.


anthrocomb <- I(anthro3a*anthro3b*anthro4)
modellogtransreducedremoved
modellogtransreduced
vif(modellogtransreduced)
vif(modellogtransreduced2)
vif(modellogtransreducedremoved)
vif(modellogtransreducedremoved2)
BIC(modellogtransreduced)
BIC(modellogtransreduced2)
BIC(modellogtransreducedremoved)
BIC(modellogtransreducedremoved2)
BIC(modellogtransreducedremovedage)
BIC(modellogtransreducedremovedage2)
PRESS(modellogtransreducedremoved)
PRESS(modellogtransreducedremoved2)
PRESS(modellogtransreducedremovedage)
PRESS(modellogtransreducedremovedage2)

summary(influence.measures(modellogtransreduced))

ss.tot <- sum( (bodyfatwomen$DEXfat - mean(bodyfatwomen$DEXfat)) ^ 2)


#modellogtransreducedremoved
ssT <- (7.5662+0.0844+1.6165+0.1201+0.5811)
R2pred <- 1-(PRESS(modellogtransreducedremoved)/ssT)
R2pred


SST2 <- (7.5662+0.0844+1.6165+0.0956+0.6056)
R2pred2 <- 1-(PRESS(modellogtransreducedremoved2)/SST2)
R2pred2

SST3 <- (7.5662+0.0844
R2pred2 <- 1-(PRESS(modellogtransreduced)/SST2)
R2pred2



7.6479
