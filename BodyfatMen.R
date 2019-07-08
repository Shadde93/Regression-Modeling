setwd("C:/Users/Shadman/Desktop/Regression/pr1")
bodyfatmen <- read.csv(file="bodyfatmen.csv", header=TRUE, sep=",")

model <- lm(density ~ age  + weight + height + neck + chest + abdomen + hip + thigh + knee+ ankle + biceps + forearm + wrist  ,data=bodyfatmen)

mean(model$residuals)


#residual scale and plot

plot(model)
#outliers, levarge, influential observ
cooks.distance(model)

covratio(model)


plot(density,studres(modelFull))

#transform
modelLog <- lm(log(density) ~ age  + weight + height + neck + chest + abdomen + hip + thigh + knee+ ankle + biceps + forearm + wrist  ,data=bodyfatmen)
modelSqrt <- lm(sqrt(density) ~ age  + weight + height + neck + chest + abdomen + hip + thigh + knee+ ankle + biceps + forearm + wrist  ,data=bodyfatmen)
modelRece <- lm(1/(density) ~ age  + weight + height + neck + chest + abdomen + hip + thigh + knee+ ankle + biceps + forearm + wrist  ,data=bodyfatmen)

plot(modelLog)
plot(modelSqrt)
plot(modelRece)


summary(modelLog)
summary(model)
summary(modelSqrt)
summary(modelRece)

#Multicollin
library(car)
vif(model)


a <- data.matrix(bodyfatmen)
a <- a[,-1]
X <- cor(a)

ev <- eigen(X)
max(ev$values)/min(ev$values)
kappa(X, exact = TRUE)

#handle multicollin
library(MASS)
bodyfatScale <- data.frame(scale(bodyfatmen))

lm_seq = seq(0,10,0.01)
modelRidge <- lm.ridge(density ~ age  + weight + height + neck + chest + abdomen + hip + thigh + knee+ ankle + biceps + forearm + wrist  ,data=bodyfatmen, lambda =lm_seq)
select(modelRidge)
modelRidge <- lm.ridge(density ~ age  + weight + height + neck + chest + abdomen + hip + thigh + knee+ ankle + biceps + forearm + wrist  ,data=bodyfatmen, lambda =1.02 )

## variable selection
#AIC
BackwardAIC <- step(model, direction = "backward")
forwardAIC <- step(lm(density ~ 1, data = bodyfatmen), direction = "forward", scope =~age  + weight + height + neck + chest + abdomen + hip + thigh + knee+ ankle + biceps + forearm + wrist)
# adjR2
library(olsrr)

#ols_step_forward_p(model, details = TRUE)
#fowardmodel <- ols_step_forward_p(model)
#plot(fowardmodel)

#ols_step_backward_p(model, details = TRUE)
#backwardmodel <- ols_step_backward_p(model)
#plot(backwardmodel)
library(leaps)
forward <- regsubsets(density~.,data = bodyfatmen, method = "forward", nbest=1)  
backward <- regsubsets(density~.,data = bodyfatmen, method = "backward", nbest=1)

plot(forward, scale = "adjr2", main = "Forward Selection")

plot(backward, scale = "adjr2", main = "Backward Selection")


ForwarR <- lm(density ~ age + weight + neck + abdomen + thigh + biceps+ forearm + wrist, data = bodyfatmen)
summary(ForwarR)

BackwardR <- lm(density ~ age + weight + neck +abdomen + hip + thigh + forearm + wrist, data = bodyfatmen)
summary(BackwardR)

#crossvalid
library(DAAG)
CrossAIC <- cv.lm(bodyfatmen, BackwardAIC, m=20)
CrossR <- cv.lm(bodyfatmen, BackwardR, m=20)

#boots
library(MPV)

par(mfrow = c(2, 3))
sum <- summary(BackwardAIC)
#for (m in ms) {
  coefs <- c()
  for (i in seq(1:900)) {
    n <- nrow(bodyfatmen)
    indices <- sample(n, n, replace = TRUE)
    residual.boot <- sum$residuals[indices]
    y.boot <- BackwardAIC$fitted.values + residual.boot # New bootstrap samples, see (eq. 5.19)
    BackwardAIC.boot <- lm(y.boot ~ bodyfatmen$age  + bodyfatmen$weight + bodyfatmen$neck  + bodyfatmen$abdomen + bodyfatmen$hip + bodyfatmen$thigh  + bodyfatmen$forearm + bodyfatmen$wrist)
    coefs <- rbind(coefs, coef(BackwardAIC.boot))
  }
  
  param.sd.boot <- apply(coefs, 2, sd)
  print(param.sd.boot)
  
  # Also create confidence intervals accoring to the percentile method
  # presented in Section 15.4.2, Montgomery
  s = c("Intercept","age","weight","neck","abdomen","hip","thigh","forearm","wrist")
  conf.ints <- c()
  for (k in 1:9) {
    hist(coefs[, k], main = paste("Histogram of" , s[k] ), xlab = paste("coefs" , s[k] ))
    quants <- quantile(coefs[,k], probs = c(0.025, 0.975))
    beta.est <- coef(BackwardAIC)[k]
    D1 <- beta.est - quants[1]
    D2 <- quants[2] - beta.est
    conf.ints <- rbind(conf.ints, c(beta.est - D2, beta.est + D1, beta.est))
  }
  
  colnames(conf.ints) <- c( names(quants), "beta est")
  rownames(conf.ints) <- names( coef(BackwardAIC))
  conf.ints
  confint(BackwardAIC)
#}



