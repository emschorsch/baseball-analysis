#Introduction to R

#Set working Directory
setwd("/Users/emanuelschorsch/Documents/Baseball")

#####Reading Data#####
data2<-read.csv("processed-blines.csv", header=T)
data2$run_total <- data2$runs + data2$o.runs
data3<-subset(data2, inns==9)



##########
#try out the Poisson Regression
#########
model1<-glm(data2$pa~data2$odds+data2$total, family=poisson)
model<-glm(data2$pa~data2$odds+data2$total+data2$total.odds+data2$pa.mean, family=poisson)
summary(model)
1-(model$deviance/model$null.deviance)
### var(model$residuals) == .02 

press(model)


#The cv package
model1a<-glm(pa~odds+total, data=data2, family=poisson())
model2a<-glm(pa~odds+total+total.odds+pa.mean, data=data2, family=poisson())

library(MASS)
modelRBI<-glm.nb(rbi~odds+total+total.odds, data=data2)
summary(modelRBI)
#.0565
cor(data2$rbi, fitted(modelRBI))^2
#Is this a good metric for good fit?
#Is this RMSE? 2.797
#problem is residuals are in terms of the linear model and not the exponentiated model
sqrt(mean((data2$rbi - fitted(modelRBI2))^2))
# MAE is 1.477
sqrt(mean(abs(data2$rbi - fitted(modelRBI2))))
#baseline RMSE 2.882
sqrt(mean((data2$rbi - data2$temp)^2))
#baseline MAE is 1.501
sqrt(mean(abs(data2$rbi - data2$temp)))
sd(data2$rbi - fitted(modelRBI2))

modelRBI2<-glm.nb(rbi~odds+total+total.odds+run.line.runs+run.line.odds, data=data2)
summary(modelRBI2)
#.0578 prediction all significant though
cor(data2$rbi, fitted(modelRBI2))^2

m3 <- update(modelRBI2, . ~ . - run.line.runs)
#anova shows that run.line.runs is a significant predictor
anova(modelRBI2, m3)

m4 <- glm(rbi~odds+total+total.odds+run.line.runs+run.line.odds, data=data2)
# chissq strongly suggests that negative binomial is better
pchisq(2 * (logLik(modelRBI2) - logLik(m4)), df = 1, lower.tail = FALSE)

(est <- cbind(Estimate = coef(modelRBI2), confint(modelRBI2)))
#shows us our confidence intervals in an interpretable manner
exp(est)


cv.err <- cv.glm(data2, model2, K=10)


