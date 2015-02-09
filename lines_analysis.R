#Introduction to R

#Set working Directory
setwd("/Users/emanuelschorsch/Documents/Baseball")

#####Reading Data#####
data2<-read.csv("processed-blines.csv", header=T)

#predict win from line
mylogit<-glm(data2$win~data2$odds, family="binomial")
predicted<-predict(mylogit)
error.rate <- (predicted>0 & mylogit$y==0) | (predicted<0 & mylogit$y==1)
summary(error.rate)

margin <- .05
vegas.error <- (data2$odds>(.5+margin) & data2$win == F) | (data2$odds<(.5-margin) & data2$win == T)
qualified <- data2$odds>(.5+margin) | data2$odds<(.5-margin)
summary(vegas.error)
summary(qualified)
#error rate:
sum(vegas.error)/sum(qualified)


#X.4 (data[5]) is line, X.5 is run line
data_even<-subset(data2, odds == .50) #&& X.7 == -110 )
#data_pos<-subset(data_even, odds > .5)
#data_even<-subset(data_even, odds < .5) #neg
total<-data_even$total #the over under
exp<-total/2
model<-lm(runs~exp+run.line.runs+run.line.odds+odds, data=data_even)
summary(model)

#predicting fraction of runs out of total
y<-data2$runs/data2$total
true_y<-data2$runs/(data2$runs+data2$o.runs)
odds<-data2$odds
model<-lm(true_y~odds)
summary(model)
model<-lm(true_y~odds+data2$total)
summary(model)
model<-lm(data2$runs~odds+data2$total+data2$total.odds)
summary(model)

#gives an idea of what runs scored looks like
cor(data_even$runs, data_even$total)
plot(data_even$runs-(data_even$total/2))
summary(data_even$runs-(data_even$total/2))

#visualization
plot(density(data2$runs)) # returns the density data 
plot(density(data2$o.pa))

hex_plot(data2$odds, true_y)

# Are runs correlated to predicted total?
hex_plot(data2$runs, data2$guesses)
#####lm() function#####

#Plot Regression Line
abline(model$coefficients[[1]],model$coefficients[[2]])


#predicts rbi decently, with an adj R^2 of approx .05717
#RMSE of 2.797
model<-lm(rbi~odds+total+total.odds+run.line.runs+run.line.odds, data=data2)
summary(model)

#predicts rbi decently, with an adj R^2 of approx .0564
model<-lm(data2$rbi~data2$odds+data2$total+data2$total.odds)
summary(model)
#!!!! RBI and PA have same problem of non-normality
qqnorm(model$residuals)
qqline(model$residuals)

#for LOESS R^2 is down at .0545
model<-loess(data2$rbi~data2$odds+data2$total+data2$total.odds)
summary(model)
cor(data2$rbi, fitted(model))^2

#total.odds doesn't add that much. This model has adj R^2 of .0551
model<-lm(data2$rbi~data2$odds+data2$total)
summary(model)

#predicts pa poorly, with an R^2 of approx .0176
model<-lm(pa~odds+total+total.odds+pa.mean, data=data2)
summary(model)
#plot(model)
qqnorm(model$residuals)
qqline(model$residuals)
#qqplot(model$residuals, distribution=normal())
#!!! qqplot shows clearly not normal!!!!
model<-loess(pa~odds+total+total.odds+pa.mean, data=data2)
summary(model)
#R^2 is up to .0195 with LOESS
cor(data2$pa, fitted(model))^2


data2$run_total <- data2$runs + data2$o.runs
data3<-subset(data2, inns==9)
mean_plot(data2$total, data2$run_total)
mean_plot(data3$total, data3$run_total)

par(bg="cornsilk")
n <- 10
g <- gl(n, 100, n*100)
x <- rnorm(n*100) + sqrt(as.numeric(g))
boxplot(split(data2$run_total,data2$total), col="lavender", notch=TRUE)

## Conditioning plots
#par(bg="cornsilk")
coplot(lat ~ long | depth, data = quakes, pch = 21, bg = "green3")

##########
#try out the Poisson Regression
#########
model1<-glm(data2$pa~data2$odds+data2$total, family=poisson)
model<-glm(data2$pa~data2$odds+data2$total+data2$total.odds+data2$pa.mean, family=poisson)
summary(model)
1-(model$deviance/model$null.deviance)
### var(model$residuals) == .02 


#predictive R^2
pr <- residuals(model)/(1 - lm.influence(model)$hat)
PRESS <- sum(pr^2)
PRESS

# anova to calculate residual sum of squares
my.anova <- anova(model)
tss <- sum(my.anova$"Sum Sq")
# predictive R^2
pred.r.squared <- 1 - PRESS/(tss)
pred.r.squared


#########
#Trying out rms package
##########
d<-datadist(data2$pa)
options( datadist = NULL )
#model1<-Glm(pa~odds+total, data=data2, family=poisson())
#model2<-Glm(pa~odds+total+total.odds+pa.mean, data=data2, family=poisson())
#modelRBI<-Glm(rbi~odds+total+total.odds, data=data2, family=poisson())
summary(model2)
#does this tell us what we want?
#This seems to indicate the R^2 is .0176 which is terrible.
cor(predict(model2), data2$pa)^2
#for RBI its .0567
cor(predict(modelRBI), data2$rbi)^2

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

#######
#Fitting the right distribution
#link1 (super useful!!): http://www.r-project.org/conferences/useR-2009/slides/Delignette-Muller+Pouillot+Denis.pdf
#
#######
library(fitdistrplus)
#seems like rbi is close to a gamma distribution
descdist(data2$rbi,boot=1001)
#Try to fit a gamma
fg.mle<-fitdist(data2$rbi,"gamma",method="mme")
summary(fg.mle)
plot(fg.mle)

#seems like rbi is somewhere between negative binomial and poisson
descdist(data2$rbi, discrete=TRUE)

#fit it
fnbinom<-fitdist(data2$rbi,"nbinom")
plot(fnbinom)

#much worse
fnbinom2<-fitdist(data2$rbi,"pois")
plot(fnbinom2)

#x.wei<-rweibull(n=40000,shape=2.1,scale=1.1) ## sampling from a Weibull
#distribution with parameters shape=2.1 and scale=1.1
x.teo2<-rnbinom(n=9625,size=4, prob=1/(2.03))
x.teo<-rnbinom(n=9625,size=4, mu=4.13) ## theorical quantiles from a
#Weibull population with known paramters shape=2 e scale=1
qqplot(x.teo2, data2$rbi,main="QQ-plot distr. Weibull") ## QQ-plot
abline(0,1) ## a 45-degree reference line is plotted

#########
#Now for PA
model<-lm(data2$pa~data2$odds+data2$total+data2$total.odds+data2$pa.mean)
summary(model)

model2<-loess(data2$pa~data2$odds+data2$total+data2$total.odds+data2$pa.mean)
summary(model2)
#R^2 is up to .0195 with LOESS
descdist(model2$residuals,boot=1001)


descdist(data2$pa,boot=1001)
descdist(data2$pa, boot=1001, discrete=T)

library(actuar)
fnbinom2<-fitdist(data2$pa,"burr",start = list(shape1 = 1, shape2=2))
plot(fnbinom2)

#start = list(c = 1, k = 1), # need to provide named list of starting values
#lower = list(c = 0, k = 0)


#Looks like total is kinda normal
descdist(data2$total,boot=1001)
descdist(data2$total, boot=1001, discrete=T)

#run_total isn't though
descdist(data2$run_total,boot=1001)
descdist(data2$run_total, discrete=T)



#now to check the residuals
#Good news is residuals follow basically the same distribution as the original data
#That probably makes sense since the linear regression won't distort the distribution too much
model<-lm(data2$pa~data2$odds+data2$total+data2$total.odds+data2$pa.mean)
descdist(model$residuals, boot=1001)
model<-lm(rbi~odds+total+total.odds+run.line.runs+run.line.odds, data=data2)
descdist(model$residuals, boot=1001)


