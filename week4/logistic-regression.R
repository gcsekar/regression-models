download.file("https://dl.dropboxusercontent.com/u/7710864/data/ravensData.rda", 
              destfile = "./week4/data/ravensData.rda", method = "curl")

load("./week4/data/ravensData.rda")
head(ravensData)

# To predict Ravens Win the Linear equation won't hold true
#  RW = b0+ b1 * RSi + ei

lmRavens <- lm(ravensData$ravenWinNum ~ ravensData$ravenScore)
summary(lmRavens)$coef

#                      Estimate  Std. Error  t value   Pr(>|t|)
#(Intercept)           0.28503172 0.256643165 1.110615 0.28135043
#ravensData$ravenScore 0.01589917 0.009058997 1.755069 0.09625261

# Linear vs. Logistic Regression
# 
# LINEAR
# 
# RW(i) = b0 + b1RS(i) + e(i)
# Where RW represents Ravens Win, RS represents Raven Score
#
# OR
#
# E[RW(i)|RS(i), b0, b1] = b0 + b1RS(i)
#
# LOGISTIC
# 
# Pr(RW(i)|RS(i), b0, b1) = exp(b0 + b1RS(i)) / (1 + exp(b0 + b1RS(i)))
#
# OR
#
# log(Pr(RW(i)|RS(i), b0, b1)/(1-Pr(RW(i)|RS(i), b0, b1)))  =   b0 + b1RS(i)
#
#

## VISUALLY FITTING LOGISTIC REGRESSION CURVES

#
library(manipulate)
x <- seq(-10,10, length =1000)
manipulate::manipulate(
  plot(x, exp(beta0 + beta1 * x)/(1+exp(beta0 + beta1 * x)), type="l",
       lwd=3, frame=FALSE),
  beta0 = slider(-2, 2, step=.1, initial=2),
  beta1 = slider(-2, 2, step=.1, initial=0)
  
)

## Let's fit the Raven's data

logRegRavens <- glm(ravensData$ravenWinNum ~ ravensData$ravenScore, family = "binomial")
summary(logRegRavens)

plot(ravensData$ravenScore, logRegRavens$fitted, pch=19, col='blue',
     xlab='Score',ylab="Prob Ravens Win")

anova(logRegRavens, test="Chisq")