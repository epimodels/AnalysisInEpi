library(readr)
library(survival)

# Read in the data set
leu <- read_csv("survival.csv")

# Create the "survival object" - this will be used in *many* future stages
trialtimes <- Surv(leu$time, leu$relapse)

# Fit an unadjusted K-M curve
fit_raw <- survfit(trialtimes ~ 1)
plot(fit_raw)

# Fit different KM curves by treatment and controls
fit_treatment <- survfit(trialtimes ~ leu$treated)

plot(fit_treatment,conf.int=FALSE,col=c("red","blue"),mark.time=TRUE,xlab="Weeks",ylab="Proportion Recurring")
legend("topright", c("Treated","Untreated"), lwd=3, col=c("blue","red"),
       lty=c(1,1), pch=c(NA,NA), bty='n')

trial_p <- survdiff(trialtimes ~ leu$treated)

# Fit a Cox PH model
coxmodel <- coxph(trialtimes ~ leu$treated)
summary(coxmodel)

# Test of the proportional hazard assumption
prophaz <- cox.zph(coxmodel)
prophaz
plot(prophaz)

# Parametric Survival models
summary(survreg(trialtimes ~ leu$treated, dist="exponential"))

summary(survreg(trialtimes ~ leu$treated, dist="weibull"))