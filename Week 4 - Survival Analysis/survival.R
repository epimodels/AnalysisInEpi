library(readr)
library(survival)
library(flexsurv)
library(survminer)

# Read in the data set
leu <- read_csv("survival.csv")
leu$treated2 <- as.factor(leu$treated)

# Create the "survival object" - this will be used in *many* future stages
trialtimes <- Surv(leu$time, leu$relapse)

# Fit an unadjusted K-M curve
fit_raw <- survfit(trialtimes ~ 1)
plot(fit_raw)

# Fit different KM curves by treatment and controls
fit_treatment <- survfit(Surv(time, relapse) ~ treated, data=leu)

ggsurvplot(fit_treatment)
ggsurvplot(fit_treatment,risk.table=TRUE)
ggsurvplot(fit_treatment,pval=TRUE)
ggsurvplot(fit_treatment,conf.int=TRUE)

trial_p <- survdiff(trialtimes ~ leu$treated)

# Fit a Cox PH model
coxmodel <- coxph(Surv(time, relapse) ~ treated, data=leu)
summary(coxmodel)

# Test of the proportional hazard assumption
prophaz <- cox.zph(coxmodel)
prophaz
ggcoxzph(prophaz)

ggcoxdiagnostics(coxmodel,type="schoenfeld")

# Parametric Survival models
expo <- flexsurvreg(Surv(time, relapse) ~ treated2, data=leu, dist="exponential")
expo
plot(expo)
ggflexsurvplot(expo,risk.table=TRUE)

wei <- flexsurvreg(Surv(time, relapse) ~ treated2, data=leu, dist="weibull")
wei
ggflexsurvplot(wei)
