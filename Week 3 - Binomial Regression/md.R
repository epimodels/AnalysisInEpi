### Preliminary Loading, Data Intake, Etc. ###
set.seed(8675309)
# Load Libraries
library(Amelia)
library(snow)
library(sandwich)
library(lmtest)

mers <- read.csv("mi_mers.csv")

b.out <- NULL
se.out <- NULL

poisson_missing <- glm(Fatal ~  Male + Age + Saudi + HCW + Comorbidity, data=mers, family=poisson,trace=TRUE)
summary(poisson_missing)
b.out <- rbind(b.out,poisson_missing$coef)
se.out <- rbind(se.out, coeftest(poisson_missing,vcov=sandwich)[,2])

sex_beta <- (b.out[2])
sex_se <- se.out[2]
sex_RR <- exp(sex_beta)
sex_LCL <- exp(sex_beta - (1.96*sex_se))
sex_UCL <- exp(sex_beta + (1.96*sex_se))

cat("Adjusted RR :", round(sex_RR,2), "95% CI: ", round(sex_LCL,2),",",round(sex_UCL,2))

mi.mers <- amelia(mers, m=1000, ords=c("Fatal"), idvars=c("Number"),parallel="snow",ncpus=4, p2s=2)

b.out <- NULL
se.out <- NULL

for(i in 1:mi.mers$m){
  model <- glm(Fatal ~  Male + Age + Saudi + HCW + Comorbidity, family=poisson, data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,model$coef)
  se.out <- rbind(se.out, coeftest(model,vcov=sandwich)[,2])
}
combined <- mi.meld(q = b.out, se = se.out)
print(combined)

sex_beta <- combined[[1]][2]
sex_se <- combined[[2]][2]
sex_RR <- exp(sex_beta)
sex_LCL <- exp(sex_beta - (1.96*sex_se))
sex_UCL <- exp(sex_beta + (1.96*sex_se))

cat("Adjusted RR :", round(sex_RR,2), "95% CI: ", round(sex_LCL,2),",",round(sex_UCL,2))
