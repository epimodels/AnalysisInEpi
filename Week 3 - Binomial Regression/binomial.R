library(readr)
library(sandwich)
library(lmtest)

titanic <- read_csv("titanic.csv")

# Logistic Regression of Survival by Sex
logistic <- glm(Survived ~ Sex,family=binomial(link='logit'),data=titanic)
summary(logistic)

bin <- glm(Survived ~ Sex,family=binomial(link='log'),data=titanic)
summary(bin)

# Note the similar AICs, residual deviance, etc. Both *fit* the model equally, they're just estimating different things

sex <- glm(Survived ~ Sex,family=binomial(link='log'),data=titanic,trace=TRUE)

bin3 <- glm(Survived ~ Sex + Age,family=binomial(link='log'),data=titanic,trace=TRUE)

age <- glm(Survived ~ Age,family=binomial(link='log'),data=titanic,trace=TRUE)
summary(sex)
summary(age)

start <- c(-1.16,-1.239,0.51447)
age_sex <- glm(Survived ~ Sex + Age,family=binomial(link='log'),data=titanic,trace=TRUE,start=start)
summary(age_sex)

start2 <- c(-1,-1.2,0.5)
age_sex2 <- glm(Survived ~ Sex + Age,family=binomial(link='log'),data=titanic,trace=TRUE,start=start2)
summary(age_sex2)

start3 <- c(-1,0,0)
age_sex3 <- glm(Survived ~ Sex + Age,family=binomial(link='log'),data=titanic,trace=TRUE,start=start3)
summary(age_sex3)

# Now lets try a full model
class <- glm(Survived ~ Class,family=binomial(link='log'),data=titanic)
summary(class)

start_full <- c(-1,-1.239,0.514,0.958,0.547,0.051)
full <- glm(Survived ~ Sex + Age + Class,family=binomial(link='log'),data=titanic,trace=TRUE,start=start_full)

# There must be a better way...

b.out <- NULL
se.out <- NULL

poisson_full <- glm(Survived ~ Sex + Age + Class, data=titanic, family=poisson,trace=TRUE)
b.out <- rbind(b.out,poisson_full$coef)
se.out <- rbind(se.out, coeftest(poisson_full,vcov=sandwich)[,2])
summary(poisson_full)

sex_beta <- (b.out[2])
sex_se <- se.out[2]
sex_RR <- exp(sex_beta)
sex_LCL <- exp(sex_beta - (1.96*sex_se))
sex_UCL <- exp(sex_beta + (1.96*sex_se))

cat("Adjusted RR :", sex_RR, "95% CI: ", sex_LCL,",",sex_UCL)

start_full <- c(-0.29514,-1.19174,0.48028,0.30396,-0.07253,-0.46057)
full <- glm(Survived ~ Sex + Age + Class,family=binomial(link='log'),data=titanic,trace=TRUE,start=start_full)