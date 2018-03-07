library(readr)
titanic <- read_csv("titanic.csv")

# Explore the data set
table(titanic$Sex)
table(titanic$Age)
table(titanic$Class)

# Calculate the crude probability of survival
raw <- glm(Survived ~ 1,family=binomial(link='logit'),data=titanic)

summary(raw)
raw_int <- summary(raw)$coefficients[1]
raw_p <- exp(raw_int)/(1+(exp(rawe_int)))
print(raw_p)

# Estimate the effect of sex on survival
sex_surv <- glm(Survived ~ Sex,family=binomial(link='logit'),data=titanic)
summary(sex_surv)
crude_coef <- summary(sex_surv)$coefficients[2,1]
crude_se <- summary(sex_surv)$coefficients[2,2]

crude_OR <- exp(crude_coef)
crude_LCL <- exp(crude_coef-(1.96*crude_se))
crude_UCL <- exp(crude_coef+(1.96*crude_se))
cat("Crude OR :", crude_OR, "95% CI: ", crude_LCL,",",crude_UCL)

# Should we adjust for age?
age_adj <- glm(Survived ~ Sex + Age,family=binomial(link='logit'),data=titanic)
summary(age_adj)
ageadj_coef <- summary(age_adj)$coefficients[2,1]
age_se <- summary(age_adj)$coefficients[2,2]
change <- abs(crude_coef - ageadj_coef)
print(change)

# Should we adjust for class?
class_adj <- glm(Survived ~ Sex + Class,family=binomial(link='logit'),data=titanic)
summary(class_adj)
classadj_coef <- summary(class_adj)$coefficients[2,1]
class_se <- summary(class_adj)$coefficients[2,2]
change2 <- abs(crude_coef - classadj_coef)
print(change2)

# Plot some of these
library("ggplot2") #remember to install.packages("ggplot2")
y <- c(crude_coef,ageadj_coef,classadj_coef)
x <- c("Crude","Age Adjusted","Class Adjusted")
se <- c(crude_se,age_se,class_se)
qplot(x,exp(y),xlab="Model",ylab="Survival Odds Ratio (Men vs. Women",size=I(5))+geom_errorbar(aes(x=x,ymin=exp(y-1.96*se),ymax=exp(y+1.96*se),width=0.25))
