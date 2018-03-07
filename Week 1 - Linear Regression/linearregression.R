## Data Importation and Setup ##

# DONT FORGET TO SET YOUR WORKING DIRECTORY

# Import the libraries we'll need
library(readr)
library(ez)

# Read in a file from CSV
births <- read_csv("poverty.csv")

## Visualizing the Data ##

# Histogram of %-age living in poverty
hist(births$Pov)

# Histogram of teen birth date
hist(births$Birth)

# Plot Teen Birth Rate by Poverty
plot(births$Pov,births$Birth)

# Lets make this plot a little prettier...
plot(births$Pov,births$Birth, xlab="Percentage Below Poverty Line",ylab="Teen Birth Rate (Births per 1000 Women)")

## Regression, Fitting and Visualization ##

# Simple Line Fit
simple <- lm(Birth ~ Pov,data=births)
summary(simple)

# Visualize that Fit
new_pov<- seq(min(births$Pov), max(births$Pov), length.out=100)
preds <- predict(simple, newdata = data.frame(Pov=new_pov), interval = 'confidence')

plot(births$Pov,births$Birth, xlab="Percentage Below Poverty Line",ylab="Teen Birth Rate (Births per 1000 Women)")
lines(new_pov,preds[,1],lty=1,col="black",lwd=3)
lines(new_pov,preds[,2],lty=2,col="red",lwd=2)
lines(new_pov,preds[,3],lty=2,col="red",lwd=2)

legend("topleft",c("Linear Fit","95% CI"),lwd=c(3,2),lty=c(1,2),col=c("Black","Red"),bty='n')

# Adding a Squared Term
sqr <- lm(Birth ~ Pov + I(Pov^2),data=births)
summary(sqr)

preds2 <- predict(sqr, newdata = data.frame(Pov=new_pov), interval = 'confidence')

plot(births$Pov,births$Birth, xlab="Percentage Below Poverty Line",ylab="Teen Birth Rate (Births per 1000 Women)")
lines(new_pov,preds2[,1],lty=1,col="black",lwd=3)
lines(new_pov,preds2[,2],lty=2,col="red",lwd=2)
lines(new_pov,preds2[,3],lty=2,col="red",lwd=2)