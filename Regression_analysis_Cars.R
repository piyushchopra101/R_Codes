library(MASS)
data("mtcars")
mydata<-mtcars

# Exploratory data Analysis
# Convert categorical variables into factors so that it would be effective in our analysis.

names(mtcars)<-c("Miles_per_gallon","Number_of_cylinders","Displacement","horsepower","Rear_axle_ratio","Weight","quarter_mile_time","V/S","Transmission","Number_gears","Number_carburetors")
mtcars$Number_of_cylinders<-as.factor(mtcars$Number_of_cylinders)
mtcars$Transmission <- factor(mtcars$Transmission)
mtcars$Number_of_cylinders <- factor(mtcars$Number_of_cylinders)
mtcars$Number_gears<- factor(mtcars$Number_gears)
mtcars$Number_carburetors <- factor(mtcars$Number_carburetors)
mtcars$`V/S`<- factor(mtcars$`V/S`)
levels(mtcars$Transmission) <- c("automatic", "manual")

## Check for normality of the data
shapiro.test(mtcars$Miles_per_gallon)

# Performing t-test
t.test(mtcars$Miles_per_gallon ~ mtcars$Transmission)

# Do a co-relation of the dataset to understand the co-relations between different variables.

cor(mydata)

# Analysis of variance test to figure out the significant factors
# This would help in better model building.
anova_analysis<- aov(mtcars$Miles_per_gallon~., data=mtcars)
summary(anova_analysis)


#Fitting Models 
# Here I fit different Regression  models to figure out which models are 
# accurate and give us a better understanding of the relationship.
# I check each of the individual signifant variables effect individually on mpg
# and then a combination of factors. The model with the lowest R-sqaured and best adj.R-sqaured value is chosen.
# The chosen model is also checked for its statistical significance.
attach(mtcars)
fit0<- lm(Miles_per_gallon~.,data=mtcars)
fit1 <- lm(Miles_per_gallon ~ Number_of_cylinders, data = mtcars)
fit2 <- lm(Miles_per_gallon ~ Weight, data = mtcars)
fit3 <- lm(Miles_per_gallon  ~ Number_of_cylinders+ Weight, data = mtcars)
fit4 <- lm(Miles_per_gallon  ~ Displacement, data = mtcars)
fit5 <- lm(Miles_per_gallon  ~ Displacement + Number_of_cylinders, data = mtcars)
fit6 <- lm(Miles_per_gallon ~ Displacement + Weight, data = mtcars)
fit7 <- lm(Miles_per_gallon ~ Displacement + Number_of_cylinders + Weight, data = mtcars)
fit8 <- lm(Miles_per_gallon ~ Displacement + Number_of_cylinders + Weight + Transmission ,data = mtcars)
fit9 <- lm(Miles_per_gallon ~ Transmission ,data = mtcars)

summary(fit3)$coefficients
summary(fit7)$coefficients
summary(fit8)$coefficients

# From our results the models 3,7 and 8 fit best the regression.their adj-R-squared values are
summary(fit3)$adj.r.squared
summary(fit7)$adj.r.squared
summary(fit8)$adj.r.squared

#comparing the coefficients of model 7 and 8 there is only a slight difference
# The mpg increases by 0.15 miles when the transmission mode is changed from automatic to manual.

anova(fit0,fit3)
anova(fit0,fit7)
anova(fit0,fit8)

boxplot(Miles_per_gallon~ Transmission, data = mtcars, xlab = "Transmission type", ylab = "Miles per gallon")
# The boxplot shows that there is mean difference in the miles per gallon depending on transmission type

