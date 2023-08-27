#LOAD PACKAGES
x<-c("plyr", "dplyr", "tidyr", "moments","ppcor","ggplot2","hrbrthemes", "car")
lapply(x, require, character.only = TRUE)

#scatterplot, add linear regresison line
ggplot(df, aes(df$Headcirc, df$Birthweight)) +
  geom_point() +
  stat_smooth(method = lm)


#linear regression model
model <- lm(df$Headcirc ~ df$Birthweight, df)


#ASSUMPTIONS

#check for outliers of independent variable (values above +- 3.3)
standBW <- scale(df$Headcirc)
View(standBW)

#calculate residuals from the model
residuals <- resid(model)
#plot them against the model to assess homoscedasticity
plot(fitted(model), residuals)

#create Q-Q plot for residuals to assess normality of residuals
qqnorm(residuals)
#add a straight diagonal line to the plot
qqline(residuals) 


#GOODNESS OF FIT
summary(model)

#we see that F-statistic is 35.29 with p value lower than 5% (p < .0001) which means the model is significant
#and that we have statistically significant linear relationship between the variables Headcirc and Birthweight

#R squared is 0.4687 which is equal to 47% meaning that birth weight variable can explain 47% of variation in head circumference

#we see that both intercept and birthweight are significant predictors (p<.05)

#calculate lower and upper bound for 95% confidence interval
confint(model)


#multiple regression model
model <- lm(df$Headcirc ~ df$Birthweight + df$Length + df$Gestation, df)

#ASSUMPTIONS 

#create a subset of independent variables
df1 <- df[ , c("Birthweight", "Length", "Gestation")]

#check multivariate outliers
mah <- mahalanobis(df1, colMeans(df1), cov(df1)) #calculate mahalanobis distance and check values against Chi-square table

#check multicollinearity
cor(df1) #bivariate correlation between independent variables (no coefficient should be above +-.8)
vif(model) #variance inflation factor (no value should exceed 10)

