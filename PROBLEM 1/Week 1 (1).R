#LOAD PACKAGES
x<-c("plyr", "dplyr", "tidyr", "moments")
lapply(x, require, character.only = TRUE)

#load data into dataframe "df"
df <- Birthweight_reduced_kg_SPSS

#basic descriptives for variable "birthweight"
summary(df$Birthweight)

#basic descriptives for variable "birthweight" grouped by the variable "smoker"
tapply(df$Birthweight, df$smoker, summary)

#skewness for variable "birthweight"
skewness(df$Birthweight)

#skewness for variable "birthweight" grouped by the variable "smoker"
tapply(df$Birthweight, df$smoker, skewness)

#normality for variable "birthweight"
shapiro.test(df$Birthweight)

#normality for variable "birthweight" grouped by the variable "smoker"
tapply(df$Birthweight, df$smoker, shapiro.test)
