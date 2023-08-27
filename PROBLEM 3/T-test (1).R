#LOAD PACKAGES
x<-c("plyr", "dplyr", "tidyr", "moments","ppcor","ggplot2","hrbrthemes", "haven")
lapply(x, require, character.only = TRUE)


#INDEPENDENT MEANS T-TEST
#check normality of the groups
tapply(df$Birthweight, df$smoker, shapiro.test)

#check homogeneity of variances
var.test(df$Birthweight ~ df$smoker)

#compare birth weight in smoking and non-smoking group

#independent samples t-test - homogeneity of variances
t.test(df$Birthweight ~ df$smoker, var.equal = TRUE)

#independent samples t-test - heterogeneity of variances
t.test(df$Birthweight ~ df$smoker, var.equal = FALSE)


###import Cholesterol dataset and store it in "df"###

#DEPENDENT MEANS T-TEST
#compare cholesterol before and after 8 weeks
t.test(df$Before, df$After8weeks, paired = TRUE)


#ONE SAMPLE T-TEST
#compare cholesterol before to 5
#m is the known mean
t.test(df$Before, m = 5)