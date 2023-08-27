#LOAD PACKAGES
x<-c("plyr", "dplyr", "tidyr", "moments")
lapply(x, require, character.only = TRUE)

#Pearson's correlation test
cor.test(df$Headcirc,df$Birthweight, method = "pearson")

#Spearman's correlation test
cor.test(df$Headcirc,df$Birthweight, method = "spearman")