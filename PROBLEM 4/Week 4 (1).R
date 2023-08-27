
#one-way anova
model <- aov(df2$weightLOST ~ df2$diet)


#display results
summary(model)

#plot means
ggline(df2, x = "Diet", y = "weightLOST", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3"),
       ylab = "Weight", xlab = "Diet")



#interaction ("*")
interaction <- aov(weightLOST ~ diet*gender, data = df2)

#display results
summary(interaction)



#add a covariate ("+")
model <- aov(df2$weightLOST ~ df2$diet + df2$Age)