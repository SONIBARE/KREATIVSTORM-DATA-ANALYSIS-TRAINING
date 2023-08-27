#Load datasets
library(readxl)
> NASSL2000 <- read_excel("NASSL2000.xlsx")
> View(NASSL2000)
> df<- NASSL2000
> library(readxl)
> NASSL2010 <- read_excel("NASSL2010.xlsx")
> View(NASSL2010)
> df1 <- NASSL2010

#check for descriptive stats
> View(df)
> summary(df$hetlife)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-1.0     2.0     5.0   253.8    10.0  9999.0 

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.0     2.0     5.0   193.2    10.0  9999.0 


#remove rows with meaningless values
> df<-df[!(df$hetlife==9995),]
> df<-df[!(df$hetlife==9996),]
> df<-df[!(df$hetlife==9997),]
> df<-df[!(df$hetlife==9999),]
> summary(df$hetlife)
Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
0.000    2.000    5.000    9.691   10.000 1000.000 

#plot a histogram of the dataset

> library(ggplot2)
> ggplot(df, aes(x = hetlife)) +
  +     geom_histogram(fill = "white", colour = "black") +
  +     facet_grid(rsex ~ .)
`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
> boxplot(hetlife ~ rsex, data = df,
          +         main = "No of het partners by gender",
          +         xlab = "Gender",
          +         ylab = "Hetlife")
#check for outliers by standardizing
> df$standhetlife <- unlist(by(df, df$rsex, FUN = function(x) scale(x$hetlife)))
> 
  > View(df$standhetlife)
> sum(df$standhetlife > 3.3)
[1] 105
> max(df$standhetlife)
[1] 34.14731

#REMOVE OUTLIERS

> df <- df[!(df$rsex == 1 & df$hetlife >= 300 ),]
> df <- df[!(df$rsex == 2 & df$hetlife >= 150 ),]
> ggplot(df, aes(x = hetlife)) +
  +     geom_histogram(fill = "white", colour = "black") +
  +     facet_grid(rsex ~ .)
`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.


> tapply(df$hetlife, df$rsex, summary)
$`1`
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.0     2.0     6.0    12.2    14.0   250.0 

$`2`
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.000   2.000   4.000   6.602   8.000 115.000 

#create confidence interval for the mean
> tapply(df$hetlife, df$rsex, sd)
1         2 
20.938892  8.805077 

> df %>% count(rsex)
# A tibble: 2 × 2
rsex     n
<dbl> <int>
  1     1  5022
2     2  6771
> a <- 12.2
> b <- 20.94
> n <- 5022
> error <- qnorm(0.975)*b/sqrt(n)
> left <- a-error
> right <- a+error
> a
[1] 12.2
> left
[1] 11.62086
> right
[1] 12.77914
> a <- 6.60
> b <-8.81
> n<-6771
> error <- qnorm(0.975)*b/sqrt(n)
> left <- a-error
> right <- a+error
> left
[1] 6.390155
> right
[1] 6.809845
> library(ppcor)


#check for homogeneity of data

> var.test(df$hetlife ~ df$rsex)

#check for skewness
tapply(df$hetlife, df$rsex, skewness)

#carrY out two-sample t-test for heterogenous data
> t.test(df1$hetlife ~ df$rsex, var.equal = FALSE)

#load combined dataset
> rsex_hetlife2 <- read.csv("C:/Users/USER/Downloads/rsex_hetlife2.csv")
>   View(rsex_hetlife2)
> df2<-rsex_hetlife2

#Perform two-way anova for the combined data set to check for interaction


> interaction <- aov(hetlife ~ rsex*year, data = df2)
> summary(interaction)
Df  Sum Sq Mean Sq F value Pr(>F)    
rsex            1  159839  159839 684.268 <2e-16 ***
  year            1      31      31   0.134  0.715    
rsex:year       1    1511    1511   6.467  0.011 *  
  Residuals   26326 6149536     234                   
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1