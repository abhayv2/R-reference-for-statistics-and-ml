
#  Clear the environment
rm(list=ls())



## Problem 1:  Does Confidence Interval work?  
## We'll generate a population, get a sample from it, create a Confidence Interval using this sample, and then check if our Confidence interval has the Population parameter.  

# Use a 4-digit number (nnnn)  of your choice to set the seed using this command: set.seed=nnnn
set.seed(1215)

# Normal distribution problem with N = 2,500, Mean =180,  and Std dev = 30. Round to 1 decimal place
p1_nm <- round(rnorm(2500,180,30),1)

# Find the mean of this population.  
p1_mean1 <- mean(p1_nm)

# Now, from this population, after setting the same seed again, draw a random sample of size n = 30.
set.seed(1215)

p1_s_nm <- sample(p1_nm, size=30)

# a.  Find the mean and the std error of this sample.
p1_s_mean <- mean(p1_s_nm)

p1_s_se <- sd(p1_s_nm)/sqrt(length(p1_s_nm))

# b. Get the proper t-score for a Confidence level of 84.65%.
t.test(p1_s_nm,conf.level=.8465)

tscore <- qt(0.07675,df=29,lower.tail = FALSE)

# c. Find the lower and upper limits of the Confidence interval. 
lowerinterval <- p1_s_mean - tscore*p1_s_se
upperinterval <- p1_s_mean + tscore*p1_s_se

# d.  Use If statement to see if the population mean falls within the Confidence interval.  Get the appropriate output from the R code.    
if (p1_mean1 > lowerinterval  &  p1_mean1 < upperinterval) {
  print("Population mean falls within the Confidence interval")
} else {
  print("Population mean does not fall within the Confidence interval")
}

## Problem 2 (Set-1)

library(readxl) #import library
table1 <- read_excel("F22-6359-Test-3.xlsx", sheet="Set-1");  	 # reading excel sheet 	 

# a.  Run this as an ANOVA 2-factor  R program.

# Create individual vectors.   rep command rep("Young",30) will repeat Young 30 times.  	 	 	 	 	 	 
v1<-data.frame(Hours = table1[, 2], Medicine=table1[, 1], Age=rep("Young",30))	 
v2<-data.frame(Hours = table1[, 3], Medicine=table1[, 1], Age=rep("Middle_Age",30))	 
v3<-data.frame(Hours = table1[, 4], Medicine=table1[, 1], Age=rep("Senior",30))	 

# Rename columns	 	 	 	 	 	 	 
names(v1)[1] <- 'Hours'	 	 	 	 	 	 
names(v2)[1] <- names(v1)[1]	 	 	 	 	 	 
names(v3)[1] <- names(v1)[1]	 	 	 	 	 	 

# Combine everything and create a new dataset	 	 	 	 	 	 
data1=rbind(v1, v2, v3); data1

# run the anova function
a1<-aov(Hours ~ Medicine + Age + Medicine:Age, data = data1)
summary(a1)


# b.  Look at the online test and get the relevant output from your R code (with proper labels)

# c.  Also draw the interaction graph to show the interaction between the two factors. 

# Plot 
attach(data1) # attaching data1

interaction.plot (Medicine, Age, Hours, lwd = 3,  col=1:3,main="Age vs Medicine")
interaction.plot (Age, Medicine, Hours, lwd = 3,  col=1:3,main="Medicine vs Age")

detach(data1) # detaching data1


## Problem 3 (Set-2) 
## Two sample t-test

# a.  Do a variance test to see if the two variances are equal.

table2 <- read_excel("F22-6359-Test-3.xlsx", sheet="Set-2") # reading excel sheet
var.test(table2$Distance~table2$Gender)

#b.  Do the appropriate t-test at alpha = 5%. 

t.test(table2$Distance~table2$Gender, var.equal = TRUE, alternative = "two.sided")

#c.  Look at the online test and get the relevant output from your R code (with proper labels)


## Problem 4 (Set-3)

table3<-read_excel("F22-6359-Test-3.xlsx", sheet="Set-3") # reading excel sheet

credit_scores <- table3$`Credit scores`/10 # dividing credit score by 10
incomes <- table3$Income/1000 # dividing income by 1000

log_reg<- glm(table3$`Loan Approved`~incomes + credit_scores + table3$`Neighborhood income` , family = "binomial")
summary(log_reg)

RegOut<-c(coef(log_reg));  RegOut
# a.  Look at the online test and get the relevant output from your R code (with proper labels)

RegOut<-c(coef(log_reg));  RegOut
names(RegOut) <- NULL

#For Set 3, what is the probability of loan approval for a person whose credit score is 837, income is 60899 and lives in a neighborhood whose income is 40158?
q13 <- exp(RegOut[1]+RegOut[2]*60.899+RegOut[3]*83.7+RegOut[4]*40.158)
q13
#For Set 3, what are the odds of loan approval for a person who lives in a neighborhood with income of 48726 vs someone with the neighborhood income of 45110 assuming everything else being equal?
q14 <- exp(RegOut[4]*(48.726-45.110))
q14

#For Set 3, what are the Odds of loan approval for a person whose credit score is 826, income is 56217 and lives in a neighborhood whose income is 42744
q15 <- exp(RegOut[1]+RegOut[2]*56.217+RegOut[3]*82.6+RegOut[4]*42.744)
q15


## Problem 5 (Set-4)
library(moments)

# Reading the data 
table4<-read_excel("F22-6359-Test-3.xlsx", sheet="Set-4") # reading excel sheet


# a.  Plot the qqline and boxplot of the data.  Also get the skewness. 
# What is your conclusion about the distribution being normal?

#  qqline
qqnorm(table4$Weight)
qqline(table4$Weight)

# boxplot
boxplot(table4$Weight)

# Skewness
skewness<-skewness(table4$Weight)

# Conclusion: not normally distributed, skewness > 1

#Log Transformation with base e
z<-log(table4$Weight)

qqnorm(z)
qqline(z)
boxplot(z)
skewnwss.transformation<-skewness(z)

# Conclusion: normally distributed , skewness is approximately 0


#c.  What is the mean, Std dev, and the sample size?

# mean
mean_p5 <- mean(z)

# std deviation
sd_p5<-sd(z)

# sample size
sample_size_p5<- length(table4$Weight)

# std error
se_p5<- sd_p5/sqrt(sample_size_p5)

# e.  Find the t-score for the 93.47% confidence interval. 

# T-score
tscore_p5 <- qt(0.03265, df=sample_size_p5, lower.tail = FALSE)

# f.  Use this t-score, sample mean, std error to get the upper and lower limit of the Confidence Interval.  Use the formula we've discussed.  

# Upperlimit
upperlimit_p5<- mean_p5 + tscore_p5*se_p5

# Lowerlimit
lowerlimit_p5<- mean_p5 - tscore_p5*se_p5

# g.  Do reverse transformation to get the Confidence Interval in Ounces.  

# Reverse transformation
u_p5<- exp(upperlimit_p5)
l_p5<- exp(lowerlimit_p5)


## Problem 6

table5<- read_excel("F22-6359-Test-3.xlsx", sheet="Set-5") #reading excel sheet
data5 <- data.frame(table5) # convert to dataframe

d <- data5
d$...1 <- NULL # remove first column
d <- d[-4,1:3] # remove totals

rownames(d) <- data5$...1[-4] # setting row names
d

chisq.test(d) # running chisq-test function



