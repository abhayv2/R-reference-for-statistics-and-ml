rm(list=ls())
name1 <- "AVV210000_VAIDIAN_ABHAY"; name1

dir1<-getwd()
setwd(dir1)

# install.packages("moments")
#library("readxl")
library("moments")

# Create output file name using name1.
csvfile <- paste(name1,"_HW2.csv",sep=""); csvfile
sink(csvfile)

cat("NAME",  sep = ","   ,  "Abhay Vaidian", "\n")
cat("NETID" ,  sep = ","   , "AVY210000", "\n")

cat("Part A","\n")

# a) None of the LED bulbs are defective?
ans1 <- dbinom(0,10,0.05)
cat("None of the LED bulbs are defective?",sep=",",ans1,"\n")

# b) Exactly one of the LED bulbs is defective?
ans2 <-  dbinom(1,10,0.05)
cat("Exactly one of the LED bulbs is defective?",sep=",",ans2,"\n")

# c) Two or fewer of the LED bulbs are defective?
ans3 <- pbinom(2,10,0.05)
cat("Two or fewer of the LED bulbs are defective?",sep=",",ans3,"\n")

# d) Three or more of the LED bulbs are defective
ans4 <- 1-ans3
cat("Three or more of the LED bulbs are defective",sep=",",ans4,"\n")


cat("Part B","\n")

# a) Probability that the agent sells some policies is
ans5 <- ppois(0, lambda=3, lower=FALSE)
cat("Probability that the agent sells some policies is",sep=",",ans5,"\n")

# b. Agent sells 2 or more but less than 5 policies
ans6 <-  (ppois(4, lambda=3) -  ppois(2, lambda=3))
cat("Agent sells 2 or more but less than 5 policies",sep=",",ans6,"\n")

cat("Part C","\n")

# Calculate the probabilities and print the result and  proper label for each
# Assume  Mean = 80, Sigma = 13

# Find P(X ≥  92)
Mean<- 80
Sigma<- 13
MoreThan92 <- pnorm(92 , Mean , Sigma, lower.tail = FALSE)
cat("P(X  >=  92)" , sep = "," ,MoreThan92 ,"\n")

# Find P(72 ≤ X ≤ 95)
From72To95<- pnorm(95, Mean, Sigma, lower.tail = TRUE)- pnorm(72, Mean, Sigma, lower.tail = TRUE)
cat("P(72 <= X <= 95)" , sep = "," ,From72To95,"\n")

# Find the cut-off for top 10%.
c10 <- qnorm(0.9, Mean, Sigma,lower.tail = TRUE )
cat("P( X Find the cut-off for top 10%.)" , sep = "," ,c10,"\n")

# Find the cutoff for bottom 12%.
c12 <- qnorm(0.12, Mean, Sigma, lower.tail= TRUE)
cat("P( Find the cutoff for bottom 12%.)" , sep = "," ,c12,"\n")

# t-distribution
# Assume  Mean = 80,  std dev = 13, sample size = 23
# Find the cut-off for top 10%.
tdistc10 <- 80+qt(0.9,22)*13
cat("Top 10% t-dist" , sep = "," ,tdistc10,"\n")

tdistc12 <- 80+qt(0.12,22)*13
cat("Bottom 12% t-dist" , sep = "," ,tdistc12,"\n")

# Generate a normal distribution dataset with mean = 83, sigma = 27, n  = 200
# Assign it to a vector, say, nm1.  Assume nm1 is a population.
nm1 <- rnorm(200,83,27)

# make all the numbers of vector nm1  integer
nm1 <- as.integer(nm1)
# For nm1, find the following  and print the values and label.   
#Mean
u <- mean(nm1)
cat("Mean" , sep="," , u , "\n")

#Population Std Dev (R gives sample std dev.  Convert it into population std dev)
s <- sd(nm1)*(sqrt((200-1)/200))
cat("Standard Deviation" , sep = "," , s ,"\n")

#Skewness
e <- skewness(nm1)
cat("Skewness" , sep = "," ,e,"\n")

# Numbers outside µ ± 2σ will be treated as outliers.  Determine the upper and lower
# cut-off numbers.  Also the number of outliers.  Label everything.
# What is the upper cut-off number?
upper_cut <- u + 2*s
cat("Upper cut-off number" , sep = "," ,upper_cut,"\n")

# What is the lower cutoff number? 
lower_cut <- u - 2*s
cat("Lower cut-off number?" , sep = "," ,lower_cut,"\n")

# How many numbers are more than the upper cutoff number?  Use length function. 
upper_len <- length(nm1[nm1>upper_cut])
cat("Numbers above upper cut-off" , sep = "," ,upper_len,"\n")

# How many numbers are below the lower cutoff number?  Use length function. 
lower_len <- length(nm1[nm1<lower_cut])
cat("Numbers below lower cut-off" , sep = "," ,lower_len,"\n")

# create a divider line
cat("---------------------------------", "\n")

# Write summary statistics of nm1
summary(nm1)

# create a divider line
cat("---------------------------------", "\n")

# write the vector nm1 in one column
cat(nm1, sep="\n")

# Stop writing to the CSV file.  
sink()

# Partition the graph area into 4 parts (2 rows and 2 columns). This will print all 4 graphs below on one screen. 
par(mfrow=c(2,2))

# density plot of nm1
d1<-density(nm1);  plot(d1)

# boxplot of nm1
boxplot(nm1)

# Histogram of nm1
hist(nm1)

# qqplot of nm1
qqnorm(nm1)





