#  Clear the environment
rm(list = ls())

# Get Current Working Directory and store it in dir1 variable 
dir1<-getwd()

# Set your working Directory by using the Following Command
setwd(dir1)

# your info (NetID_LastName_FirstName); write to Console
name1 <- "AVV210000_VAIDIAN_ABHAY"; name1

# Create output file name using name1.
csvfile <- paste(name1,"_HW3.csv",sep=""); csvfile

# Instruct to send all the output of your calculation  to a csv file 
sink(csvfile)

# Use cat function to write your name
cat("NAME",  sep = "," ,  "Abhay Vaidian", "\n")

# Use cat function to write your netid
cat("NETID" ,  sep = "," , "AVV210000", "\n")

# Use this cat function to print the Problem 1 in your output
cat("Problem 1","\n")

# Read the excel file
library(readxl)
table <- read_excel("HW3-6359-F22.xlsx", sheet="t-test")

#Calculate the Mean of single population

ans1 <- mean(table$Weight)
cat("The Mean of the sample is = ",  sep = ","  , ans1 , "\n")

#Calculate the Std. Deviation

ans2 <- sd(table$Weight)
cat("Standard Deviation is =",  sep = ","  ,ans2 , "\n")

#Calculate the Sx-Bar

len <- length(table$Weight)
ans3 <- ans2/sqrt(len)
cat("Sx-Bar is =",  sep = "," , ans3 , "\n")

#Upper cut-off point

t <- qt(0.0125, len-1, lower.tail=FALSE)
ans4 <- 150+t*ans3
cat("Upper cut-off point is",  sep = ","   , ans4 , "\n")

#Lower cut-off point

ans5 <- 150-t*ans3
cat("Lower cut-off point is",  sep = ","   , ans5, "\n")

#P-value

t1 <- (ans1-150)/ans3
ans6 <- pt(t1, df=(len-1), lower.tail= FALSE)
cat("P-Value is",  sep = ","   , ans6*2, "\n")

#Decision. Make Sure You Print ANY ONE of the the statement depending on your Result. Don't make Changes to the given statement.

cat("Decision",  sep=",", "We Fail to reject Null Hypothesis", "\n") 


##Problem 2:  ANOVA single factor

# Use this cat function to print the Problem 2 in your output
cat("Problem 2 ","\n")

# Read the excel file  (sheet = ANOVA sheet). 
table1 <- read_excel("HW3-6359-F22.xlsx", sheet="ANOVA")

#Calculate The Mean of Dallas city 

ans7 <- table1[table1$City == 'Dallas',]
cat("The Mean of Dallas city",  sep = ","   , mean(ans7$Stocks), "\n")

#Calculate The Mean of Pittsburgh city 

ans8 <- table1[table1$City == 'Pittsburgh',]
cat("The Mean of Pittsburgh city",  sep = ","   , mean(ans8$Stocks), "\n")

#Calculate The Mean of Boston city 

ans9 <- table1[table1$City == 'Boston',]
cat("The Mean of Boston city",  sep = ","   , mean(ans9$Stocks), "\n")

#Calculate The Mean of Seattle city 

ans10 <- table1[table1$City == 'Seattle',]
cat("The Mean of Seattle city",  sep = ","   , mean(ans10$Stocks), "\n")

#P-Value

anova1 <- aov(table1$Stocks~table1$City)
ans11 <- summary(anova1)[[1]][1,5]
cat("P-Value is",  sep = ","   , ans11, "\n")

#Decision. Make Sure You Print ANY ONE of the the statement depending on your Result. Don't make Changes to the given statement.

cat("Decision",  sep="," ,  "We reject the Null Hypothesis", "\n")


#Problem 3:  Log-Transformation

# Use this cat function to print the Problem 3 in your output
cat("Problem 3 ","\n")

# Read the excel file  (sheet = Log sheet). 
table2 <- read_excel("HW3-6359-F22.xlsx", sheet="Log")

#Calculate the Skewness Before Transformation

library(moments)

ans12 <- skewness(table2$Radiation)
cat("The Skewness Before Log Transformation ",  sep="," , ans12, "\n")

# Calculate the Skewness After log Transformation with base e

e <- log(table2$Radiation,base = exp(1))
ans13 <- skewness(e)
cat("The Skewness After Log Transformation with base e  ",  sep="," , ans13, "\n")

# Calculate the Skewness After log Transformation with base 10

t <- log(table2$Radiation,base = 10)
ans14 <- skewness(t)
cat("The Skewness After Log Transformation with base 10   ",  sep="," , ans14, "\n")

# Compare the skewness of both log Transformations is it the same? Answer 'Yes' or 'No'  Only.

cat("Similar: ",  sep = "," ,"Yes", "\n")

# Stop writing to the CSV file. 

sink()

# Partition the graph area into 4 parts (2 rows and 2 columns). This will print all 4 graphs on one screen. 

par(mfrow=c(2,2))

# Plot the Histogram and QQplot Graph comparing Before and After log Transformation with base e.

hist(table2$Radiation)
hist(e)
qqnorm(table2$Radiation)
qqnorm(e)





