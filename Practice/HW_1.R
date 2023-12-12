rm(list=ls())
name1 <- "AVV210000_VAIDIAN_ABHAY"; name1

dir1 <- "C:/Users/Asus/Documents/Work/advStat"
setwd(dir1)

# setwd("C:\Users\saura\Box\TA-Saurabh\22F\6359\Homework\Homework 1\Sec-003")

library(readxl)

table<-read_excel("HW1-6359.xlsx", sheet="Pioneer")

# change column name
t2<- data.frame(table)
names(t2)
names(t2)[2]<-"Quantity"

# new vecctors
Total <- c(t2$Quantity*t2$Price)
Commission <- c(Total*0.10)

# add the new vectors to the excel file
t3<- data.frame(t2,Total,Commission)

csvfile <- paste(name1,"_HW1.csv",sep=""); csvfile
sink(csvfile)

cat("NAME",  sep = ","   ,  "Abhay Vaidian", "\n")
cat("NETID" ,  sep = ","   , "AVV210000", "\n")

len1 <- length(table$`Part No`)
cat("LENGTH" ,  sep = ","   , len1, "\n")

average <- mean(t3$Price)
cat("AVERAGE" ,  sep = ","   , average, "\n")

minimum <- min(t3$Price)
cat("MINIMUM" ,  sep = ","   , minimum, "\n")

maximum <- max(t3$Price)
cat("MAXIMUM" ,  sep = ","   , maximum, "\n")

total_q <- sum(t3$Quantity)
cat("TOTAL QUANTITY" ,  sep = ","   , total_q, "\n")

total_c <- sum(t3$Commission)
cat("TOTAL COMMISSION" ,  sep = ","   , total_c, "\n")

avg_c <- mean(t3$Commission)
cat("AVERAGE COMMISSION" ,  sep = ","   , avg_c, "\n")

std_c <- sd(t3$Commission)
cat("STD DEV COMMISSION" ,  sep = ","   , std_c, "\n")

cat("--------------------------------", "\n")

write.table(t3, sep= "," , row.names=FALSE)

# PART B

cat("--------------------------------", "\n")
cat("Part B", "\n")

table2<-read_excel("HW1-6359.xlsx", sheet="Inc_Exp_Data")


# Mean expenses of Household

expenses <- c(table2$Mthly_HH_Expense+table2$Emi_or_Rent_Amt)
mean_expense <- mean(expenses)
cat("Mean Expense of a household" ,  sep = ","   , mean_expense, "\n")

# Median expenses of Household

median_expense <- median(expenses)
cat("Median Expense of a household" ,  sep = ","   , median_expense, "\n")

# 2 or more earning member households

member <- sum(table2$No_of_Earning_Members>=2)
cat("Households have 2 or more earning members" ,  sep = ","   , member, "\n")

#  Frequency of each Highest_Qualified_Member

t4 <- table(table2$Highest_Qualified_Member)

write.table(t4,sep=",",row.names=FALSE)

# Standard deviation of first column

std_mhhi <- sd(table2$Mthly_HH_Income)
cat("Standard Deviation for first columns. " ,  sep = ","   , std_mhhi, "\n")

sink()
