rm(list=ls())
name1 <- "AVV210000_VAIDIAN_ABHAY"; name1

dir1 <- "C:/Users/Asus/Documents/Work/advStat"
setwd(dir1)
# setwd("C:/Users/saura/Box/22F/6359/Homework/Bonus_HW")

#install.packages("readxl")
#install.packages("dplyr")
library("readxl")
Table1<-read_excel("Bonus_HW.xlsx", sheet="Data")

set.seed(115)
len <-length(Table1$A1)
A3 <- round(runif(len,min=80,max=100),0)

Table1 <- data.frame(Table1,A3)
Total1 <- c(Table1$A1 + Table1$A2 + Table1$A3)

t2<- data.frame(Table1,Total1)
names(t2)
names(t2)[1]<-"Test1"
names(t2)[2]<-"Test2"
names(t2)[3]<-"Test3"

Grade<-(rep("F",len))
t2<- data.frame(t2,Grade)
t2$Grade<-as.character(t2$Grade)
t2$Grade[t2$Total1>=250]<-"D"
t2$Grade[t2$Total1>=260]<-"C"
t2$Grade[t2$Total1>=270]<-"B"
t2$Grade[t2$Total1>=280]<-"A"

csvfile <- paste(name1,"_Bonus_HW.csv",sep=""); csvfile
sink(csvfile)
cat("NAME",  sep = ","   ,  "Abhay Vaidian", "\n")
cat("NETID" ,  sep = ","   , "AVV210000", "\n")
cat("--------------------------------", "\n")
write.table(t2, sep= "," , row.names=FALSE)
sink()
