#### Table 4.3

boston.housing.df <- read.csv("BostonHousing.csv", header = TRUE) 
head(boston.housing.df, 9)
summary(boston.housing.df) 

#### Figure 4.1

library(ggmap)

sort(unique(boston.housing.df$ZN))

tbl <- table(boston.housing.df$CAT..MEDV, boston.housing.df$ZN)
prop.tbl <- prop.table(tbl, margin=2)
barplot(prop.tbl, xlab="ZN", ylab="", yaxt="n",main="Distribution of CAT.MEDV by ZN")
axis(2, at=(seq(0,1, 0.2)), paste(seq(0,100,20), "%"))



#### Table 4.10

cereals.df <- read.csv("Cereals.csv") 

# compute PCs on two dimensions

pcs <- prcomp(data.frame(cereals.df$calories, cereals.df$rating)) 
summary(pcs) 
pcs$rot
scores <- pcs$x
head(scores, 5)



#### Table 4.11

pcs <- prcomp(na.omit(cereals.df[,-c(1:3)])) 
summary(pcs)
pcs$rot[,1:5]


#### Table 4.12

pcs.cor <- prcomp(na.omit(cereals.df[,-c(1:3)]), scale. = T)
summary(pcs.cor)
pcs.cor$rot[,1:5]








