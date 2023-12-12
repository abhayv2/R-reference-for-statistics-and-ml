#### Table 15.2

pharma.df <- read.csv("Pharmaceuticals.csv")

# set row names to the utilities column
row.names(pharma.df) <- pharma.df[,1]

# remove the utility column
pharma.df <- pharma.df[,c(-1,-2,-12,-13,-14)]


# compute Euclidean distance
# (to compute other distance measures, change the value in method = )
d <- dist(utilities.df, method = "euclidean")


#### Table 15.4

# normalize input variables
pharma.df.norm <- sapply(pharma.df, scale)

# add row names: utilities
row.names(pharma.df.norm) <- row.names(pharma.df) 

# compute normalized distance based on variables Sales and FuelCost
d.norm <- dist(pharma.df.norm[,c(6,8)], method = "euclidean")



#### Figure 15.3
# compute normalized distance based on all 8 variables
d.norm <- dist(pharma.df.norm, method = "euclidean")

# in hclust() set argument method =  
# to "ward.D", "single", "complete", "average", "median", or "centroid"
hc1 <- hclust(d.norm, method = "single")
plot(hc1, hang = -1, ann = FALSE)

hc2 <- hclust(d.norm, method = "complete")
plot(hc2, hang = -1, ann = FALSE)

library(cluster)
agnes(d.norm, method = "single")$ac
agnes(d.norm, method = "average")$ac

hc3 <- hclust(d.norm, method = "ward.D")
plot(hc3, hang = -1, ann = FALSE)


#### Table 15.6

memb_s <- cutree(hc1, k = 6)
memb_s

memb_c <- cutree(hc2, k = 6)
memb_c

# better grouping, dendexten
plot(hc2, cex = 1)
rect.hclust(hc2, k = 6, border = 2:5)

dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

library(dendextend)
tanglegram(dend1, dend2)

#### Figure 15.4

# set labels as cluster membership and utility name
row.names(utilities.df.norm) <- paste(memb, ": ", row.names(utilities.df), sep = "")

# plot heatmap 
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(utilities.df.norm), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))
