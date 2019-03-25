library(data.table)
library(kohonen)
library(dummies)
library(ggplot2)
library(sp)
library(maptools)
library(reshape2)
library(rgeos)

mydat <- readxl::read_xlsx("~/SOM/laurel-world-happiness-report-data/laurel-world-happiness-report-data/online-data-chapter-2-whr-2017.xlsx", sheet = 3)
mydat <- data.table(mydat)

mydat <- mydat[,1:10]
mydat <- mydat[, -c(2,3,4)]

data_matrix <- as.matrix(mydat[, 2:7])
som_grid <- somgrid(xdim = 5, ydim=5, topo="hexagonal")
set.seed(123)
som_model <- som(data_matrix,grid=som_grid,
                 rlen=1000,
                 alpha=c(0.05,0.01),
                 keep.data = TRUE)
names(som_model)
#plot(som_model, type="changes")
#plot(som_model, type="counts")
#plot(som_model, type="dist.neighbours")
#plot(som_model, type="codes")

coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}
library(RPMG)
pastel <- function(n){
  pastel.colors(n)
}

#gradients
colfunc <- colorRampPalette(c("#824C71", "#FFF0F5"))

pal <- function(n){
  colfunc(n)
}

x <- as.data.frame(som_model$codes)

data <- x
wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(data, centers=i)$withinss)
}
plot(wss)

pretty_palette <- c("#1f77b4", '#2ca02c', '#ff7f0e', '#d62728', '#9467bd', '#8c564b', '#e377c2')
## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(x)), 8)
# plot these results:

plot(som_model, type="codes", labels = mydat$Country,
     bgcol = pretty_palette[som_cluster], pchs = NA, shape = "straight")
som.hc <- cutree(hclust(dist(x)), 8)
add.cluster.boundaries(som_model, som.hc)


