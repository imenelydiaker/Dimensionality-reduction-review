## File : main.R
## Apply linear and nonliear dimensionality reduction methods on artificial datasets
## Authors : Imene Kerboua, Mouad Laribia

rm(list = ls())

# Uncomment the following lines to install packages :
# install.packages("vegan")
# install.packages("kernlab")
# install.packages("FactoMineR")
# install.packages("Rtsne")
# install.packages("lle")
# install.packages("dimRed")
# install.packages("loe")
# install.packages("RSpectra")
# install.packages("RANN")
# install.packages("igraph")

require(FactoMineR)
require(Rtsne)
# require(vegan)
require(ProjectionBasedClustering)
require(kernlab)
require(lle)
require(dimRed)
require(rgl)
require(ggplot2)

## Load artificial datasets and metrics functions
source("artificial_datasets.R")
source("metrics.R")

source("int_dim_estim.R")

## METHODS : PCA, Kernel PCA, t-SNE, Isomap, LLE, Laplacian Eigenmaps, Sammon's mapping

## Artificial Datasets ####
N <- 1000; layout3d(matrix(1:4, 1:4))
## Swiss Roll dataset####
# swiss_roll <- swissroll(N = N, noise = 0.05)
# write.table(swiss_roll, file = "data/swissroll.txt")
swiss_roll <- read.table("data/swissroll.txt")
plot3d(swiss_roll, col = rainbow(N), box = F, size = 2)

## Broken Swiss Roll dataset ####
# broken_swiss_roll <- brokenswissroll(N = N, noise = 0.05)
# write.table(broken_swiss_roll, file = "data/broken_swissroll.txt")
broken_swiss_roll <-  read.table("data/broken_swissroll.txt")
plot3d(broken_swiss_roll, col = rainbow(N), box = F, size = 2)

## Helix dataset ####
# helx <- helix(N = N, noise = 0.05)
# write.table(helx, file = "data/helix.txt")
helx <- read.table("data/helix.txt")
plot3d(helx, col = rainbow(N), box = F, size = 2)

## Twinpeaks dataset ####
# twinpeak <- twinpeaks(N = N, noise = 0.05)
# write.table(twinpeak, file = "data/twinpeaks.txt")
twinpeak <- read.table("data/twinpeaks.txt")
plot3d(twinpeak, col = rainbow(N), box = F, size = 2)

## High-Dimensional dataset ####
# HD_data <- highDimensionalData(N = N, noise = 0.05) #High Dimensional data can't be plotted dim = 10
# write.table(HD_data, file = "data/hd.txt")
HD_data <- read.table("data/hd.txt")

## Real Dataset : MNIST 1000 samples 784 columns
minimnist <- as.matrix(read.table("data/minimnist/data.txt"))
minimnist <- minimnist[1:1000,]
dim(minimnist)

## Trsutworthiness TR and Continuity C dataframes initialization ####
dataset <- list(swiss_roll, broken_swiss_roll, helx, twinpeak, HD_data, minimnist)
N_dataset <- length(dataset)
row.names <- c('Swiss Roll', 'Broken Swiss Roll', 'Broken Swiss Roll', 'Helix', 'Twinpeaks', 'HD_data', 'MNIST')
TR <- data.frame(matrix(0, N_dataset, 6),row.names = row.names)
colnames(TR) <- c('PCA', 'KPCA', 'ISOMAP', 'Sammon', 'LLE', 'tsne' )
C <- data.frame(matrix(0, N_dataset, 6),row.names = row.names)
colnames(C) <- colnames(TR)
dim_RD <- c(2, 2, 1, 2, 5, 20)
plot.new()

## Computing TR and C for every dataset with different DR techniques ####
for (ds in seq(N_dataset)){
  png(paste(row.names[ds],'.png', sep = "_"), width = 700, height = 700)
  par(mfrow=c(3,2))
  cat(paste("\n\n Dataset :", row.names[ds]))

## PCA ####
  cat("\n ******************* PCA *******************")
  res.pca <- FactoMineR::PCA(dataset[[ds]], scale.unit = T, graph = F, ncp = dim_RD[ds])
  ind.coord <- res.pca$ind$coord
  dim(ind.coord)
  plot(ind.coord, xlab="Coordinate 1", ylab="Coordinate 2", main="PCA", col = rainbow(N))

  # test metrics : trust and continuity
  TR$PCA[ds] <- trustworthiness(orig = as.matrix(dataset[[ds]]), proj = ind.coord, kn = 12)
  cat(paste("\nTrustworthiness of PCA :", TR$PCA[ds]))
  C$PCA[ds] <- continuity(orig = as.matrix(dataset[[ds]]), proj = ind.coord, kn = 12)
  cat(paste("\nContinuity of PCA :", C$PCA[ds]))


## t-SNE ####
  cat("\n\n ******************* t-SNE *******************")
  tsne <- Rtsne(dataset[[ds]], dims = dim_RD[ds], perplexity=30, verbose=TRUE, max_iter = 1000)
  plot(tsne$Y,col= rainbow(N), xlab="Coordinate 1", ylab="Coordinate 2", main="t-SNE")

  # test metrics : trust and continuity
  TR$tsne[ds] <- trustworthiness(orig = dataset[[ds]], proj = as.matrix(tsne$Y), kn = 12)
  cat(paste("\nTrustworthiness of t-SNE :", TR$tsne[ds]))
  C$tsne[ds] <- continuity(orig = dataset[[ds]], proj = as.matrix(tsne$Y), kn = 12)
  cat(paste("\nContinuity of t-SNE :", C$tsne[ds]))


## ISOMAP ####
  cat("\n\n ******************* ISOMAP *******************")
  res.isomap <- Isomap(as.matrix(dist(dataset[[ds]])), OutputDimension = dim_RD[ds], k = 13)
  x <- res.isomap$ProjectedPoints[,1]
  if(ds == 2){
    y <- res.isomap$ProjectedPoints[,2]
    plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="ISOMAP", col= rainbow(N))
  }
  else
    if(ds ==1) {
    plot(x, xlab="Coordinate 1", ylab="Coordinate 2", main="ISOMAP", col= rainbow(N))
  }

  # test metrics : trust and continuity
  TR$ISOMAP[ds] <- trustworthiness(orig = dataset[[ds]], proj = res.isomap$ProjectedPoints, kn = 12)
  cat(paste("\nTrustworthiness of ISOMAP :", TR$ISOMAP[ds]))
  C$ISOMAP[ds] <- continuity(orig = dataset[[ds]], proj = res.isomap$ProjectedPoints, kn = 12)
  cat(paste("\nContinuity of ISOMAP :", C$ISOMAP[ds]))


## LLE : Locally Linear Embedding ####
  cat("\n\n ******************* LLE *******************")
  calc_k(dataset[[ds]], 2, 1, 15, F, T, 3) # best k in order : 14 15 11
  res.lle <- lle(dataset[[ds]], m = dim_RD[ds], k = 14)
  plot(res.lle$Y, xlab = "Coordinate 1", ylab = "Coordinate 2", main = "LLE", col = rainbow(N))

  # test metrics : trust and continuity
  TR$LLE[ds] <- trustworthiness(orig = dataset[[ds]], proj = res.lle$Y, kn = 12)
  cat(paste("\nTrustworthiness of LLE :", TR$LLE[ds]))
  C$LLE[ds]<- continuity(orig = dataset[[ds]], proj = res.lle$Y, kn = 12)
  cat(paste("\nContinuity of LLE :", C$LLE[ds]))


## Kernel-PCA ####
  cat("\n\n ******************* Kernel PCA *******************")
  res.kpca <- kpca(as.matrix(dataset[[ds]]), kernel = "polydot", kpar = list(degree = 5, scale = 1, offset = 1), features = dim_RD[ds])
  plot(pcv(res.kpca), xlab = "Coordinate 1", ylab = "Coordinate 2", main = "Kernel PCA", col = rainbow(N))

  # test metrics : trust and continuity
  TR$KPCA[ds] <- trustworthiness(orig = dataset[[ds]], proj = as.matrix(res.kpca@pcv), kn = 12)
  cat(paste("\nTrustworthiness of Kernel PCA :", TR$KPCA[ds]))
  C$KPCA[ds] <- continuity(orig = dataset[[ds]], proj = as.matrix(res.kpca@pcv), kn = 12)
  cat(paste("\nContinuity of Kernel PCA :", C$KPCA[ds]))


## SAMMON ####
  cat("\n\n ******************* Sammon's Mapping *******************")
  res.sammon <- sammon(d = dist(dataset[[ds]]) , k = dim_RD[ds])
  x <- res.sammon$points[,1]
  if(ds == 2){
    y <- res.sammon$points[,2]
    plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", main = "SAMMON", col = rainbow(N))
  }
  else
    if(ds ==1) {
      plot(x, xlab = "Coordinate 1", ylab = "Coordinate 2", main = "SAMMON", col = rainbow(N))
    }


  # test metrics : trust and continuity
  TR$Sammon[ds] <- trustworthiness(orig = dataset[[ds]], proj = res.sammon$points, kn = 12)
  cat(paste("\nTrustworthiness of SAMMON :", TR$Sammon[ds]))
  C$Sammon[ds] <- continuity(orig = dataset[[ds]], proj = res.sammon$points, kn = 12)
  cat(paste("\nContinuity of SAMMON :", C$Sammon[ds]))

  dev.off()
}

# save results
write.table(TR, file = "results/Trustworthiness.txt", append = T)
write.table(C, file = "results/Continuity.txt", append = T)


## Laplacian Eigenmaps ####
# s <- loadDataSet("Swiss Roll")
# leim <- LaplacianEigenmaps(stdpars = list(ndim = 2, sparse = 'knn', knn = 50, eps = 1) )
# res.leim <- leim@fun(s ,leim@stdpars)
# plot(res.leim@data@data, xlab = "Coordinate 1", ylab = "Coordinate 2",
#      main = "Laplacian Eigenmaps", col = rainbow(N), pch = 19)
#
# # test metrics : trust and continuity
# trust.leim <- trustworthiness(orig = as.matrix(s), proj = as.matrix(res.leim@data@data), kn = 14)
# print(paste("Trustworthiness of Laplacian Eigenmaps :", trust.leim))
# cont.eim <- continuity(orig = s, proj = as.matrix(res.leim@data@data), kn = 14)
# print(paste("Continuity of Laplacian Eigenmaps :", cont.leim))
