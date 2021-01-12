## File : dim_estim.R
## Intrinsic Dimensionality Estimation on artificial datasets
## Author : Imene Kerboua, Mouad Laribia

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
require(vegan)
require(kernlab)
require(lle)
require(dimRed)
require(rgl)
require(ggplot2)

## Load artificial datasets and metrics functions ####
source("artificial_datasets.R")
source("metrics.R")
source("cor_dim.R")

## METHOD : Correlation Dimension

## Artificial Datasets ####
N <- 5000; layout3d(matrix(1:4, 1:4))
## Swiss Roll dataset####
swiss_roll <- swissroll(N = N, noise = 0.05)
plot3d(swiss_roll, col = rainbow(N), box = F, size = 0.1)

## Broken Swiss Roll dataset ####
broken_swiss_roll <- brokenswissroll(N = N, noise = 0.05)
plot3d(broken_swiss_roll, col = rainbow(N), box = F, size = 0.1)

## Helix dataset ####
helx <- helix(N = N, noise = 0.05)
plot3d(helx, col = rainbow(N), box = F, size = 0.1)

## Twinpeaks dataset ####
twinpeak <- twinpeaks(N = N, noise = 0.05)
plot3d(twinpeak, col = rainbow(N), box = F, size = 0.1)

## High-Dimensional dataset ####
HD_data <- highDimensionalData(N = N, noise = 0.05)#High Dimensional data can't be plotted dim = 10

## MNIST dataset : 5000 samples * 784 columns
minimnist2 <- as.matrix(read.table("data/minimnist/data.txt"))
minimnist2 <- minimnist2 / rowSums(minimnist2) # normalize data

## Intrinsic Dimension Estimation ####
dataset <- list(swiss_roll, broken_swiss_roll, helx, twinpeak, HD_data, minimnist2)
N_dataset <- length(dataset)
row.names <- c('Swiss Roll', 'Broken Swiss Roll', 'Helix', 'Twinpeaks', 'HD_data', 'MNIST')
dim_RD <- c(2, 2, 1, 2, 5, NA)

plot.new()
png('dim_estim.png', width = 700, height = 700)
par(mfrow=c(3,2))
for (ds in seq(N_dataset)){
  print(paste("Dataset :", row.names[ds]))
  Xdim <- corrDim(dataset[[ds]], epsilon = 10 ^ seq(-1, 1, length.out = 50))
  der_Xdim <- derivate(log10(Xdim$epsilon), log10(Xdim$C))
  plot(log10(Xdim$epsilon), c(NA,der_Xdim), type = 'l', main = row.names[ds])
  abline(h = dim_RD[ds], col = 2)
}
dev.off()

