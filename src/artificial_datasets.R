## File : artificial_datasets.R
## Generates 4 artifical datasets : Swissroll, Helix, Twinpeaks, Broken Swissroll
## Authors : Imene Kerboua, Mouad Laribia

rm(list = ls())

# Swissroll Dataset
# sample from uniform distribution in [0,1]
# N : number of samples
# noise : Gaussian noise
swissroll <- function(N, min = 0, max = 1, noise = 0){
  t <- 1.5 * pi * (1 + 2 * runif(N, min, max))
  X <- cbind(x = t * cos(t), y = 30 * runif(N, min, max), z = t * sin(t)) + noise * rnorm(N * 3, 0, 1)
  return(X[order(t),])
}

# Broken Swissroll Dataset
# sample from uniform distribution in [0,1]
# N : number of samples
# noise : Gaussian noise
brokenswissroll <- function(N, min = 0, max = 1, noise = 0){
  t <- c()
  t <- c(t, 1.5 * pi * (1 + 2 * runif(ceiling(N/2), min, max) * .4), 1.5 * pi * (1 + 2 * (runif(floor(N/2), min, max) * .4 + .6)))
  X <- cbind(x = t * cos(t), y = 30 * runif(N, min, max), z = t * sin(t)) + noise * rnorm(N * 3, 0, 1)
  return(X[order(t),])
}

# Helix Dataset
# sample from uniform distribution in [0,1]
# N : number of samples
# noise : Gaussian noise
helix <- function(N, noise = 0){
  t <- seq(1:N) / N
  t <- t * 2 * pi
  X <- cbind(x = (2 + cos(8 * t)) * cos(t), y = (2 + cos(8 * t)) * sin(t), z = sin(8 * t)) + noise * rnorm(N * 3, 0, 1)
  return(X[order(t),])
}

# Twinpeaks Dataset
# sample from uniform distribution in [0,1]
# N : number of samples
# noise : Gaussian noise
twinpeaks <- function(N, min = 0, max = 1, noise = 0){
  xy <-  1 - 2 * matrix(runif(N*2, min, max), nrow = N, ncol= 2)
  z <- sin(pi * xy[,1]) * tanh(3 * xy[,2])
  X <- cbind(x = xy[,1], y = xy[,2], z = z) + noise * rnorm(N * 3, 0, 1)
  return(X[order(z),])
}


# High Dimensional Dataset
# sample from uniform distribution in [0,1] a 5 dim dataset embedded in a 10 dim space
# N : number of samples
# noise : Gaussian noise
highDimensionalData <- function(N, min = 0, max = 1, noise = 0){
  t <- matrix(runif(N * 5, min, max), nrow = N, ncol = 5)
  X <- cbind(cos(t[,1]), tanh(3 * t[,2]), t[,1] + t[,3], t[,4] * sin(t[,2]), sin(t[,1]) + t[,5], t[,5] * cos(t[,2]), t[,5] + t[,4], t[,2], t[,3] * t[,4], t[,1]) + noise * rnorm(N * 10, 0, 1)
  return(X)
}

