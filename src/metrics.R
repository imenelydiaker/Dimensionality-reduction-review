## File : metrics.R
## Metrics : Trustworthiness measure, Continuity measure
## Authors : Imene Kerboua, Mouad Laribia


trustworthiness <- function(orig, proj, kn){
  ## Trustworthiness Measure (T) :
  ## measures the proportion of points that are
  ## too close together in the low-dimensional space
  ##
  ## INPUT
  # orig : coordinates of points in the original space
  # proj : coordinates of points in the projection space
  # kn : array indicating neighborhood(s) for which trustworthiness is calculated

  cat("\n calculating trustworthiness..")

  d_orig <- dist_euclidean(orig)
  d_proj <- dist_euclidean(proj)

  n <- nrow(d_orig)
  n_orig <- matrix(0, nrow = n, ncol = n)
  n_proj <- matrix(0, nrow = n, ncol = n)

  cat("\n ordering distance matrix.")
  for(i in 1:n){
    n_orig[i,] <- order(d_orig[i,])
    n_proj[i,] <- order(d_proj[i,])
  }

  ranks_orig <- rank(n_orig)

  trust <- c()
  for(k in kn){
    cat(paste("\n moving elements for k = ", k))
    moved <- c()
    for(i in seq(n)){
      moved <- c(moved, knn.ldim(n_orig, n_proj, i, k))
    }
    cat(paste("\n measure trust for k = ", k))
    trust <- c(trust, measure(moved, ranks_orig, n, k))
  }
  return(trust)
}



continuity <- function(orig, proj, kn){
  ## Continuity Measure (C) :
  ##
  ## INPUT
  # orig : coordinates of points in the original space
  # proj : coordinates of points in the projection space
  # kn : array indicating neighborhood(s) for which continuity is calculated

  cat("\n calculating continuity..")

  d_orig <- dist_euclidean(orig)
  d_proj <- dist_euclidean(proj)

  n <- nrow(d_orig)
  n_orig <- matrix(0, nrow = n, ncol = n)
  n_proj <- matrix(0, nrow = n, ncol = n)

  cat("\n ordering distance matrix.")
  for(i in 1:n){
    n_orig[i,] <- order(d_orig[i,])
    n_proj[i,] <- order(d_proj[i,])
  }

  ranks_proj <- rank(n_proj)

  continuity <- c()
  for(k in kn){
    cat(paste("\n moving elements for k = ", k))
    moved <- c()
    for(i in seq(n)){
      moved <- c(moved, knn.hdim(n_orig, n_proj, i, k))
    }
    cat(paste("\n measure continuity for k = ", k))
    continuity <- c(continuity, measure(moved, ranks_proj, n, k))
  }
  return(continuity)
}


knn.ldim <-  function(n_orig, n_proj, i,k){
  ## Indicates the set of points that are among the k nearest neighbors
  ## in the low-dimensional space but not in the high-dimensional space
  ##
  ## INPUT
  # n_orig : ordered (with indexes) distance matrix of points in the original space
  # n_proj : ordered (with indexes) distance matrix of points in the projection space
  # i : point i of the set
  # k : size of neighborhood

  return(setdiff(n_orig[i, 1:(k+1)], n_proj[i, 1:(k+1)]))
}


knn.hdim <-  function(n_orig, n_proj, i,k){
  ## Indicates the set of points that are among the k nearest neighbors
  ## in the high-dimensional space but not in the low-dimensional space
  ##
  ## INPUT
  # n_orig : ordered (with indexes) distance matrix of points in the original space
  # n_proj : ordered (with indexes) distance matrix of points in the projection space
  # i : point i of the set
  # k : size of neighborhood

  return(setdiff(n_proj[i, 1:(k+1)], n_orig[i, 1:(k+1)]))
}


scaling_term <- function(n, k){
  ## Term that scales measure between zero and one
  ##
  ## INPUT
  # n : number of datapoints
  # k : size of neighborhood

  if(k < (n/2))
    return(2 / (n * k * (2*n - 3*k - 1)))
  return(2 / (n * (n - k) * (n - k - 1)))
}


measure <- function(moved, ranks, n, k){
  ## Calculates the trustworthiness or continuity
  ##
  ## INPUT
  # moved :
  # ranks :
  # n :
  # k :

  s <- 0; n <- nrow(ranks)
  for(i in seq(n)){
    for(j in moved[i]){
      if(!is.na(j)) s = s + (ranks[i, j] - k)
    }
  }
  cat(paste("\n cumulated sum :", s))
  st <- scaling_term(n, k)
  cat(paste("\n scaling term", st))
  return(1 - st * s)
}


rank <- function(mat){
  ## Returns rank matrix from pairwise distance matrix
  ##
  ## INPUT
  # mat : distance matrix

  cat("\n calculating rank matrix.")
  n <- nrow(mat); m <- mat

  r <- matrix(0, nrow = n, ncol = n)
  for(i in seq(n)){
    for(j in seq(n)){
      r[i,j] <- which(m[i,] == j)
    }
  }
  return(r)
}


dist_euclidean <- function(set){
  ## N-dimensional Euclidean Distance
  ## columns : coordinates in n-dimension, rows : points i
  ##
  ## INPUT
  # set : matrix of points' coordinates

  cat("\n calculating euclidean distance.")
  # nr <- nrow(set); mdist <- matrix(0, nrow = nr, ncol = nr)
  # for(i in 1:(nr-1)){
  #   for(j in (i+1):nr){
  #     mdist[i,j] <- mdist[j,i] <- sqrt(sum((set[i,] - set[j,])^2))
  #   }
  # }
  mdist <- as.matrix(dist(as.matrix(set)))
  return(mdist)
}


## test distance function
# set <- matrix(runif(10,0,1), nrow = 5, ncol = 2)
# set
# distance <-  ndim.euclid.distance(set)
# distance
