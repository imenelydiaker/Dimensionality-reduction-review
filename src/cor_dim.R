## File : cor_dim.R
## Functions : Correlation Dimension and l'Hopital rule
## Author : Imene Kerboua, Mouad Laribia


## Correlation Dimension Function ####
corrDim <-function(data, epsilon = 10 ^ seq(-2, 1, length.out = 100)) {
    d <- as.matrix(dist(as.matrix(data)))
    n <- nrow(data)

    C <- numeric(length(epsilon))
    for (k in seq_along(C)) {
      C[k] <- 2 * sum(d <= epsilon[k]) / (n * (n-1))
    }

    return(list(C = C, epsilon = epsilon))

  }

## L’Hopital Rule Function ####
derivate <- function(x, y) {
  #(f(x2) - f(x1))/x2 - x1
  l <- length(y)
  delta_x <- x[2] - x[1]
  delta_fx <- y[2 : l] - y[1 : (l - 1)]
  Scorr <- delta_fx / delta_x
  return(c(Scorr))
}
