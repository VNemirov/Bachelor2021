#Matrix simulating a matrix n x m of N(0,1) variables
NormMatrix <- function(n, m){
  X <- matrix(rnorm(n*m),
              ncol = n)
  return(X)
}