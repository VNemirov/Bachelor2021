#Antithetic
MC_AV_Estimator <- function(nsim = 100, mu = 0.05, sigma = 0.1, S0 = 10, endT = 1, paths = 1){
  hat_theta <- matrix(nrow = nsim, ncol = paths) %>% 
    as.data.frame()
  for(i in 1:paths){
    Z <- rnorm(nsim)
    colnames(hat_theta)[i] <- paste("Path",i)
    v1 <- S0*exp((mu-sigma^2/2)*endT+sigma*sqrt(endT)*Z)
    v2 <- S0*exp((mu-sigma^2/2)*endT-sigma*sqrt(endT)*Z)
    hat_theta[,i] <- (v1+v2)/2
  }
  return(hat_theta)
}