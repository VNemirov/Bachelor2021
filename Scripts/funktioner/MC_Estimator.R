#Using (1.4) to calculate S_T and take mean:
MC_Estimator <- function(nsim = 100, mu = 0, sigma = 0.1, S0 = 0, endT = 1, paths = 1){
  hat_theta <- matrix(nrow = nsim, ncol = paths) %>% 
    as.data.frame()
  for(i in 1:paths){
    colnames(hat_theta)[i] <- paste("Path",i)
    hat_theta[,i] <- S0*exp((mu-sigma^2/2)*endT+sigma*sqrt(endT)*rnorm(nsim))
  }
  return(hat_theta)
}