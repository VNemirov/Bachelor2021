BS_MC_AV_Price_estimator <- function(nsim = 10, maxN = 10000, r = 0.05, sigma = 0.1, S0 = 100, endT = 1, K = 100){
  df <- MC_AV_Estimator(nsim = maxN, mu = r, sigma = sigma, S = S0, endT = endT, paths = nsim) %>% 
    rowwise() %>% 
    mutate_all(function(x){max((x-K)*exp(-r*endT), 0)}) %>% 
    ungroup() %>% 
    #mutate_all(function(x){cumsum(x)/c(1:maxN)}) %>%
    mutate(n = 1:maxN)
                
  colnames(df)[1:nsim] <- sprintf("Sim %s", 1:nsim)
  
  return(df)
}