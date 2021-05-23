#Cumulitative MC_Estimation with Strike (uses t = 0)
BS_MC_Price_estimator <- function(nsim = 10, maxN = 1000, mu = 0, sigma = 0.1, S = 100, endT = 1, K = 100, onevalue = FALSE){
  df <- replicate(nsim,replicate(maxN, 
                                 #Uses the MC Estimator function 
                                 MC_Estimator(nsim = 1, 
                                              mu = mu, 
                                              sigma = sigma, 
                                              S = S,
                                              endT = endT))) %>% 
    as.data.frame() %>% 
    rowwise() %>% 
    mutate_all(function(x){max((x-K)*exp(-mu*endT), 0)}) %>% 
    ungroup() %>% 
    mutate_all(function(x){cumsum(x)/c(1:maxN)}) %>%
    mutate(n = 1:maxN)
    #Chaning Col names  
    colnames(df)[1:nsim] <- sprintf("Sim %s", 1:nsim)
    
    if(onevalue){
      df[,1] %>% pull() %>% mean()
    }
    else{
      return(df)  
    }
}