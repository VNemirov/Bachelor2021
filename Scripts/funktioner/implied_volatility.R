#Function finding implied volatility
implied_volatility <- function(S, K, r, t, endT, obsPrice, option = "call"){
  root_fun <- function(sig){
      eu_option_price(S=S, K=K, r=r, sigma = sig, t=t, endT=endT, option = option) - obsPrice  
  }
  
  uniroot(root_fun, c(-1,1))$root %>% 
    return()
}