#Defining the price function of a EU Call option:
eu_option_price <- function(S, K, r, sigma, t, endT, option = "call"){
  d_1 <- 1/(sigma*sqrt(endT-t))*(log(S/K)+(r+(sigma^2)/2)*(endT-t))
  d_2 <- d_1-sigma*sqrt(endT-t)
  
  #The price itself
  price <- S*pnorm(d_1)-exp(-r*(endT-t))*K*pnorm(d_2)
  
  if(option == "put"){
    price <- K*exp(-r*(endT-t))+price-S
  }
  
  return(price)
}