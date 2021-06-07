#Parameters
endT = 1; sigma = 0.2; r = 0.05; K0 = 100 # Define K1 s.t. d1=0 (this maxes phi(d1) which will maximize Gamma)

#Creating data
# gamma_data <-
  GBM_data(nsim = 1, setSeed = T, seed = "0806", dt = 1/1000, S0 = 100, sigma = 0.2) %>%
    filter(time %in% c(0:4/4)) %>%
    mutate(K1 = value*exp(sigma*(endT-time)+r*(endT-time)),
           d1C = (log(value/K0)+(r+sigma^2/2)*(endT-time))/(sigma*sqrt(endT-time)),
           d2C = d1C-sigma*sqrt(endT-time),
           d1F = (log(value/K1)+(r+sigma^2/2)*(endT-time))/(sigma*sqrt(endT-time)),
           d2F = d1F-sigma*sqrt(endT-time),
           DeltaC = pnorm(d1C),
           DeltaF = pnorm(d1F),
           GammaC = dnorm(d1C)/(value*sigma*sqrt(endT-time)),
           GammaF = dnorm(d1F)/(value*sigma*sqrt(endT-time)),
           BetaF = -GammaC/GammaF,
           BetaS = -BetaF*DeltaF-DeltaC,
           priceC = value*pnorm(d1C)-exp(-r*(endT-time))*K0*pnorm(d2C),
           priceF = value*pnorm(d1F)-exp(-r*(endT-time))*K1*pnorm(d2F),
           PnL = case_when(
             time == 0 ~ 0,
             time != 0 & time != 1 ~ priceC+BetaF*priceF+BetaS*value-(lag(priceC)+lag(BetaF)*lag(priceF)+lag(BetaS)*lag(value))*exp(r*(time-lag(time))),
             time == 1 ~ lag(BetaF)*ifelse(value-lag(K1)<=0,0,value-lag(K1))+lag(BetaS)*value-(lag(priceC)+lag(BetaF)*lag(priceF)+lag(BetaS)*lag(value))*exp(r*(time-lag(time)))
           ),
           cumPnL = cumsum(PnL)) %>% tail()
  
  GBM_data(nsim = 1, setSeed = T, seed = "0806", dt = 1/1000, S0 = 100, sigma = 0.2) %>%
    # filter(time %in% c(0:4/4)) %>%
    mutate(K1 = value*exp(sigma*(endT-time)+r*(endT-time)),
           d1C = (log(value/K0)+(r+sigma^2/2)*(endT-time))/(sigma*sqrt(endT-time)),
           d2C = d1C-sigma*sqrt(endT-time),
           d1F = (log(value/K1)+(r+sigma^2/2)*(1))/(sigma*sqrt(1)),
           d2F = d1F-sigma*sqrt(1),
           DeltaC = pnorm(d1C),
           DeltaF = pnorm(d1F),
           GammaC = dnorm(d1C)/(value*sigma*sqrt(endT-time)),
           GammaF = dnorm(d1F)/(value*sigma*sqrt(1)),
           BetaF = -GammaC/GammaF,
           BetaS = -BetaF*DeltaF-DeltaC,
           priceC = value*pnorm(d1C)-exp(-r*(endT-time))*K0*pnorm(d2C),
           priceF = value*pnorm(d1F)-exp(-r*(1))*K1*pnorm(d2F),
           cashflow = case_when(
             time == 0 ~ -(priceC+BetaF*priceF+BetaS*value),
             time != 0 & time != 1 ~ (BetaF-lag(BetaF))*priceF+(BetaS-lag(BetaS))*value,
             time == 1 ~ -lag(BetaF)*pmax(value-lag(K1),0)-lag(BetaS)*value
           )) %>% tail()
  
  
  
  