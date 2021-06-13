#Paramete
endT = 1; sigma = 0.2; r = 0.05; K0 = 100; K1 = 110

#Creating data
gamma_data <- GBM_data(nsim = 1, setSeed = F, dt = 1/1000, S0 = 100, sigma = 0.1) %>%
    # filter(time %in% c(0:4/4)) %>%
    mutate(#K1 = value*exp(sigma*(endT-time)+r*(endT-time)),
           d1C = (log(value/K0)+(r+sigma^2/2)*(endT-time))/(sigma*sqrt(endT-time)),
           d2C = d1C-sigma*sqrt(endT-time),
           d1F = (log(value/K1)+(r+sigma^2/2)*(endT-time))/(sigma*sqrt(endT-time)),
           d2F = d1F-sigma*sqrt(endT-time),
           DeltaC = pnorm(d1C),
           DeltaF = pnorm(d1F),
           GammaC = dnorm(d1C)/(value*sigma*sqrt(endT-time)),
           GammaF = dnorm(d1F)/(value*sigma*sqrt(endT-time)),
           BetaF = round(-GammaC/GammaF,5),
           BetaS = round(DeltaF*GammaC/GammaF-DeltaC,5),
           priceC = value*pnorm(d1C)-exp(-r*(endT-time))*K0*pnorm(d2C),
           priceF = value*pnorm(d1F)-exp(-r*(endT-time))*K1*pnorm(d2F),
           PnL = case_when(
             BetaF == NaN | BetaS == NaN ~ 0,
             time == 0 ~ -(priceC+BetaF*priceF+BetaS*value),
             time != 0 & time != 1 ~ -(BetaF-lag(BetaF))*priceF-(BetaS-lag(BetaS))*value,
             time == 1 ~ lag(BetaF)*pmax(value-K1)+lag(BetaS)*value
           ),
           PnL = PnL*exp(r*(endT-time)),
           cumPnL = cumsum(-PnL)) %>% tail()

#all times
set.seed("0806")
Gamma_hedge <- data.frame(n = c(1:9,1:9*10,1:10*100), SE = c(1:9,1:9*10,1:10*100))
for(j in c(1:9,1:9*10,1:10*100)){
  print(paste("j:",j))
  hedges <- c()
  i<-0
  while(length(hedges)<=1000){
    i<-i+1
    if(i %% 100 == 0){print(paste("i:", i))}
    hedge <- GBM_data(nsim = 1, setSeed = F, dt = 1/j, S0 = 100, sigma = 0.2) %>%
      # filter(time %in% c(0:4/4)) %>%
      mutate(#K1 = value*exp(sigma*(endT-time)+r*(endT-time)),
        d1C = (log(value/K0)+(r+sigma^2/2)*(endT-time))/(sigma*sqrt(endT-time)),
        d2C = d1C-sigma*sqrt(endT-time),
        d1F = (log(value/K1)+(r+sigma^2/2)*(endT-time))/(sigma*sqrt(endT-time)),
        d2F = d1F-sigma*sqrt(endT-time),
        DeltaC = pnorm(d1C),
        DeltaF = pnorm(d1F),
        GammaC = dnorm(d1C)/(value*sigma*sqrt(endT-time)),
        GammaF = dnorm(d1F)/(value*sigma*sqrt(endT-time)),
        BetaF = round(-GammaC/GammaF,5),
        BetaS = round(DeltaF*GammaC/GammaF-DeltaC,5),
        priceC = value*pnorm(d1C)-exp(-r*(endT-time))*K0*pnorm(d2C),
        priceF = value*pnorm(d1F)-exp(-r*(endT-time))*K1*pnorm(d2F),
        PnL = case_when(
          BetaF == NaN | BetaS == NaN ~ 0,
          time == 0 ~ -(priceC+BetaF*priceF+BetaS*value),
          time != 0 & time != 1 ~ -(BetaF-lag(BetaF))*priceF-(BetaS-lag(BetaS))*value,
          time == 1 ~ lag(BetaF)*pmax(value-K1)+lag(BetaS)*value
        ),
        PnL = PnL*exp(r*(endT-time)),
        cumPnL = cumsum(-PnL))
    beta <- hedge %>% pull(BetaF) %>% abs() %>% max(na.rm = T)
    error <- abs(max(hedge %>% pull(value) %>% last()-K0,0)-hedge %>% pull(cumPnL) %>% last())
    
    if(beta >= 1.5 | is.na(beta)){
      # print(paste("error:",error))
      next
    }
    else{
      hedges <- c(hedges, error)
    }
  }
  Gamma_hedge[Gamma_hedge$n == j,]$SE <- sd(hedges, na.rm = TRUE)
}


ggplot(Gamma_hedge, aes(x = n, y = SE))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()
