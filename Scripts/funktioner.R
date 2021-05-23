#Matrix simulating a matrix n x m of N(0,1) variables
NormMatrix <- function(n, m){
  X <- matrix(rnorm(n*m),
              ncol = n)
  return(X)
}

#Function for GBM with plot option
GBM_data <- function(nsim = 10, endT = 1, mu = 0.05, sigma = 0.1, S0 = 10, dt = 1/100,
                     showGraph = FALSE, addRiskFree = TRUE,
                     setSeed = FALSE, seed = "1337"){
  #Seed
  if (setSeed){
    set.seed(seed)
  }
  
  #Setting times
  t<-seq(0, endT, by = dt)
  
  #Creating placeholder for S
  S<-matrix(nrow = length(t),
            ncol = nsim) %>% 
    as.data.frame()
  
  #Simulating normal random variables
  X <- NormMatrix(nsim, length(t))
  
  #Calculating nsim rows of S:
  for(i in 1:nsim){
    S[1,i] <- S0
    for(j in 1:(length(t)-1)){
      #Calculating the difference in S
      S[j+1,i] <- S[j,i]+mu*S[j,i]*dt+sigma*S[j,i]*sqrt(dt)*X[j,i]
    }
    colnames(S)[i]<-paste0("Sim ",i)
  }
  
  S <- S %>% 
    mutate(time = t) %>% 
    melt(id.vars = "time")
  
  if(showGraph){
    p<-ggplot(S, aes(x = time, y = value, color = variable))+
      geom_line(show.legend = FALSE)+
      theme_minimal()
    
    if(addRiskFree){
      p<-p+stat_function(fun = function(time){S0*exp(mu*time)},
                         color = "green")
    }
    print(p)
  }
  return(S)
}


#Reducing variance by the antithetic method (Using -X instead of X)
GBM_AV_data <- function(nsim = 10, endT = 1, mu = 0.05, sigma = 0.1, S0 = 10, dt = 1/100,
                     showGraph = FALSE, addRiskFree = TRUE,
                     setSeed = FALSE, seed = "1337"){
  #Seed
  if (setSeed){
    set.seed(seed)
  }
  #Setting times
  t <- seq(0, endT, by = dt)
  #Creating placeholder for S
  S <- matrix(nrow = length(t),
              ncol = 2*nsim) %>% 
    as.data.frame()
  #Simulating normal random variables
  X <- NormMatrix(nsim, length(t))
  #Calculating nsim rows of S:
  for(i in 1:nsim){
    S[,i][1] <- S[,i+nsim][1] <- S0
    for(j in 1:(length(t)-1)){
      #Calculating the difference in S and S_AV(Antithetic Variance)
      S[j+1,i] <- S[j,i]+mu*S[j,i]*dt+sigma*S[j,i]*sqrt(dt)*X[j,i]
      S[j+1,i+nsim] <- S[j,i]+ mu*S[j,i]*dt-sigma*S[j,i]*sqrt(dt)*X[j, i]
    }
    colnames(S)[i]<-paste0("Sim ",i,".1")
    colnames(S)[i+nsim]<-paste0("Sim ",i,".2")
  }
  #Reshaping data into 3 column data
  S <- S %>% 
    mutate(time = t) %>% 
    melt(id.vars = "time")
  
  if(showGraph){
    p<-ggplot(S, aes(x = time, y = value, color = variable))+
      geom_line(show.legend = FALSE)+
      theme_minimal()
    if(addRiskFree){
      p<-p+stat_function(fun = function(time){S0*exp(mu*time)},
                         color = "green")
    }
    print(p)
  }
  return(S)
}


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


#Defining a function for a call/put option (with option to add custom price):
call <- function(S, K, premium = 0){max(S-K, 0)-premium}
put <- function(S, K, premium = 0){max(K-S, 0)-premium}

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

#Function finding implied volatility
implied_volatility <- function(S, K, r, t, endT, obsPrice, option = "call"){
  root_fun <- function(sig){
      eu_option_price(S=S, K=K, r=r, sigma = sig, t=t, endT=endT, option = option) - obsPrice  
  }
  
  uniroot(root_fun, c(-1,1))$root %>% 
    return()
}


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

