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
