#Greeks grafisk:
#Greeks function
greeks_list <- function(s, K, r, sigma, t, endT){
  
  d_1 <- 1/(sigma*sqrt(endT-t))*(log(s/K)+(r+1/2*sigma^2)*(endT-t))
  d_2 <- d_1 - sigma*sqrt(endT-t)
  
  greeks <- list(
    Delta = pnorm(d_1),
    Gamma = dnorm(d_1)/(s*sigma*sqrt(endT-t)),
    rho = K*(endT-t)*exp(-r*(endT-t))*pnorm(d_2),
    Theta = -(s*dnorm(d_1)*sigma)/(2*sqrt(endT-t))-r*K*exp(-r*(endT-t))*pnorm(d_1),
    Vega = s*dnorm(d_1)*sqrt(endT-t)
  )
  
  return(greeks)
}

greek_Delta <- function(s, K, r, sigma, t, endT){
  d_1 <- 1/(sigma*sqrt(endT-t))*(log(s/K)+(r+1/2*sigma^2)*(endT-t))
  pnorm(d_1) %>% return()
}

greek_Gamma <- function(s, K, r, sigma, t, endT){
  d_1 <- 1/(sigma*sqrt(endT-t))*(log(s/K)+(r+1/2*sigma^2)*(endT-t))
  dnorm(d_1)/(s*sigma*sqrt(endT-t)) %>% return()
}

greek_rho <- function(s, K, r, sigma, t, endT){
  d_1 <- 1/(sigma*sqrt(endT-t))*(log(s/K)+(r+1/2*sigma^2)*(endT-t))
  d_2 <- d_1 - sigma*sqrt(endT-t)
  K*(endT-t)*exp(-r*(endT-t))*pnorm(d_2) %>% return()
}

greek_Theta <- function(s, K, r, sigma, t, endT){
  d_1 <- 1/(sigma*sqrt(endT-t))*(log(s/K)+(r+1/2*sigma^2)*(endT-t))
  -(s*dnorm(d_1)*sigma)/(2*sqrt(endT-t))-r*K*exp(-r*(endT-t))*pnorm(d_1) %>% return()
}

greek_Vega <- function(s, K, r, sigma, t, endT){
  d_1 <- 1/(sigma*sqrt(endT-t))*(log(s/K)+(r+1/2*sigma^2)*(endT-t))
  s*dnorm(d_1)*sqrt(endT-t) %>% return()
}


#Delta
ggplot(data.frame(s = c(60,140)), aes(x = s))+
  xlab("S")+ylab("\u0394")+
  stat_function(fun = Vectorize(greek_Delta, c("s")),
                args = list(K = 100,
                            r = 0.05,
                            sigma = 0.2,
                            t = 0,
                            endT = 0.25),
                size = 2)+
  scale_y_continuous(expand = c(0,0,0.01,0))+
  
  ggtitle("Delta")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggsave("./Output/Plots/Delta.PNG",
         width = 12,
         height = 12,
         units = "cm")

#Delta with option
intercept <- eu_option_price(100,100,0.05,0.2,0,1)-pnorm(1/(0.2)*(log(1)+(0.04+(0.2^2)/2)))*100
data.frame(S = c(60,140)) %>% 
  ggplot(aes(x = S))+ylab("Pris")+
  stat_function(fun = Vectorize(call, "S"),
                args = list("K" = 100),
                size = 1.1)+
  stat_function(fun = function(x){pnorm(1/(0.2)*(log(1)+(0.04+(0.2^2)/2)))*x+intercept},
                color = "salmon",
                size = 1)+
  stat_function(fun = Vectorize(eu_option_price, "S"),
                args = list("K" = 100,
                            "r" = 0.05,
                            "sigma" = 0.2,
                            "t" = 0,
                            "endT" = 1,
                            "option" = "call"),
                size = 0.65)+
  geom_hline(aes(yintercept = 0))+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  
  ggsave("./Output/Plots/OptionwithDelta.PNG",
         width = 12,
         height = 12,
         units = "cm")

#Gamma
ggplot(data.frame(s = c(60,140)), aes(x = s))+
  xlab("S")+ylab("\u0393")+
  stat_function(fun = Vectorize(greek_Gamma, c("s")),
                args = list(K = 100,
                            r = 0.05,
                            sigma = 0.2,
                            t = 0,
                            endT = 0.25),
                size = 2)+
  scale_y_continuous(expand = c(0,0,0.01,0))+
  
  ggtitle("Gamma")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggsave("./Output/Plots/Gamma.PNG",
         width = 12,
         height = 12,
         units = "cm")

#rho
ggplot(data.frame(s = c(60,140)), aes(x = s))+
  xlab("S")+ylab("\u03C1")+
  stat_function(fun = Vectorize(greek_rho, c("s")),
                args = list(K = 100,
                            r = 0.05,
                            sigma = 0.2,
                            t = 0,
                            endT = 0.25),
                size = 2)+
  scale_y_continuous(expand = c(0,0,0.01,0))+
  
  ggtitle("Rho")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggsave("./Output/Plots/Rho.PNG",
         width = 12,
         height = 12,
         units = "cm")

#Theta
ggplot(data.frame(s = c(60,140)), aes(x = s))+
  xlab("S")+ylab("\u0398")+
  stat_function(fun = Vectorize(greek_Theta, c("s")),
                args = list(K = 100,
                            r = 0.05,
                            sigma = 0.2,
                            t = 0,
                            endT = 0.25),
                size = 2)+
  scale_y_continuous(expand = c(0,0,0.01,0))+
  
  ggtitle("Theta")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggsave("./Output/Plots/Theta.PNG",
         width = 12,
         height = 12,
         units = "cm")

#Vega
ggplot(data.frame(s = c(60,140)), aes(x = s))+
  xlab("S")+ylab(paste("𝓥"))+
  stat_function(fun = Vectorize(greek_Vega, c("s")),
                args = list(K = 100,
                            r = 0.05,
                            sigma = 0.2,
                            t = 0,
                            endT = 0.25),
                size = 2)+
  scale_y_continuous(expand = c(0,0,0.01,0))+
  
  ggtitle("Vega")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggsave("./Output/Plots/Vega.PNG",
         width = 12,
         height = 12,
         units = "cm")

