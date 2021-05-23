#Aktie dynamik med forskellige størrelser af Delta t
rbind(
  #dt = 1/10
  cbind(GBM_data(nsim = 10, endT = 1, mu = 0.07, sigma = 0.2, S0 = 10, dt = 1/10, 
               setSeed = T, showGraph = F, addRiskFree = F, seed = 1), dt = "\u0394t=1/10"),
  #dt = 1/100
  cbind(GBM_data(nsim = 10, endT = 1, mu = 0.07, sigma = 0.2, S0 = 10, dt = 1/100,
                     setSeed = T, showGraph = F, addRiskFree = F, seed = 1), dt = "\u0394t=1/100"),
  #dt = 1/1000
  cbind(GBM_data(nsim = 10, endT = 1, mu = 0.07, sigma = 0.2, S0 = 10, dt = 1/1000,
                     setSeed = T, showGraph = F, addRiskFree = F, seed = 1), dt = "\u0394t=1/1000")
  ) %>% 
  ggplot(aes(x = time, y = value, color = variable))+
  xlab("Tid")+ylab("Pris")+
  geom_line(show.legend = F)+
  scale_x_continuous(expand=c(0,0,0.1,0))+
  
  facet_grid(cols = rev(vars(dt)))+
  ggsave("./Output/Plots/aktie.PNG",
         width = 16,
         height = 6,
         units = "cm")


# Trying to find a seed making the GBM go below 0
# i <- 704676
# while(T){
#   check <- GBM_data(nsim = 10, endT = 1, mu = 0.07, sigma = 0.2, S0 = 1, dt = 1/10, 
#            setSeed = T, showGraph = F, addRiskFree = F, seed = i) %>% 
#     filter(value<=0) %>% 
#     nrow()
#   if(check > 0){
#     break
#   }
#   else{
#     i <- i +1
#   }
#   print(i)
# }

GBM_data(nsim = 10, endT = 10, mu = 0.07, sigma = 1, S0 = 1, dt = 1, 
         setSeed = T, showGraph = F, addRiskFree = F, seed = 1) %>% 
  ggplot(aes(x = time, y = value, color = variable))+
  xlab("Tid")+ylab("Pris")+
  geom_line(show.legend = F)+
  scale_x_continuous(expand=c(0,0,0.1,0))+
  
  ggsave("./Output/Plots/negativaktie.PNG",
         width = 6,
         height = 6,
         units = "cm")






