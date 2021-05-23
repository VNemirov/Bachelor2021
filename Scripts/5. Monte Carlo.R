set.seed("1337")
#Normal
cbind(MC_Estimator(nsim = 1000, mu = 0.07, sigma = 0.2, S0 = 10, paths = 10),
      n = 1:1000) %>%
  melt(id.vars = "n") %>% 
  group_by(variable) %>% 
  mutate(value = cumsum(value)/n) %>% 
  ggplot(aes(x = n, y = value, color = variable))+
  ylab("Pris")+xlab("Antal Simulationer (n)")+
  geom_line(show.legend = F)+
  geom_hline(yintercept = 10*exp(0.07))+
  scale_x_continuous(expand=c(0,0,0.01,0))+
  scale_y_continuous(limits = 10*exp(0.07)+c(-0.5,0.5))+
  
  ggsave("./Output/Plots/MC.PNG",
         width = 16,
         height = 6,
         units = "cm")

#Antithetic
cbind(MC_AV_Estimator(nsim = 1000, mu = 0.07, sigma = 0.2, S0 = 10, paths = 10),
      n = 1:1000) %>%
  melt(id.vars = "n") %>% 
  group_by(variable) %>% 
  mutate(value = cumsum(value)/n) %>% 
  ggplot(aes(x = n, y = value, color = variable))+
  ylab("Pris")+xlab("Antal Simulationer (n)")+
  geom_line(show.legend = F)+
  geom_hline(yintercept = 10*exp(0.07))+
  scale_x_continuous(expand=c(0,0,0.01,0))+
  scale_y_continuous(limits = 10*exp(0.07)+c(-0.5,0.5))+
  
  ggsave("./Output/Plots/MC_AV.PNG",
         width = 16,
         height = 6,
         units = "cm")
