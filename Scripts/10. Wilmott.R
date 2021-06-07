mu = 0.01; r = 0.05; t = 0; endT = 1; K = 100; S0  = 100; dt = 1/10000
#Willmots Hedge Experiment
truevol <- 0.2
ivvol <- 0.1

#Hedging with Model Volatility (truevol):
hedgevol <- truevol
truehedge <- GBM_data(nsim = 10, endT = endT, mu = mu, sigma = truevol, S0 = S0, dt = dt, setSeed = T) %>% 
  mutate(Call_i = eu_option_price(S=value, K=K, r=r, sigma=ivvol, t=time, endT=endT, option = "call"),
         Call_h = eu_option_price(S=value, K=K, r=r, sigma=hedgevol, t=time, endT=endT, option = "call"),
         d1_h = (1/(hedgevol*sqrt(endT-time)))*((log(value/100))+(r+hedgevol^2/2)*(endT-time)),
         Delta_h = pnorm(d1_h),
         PnL = case_when(
           time == 0 ~ 0,
           time == 1 ~ max(value-K, 0)-lag(Delta_h)*value+(lag(Delta_h)*lag(value)-lag(Call_i))*exp(r*dt),
           TRUE ~ Call_i-lag(Delta_h)*value+(lag(Delta_h)*lag(value)-lag(Call_i))*exp(r*dt)
         ))
# Goal for this hedge
truehedge %>% filter(time == 0) %>% 
  mutate(Goal = exp(r*endT)*(Call_h-Call_i)) %>% 
  pull(Goal) %>% 
  unique()

#Hedging with Model Volatility (ivvol):
hedgevol <- ivvol
ivhedge <- GBM_data(nsim = 10, endT = endT, mu = mu, sigma = truevol, S0 = S0, dt = dt, setSeed = T) %>% 
  mutate(Call_i = eu_option_price(S=value, K=K, r=r, sigma=ivvol, t=time, endT=endT, option = "call"),
         Call_h = eu_option_price(S=value, K=K, r=r, sigma=hedgevol, t=time, endT=endT, option = "call"),
         d1_h = (1/(hedgevol*sqrt(endT-time)))*((log(value/100))+(r+hedgevol^2/2)*(endT-time)),
         Delta_h = pnorm(d1_h),
         PnL = case_when(
           time == 0 ~ 0,
           time == 1 ~ max(value-K, 0)-lag(Delta_h)*value+(lag(Delta_h)*lag(value)-lag(Call_i))*exp(r*dt),
           TRUE ~ Call_i-lag(Delta_h)*value+(lag(Delta_h)*lag(value)-lag(Call_i))*exp(r*dt)
         ))
#true
truehedge %>%
  filter(time != 1) %>% 
  group_by(variable) %>% 
  mutate(PnL = PnL*exp(r*(endT-time)),
         PnL = cumsum(PnL)) %>% 
  ggplot(aes(x = time, y = PnL, color = variable))+
  ggtitle(expression(paste("Hedging with ", sigma)))+
  xlab("")+ylab("")+
  geom_line(show.legend = F)+
  scale_y_continuous(expand = c(0,0,0.05,0))+
  scale_x_continuous(expand = c(0,0,0.05,0))+
  scale_color_brewer(type = "qual", palette = 8)+
  theme(plot.title = element_text(hjust = 0.5))+
  ggsave("./Output/Plots/hedgetrue.PNG",
         width = 16,
         height = 16,
         units = "cm")

#iv
ivhedge %>%
  filter(time != 1) %>% 
  group_by(variable) %>% 
  mutate(PnL = PnL*exp(r*(endT-time)),
         PnL = cumsum(PnL)) %>% 
  ggplot(aes(x = time, y = PnL, color = variable))+
  ggtitle(expression(paste("Hedging with ", sigma^{i})))+
  xlab("")+ylab("")+
  geom_line(show.legend = F)+
  scale_y_continuous(expand = c(0,0,0.05,0))+
  scale_x_continuous(expand = c(0,0,0.05,0))+
  scale_color_brewer(type = "qual", palette = 8)+
  theme(plot.title = element_text(hjust = 0.5))+
  ggsave("./Output/Plots/hedgeiv.PNG",
         width = 16,
         height = 16,
         units = "cm")

#both
rbind(cbind(truehedge, hedge = "1sand"),
      cbind(ivhedge, hedge = "2implicit")) %>% 
  filter(time != 1) %>% 
  group_by(hedge, 
           variable) %>% 
  mutate(PnL = PnL*exp(r*(endT-time)),
         PnL = cumsum(PnL)) %>% 
  ggplot(aes(x = time, y = PnL, color = variable))+
  xlab("Tid")+ylab("PnL")+
  geom_line(show.legend = F)+
  scale_y_continuous(expand = c(0,0,0.05,0))+
  scale_x_continuous(expand = c(0,0))+
  scale_color_brewer(type = "qual", palette = 8)+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )+
  facet_wrap(.~hedge)+
  ggsave("./Output/Plots/Wilmott.PNG",
         width = 32,
         height = 16,
         units = "cm")


# #Hedging with at vol in between 0.15
# #Hedging with Model Volatility (ivvol):
# hedgevol <- 0.15
# betweenhedge <- GBM_data(nsim = 10, endT = endT, mu = mu, sigma = truevol, S0 = S0, dt = dt, setSeed = T) %>% 
#   mutate(Call_i = eu_option_price(S=value, K=K, r=r, sigma=ivvol, t=time, endT=endT, option = "call"),
#          Call_h = eu_option_price(S=value, K=K, r=r, sigma=hedgevol, t=time, endT=endT, option = "call"),
#          d1_h = (1/(hedgevol*sqrt(endT-time)))*((log(value/100))+(r+hedgevol^2/2)*(endT-time)),
#          Delta_h = pnorm(d1_h),
#          PnL = case_when(
#            time == 0 ~ 0,
#            time == 1 ~ max(value-K, 0)-lag(Delta_h)*value+(lag(Delta_h)*lag(value)-lag(Call_i))*exp(r*dt),
#            TRUE ~ Call_i-lag(Delta_h)*value+(lag(Delta_h)*lag(value)-lag(Call_i))*exp(r*dt)
#          ))
# #trueb
# betweenhedge %>%
#   filter(time != 1) %>% 
#   group_by(variable) %>% 
#   mutate(PnL = PnL*exp(r*(endT-time)),
#          PnL = cumsum(PnL)) %>% 
#   ggplot(aes(x = time, y = PnL, color = variable))+
#   ggtitle(expression(paste("Hedging with inbetween")))+
#   xlab("")+ylab("")+
#   geom_line(show.legend = F)+
#   scale_y_continuous(expand = c(0,0,0.05,0))+
#   scale_x_continuous(expand = c(0,0,0.05,0))+
#   scale_color_brewer(type = "qual", palette = 8)+
#   theme_minimal()+
#   theme(plot.title = element_text(hjust = 0.5))+
#   ggsave("./Output/Plots/hedgetrue.PNG",
#          width = 16,
#          height = 16,
#          units = "cm")
# 
# 
# 