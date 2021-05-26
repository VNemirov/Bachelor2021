#Parameters
endT = 1; sigma = 0.2; r = 0.05; K = 100

#Creating data
delta_data <- GBM_data(nsim = 1, setSeed = T, seed = "0806", dt = 1/1000, S0 = 100, sigma = 0.2) %>% 
  mutate(d1 = (log(value/K)+(r+sigma^2/2)*(endT-time))/(sigma*sqrt(endT-time)),
         d2 = d1-sigma*sqrt(endT-time),
         optionprice = value*pnorm(d1)-exp(-r*(endT-time))*K*pnorm(d2),
         Delta = pnorm(d1)) 

#Visuals
  ggplot(delta_data, 
         aes(x=time, y = value))+
    xlab("Tid")+ylab("S")+
    geom_line(color = brewer.pal(3,"Dark2")[1])+
    geom_label_repel(aes(label = paste(ifelse(time %% 0.25 == 0, round(value, 2),""))),
                     segment.curvature = -0.1,
                     max.overlaps = Inf,
                     size = 11)+
    
    ggsave("./Output/Plots/StockwithDelta1.PNG",
           width = 30,
           height = 30,
           units = "cm")
  
  ggplot(delta_data,
         aes(x=time, y = Delta))+
    xlab("Tid")+ylab("\u0394")+
    geom_line(color = brewer.pal(3,"Dark2")[2])+
    geom_label_repel(aes(label = paste(ifelse(time %% 0.25 == 0, round(Delta, 2),""))),
                     segment.curvature = -0.1,
                     max.overlaps = Inf,
                     size = 11)+
    ggsave("./Output/Plots/StockwithDelta2.PNG",
           width = 30,
           height = 30,
           units = "cm")
  
#Cashflow approach
delta_data %>% 
  # filter(time %in% c(0:4/4)) %>%
  mutate(cashflow = case_when(
    time == 0 ~ optionprice - Delta*value,
    time != 0 & time != 1 ~ -(Delta-lag(Delta))*value,
    time == 1 ~ lag(Delta)*value
  ),
  cashflow = cashflow*exp(r*(1-time)),
  cumflow = cumsum(cashflow)) %>% tail()


#Hedging 4 times
Paths <- 1000
Hedge1 <- cbind(S = rep(NA, Paths), value = rep(NA, Paths), error = rep(NA, Paths))

for(i in 1:Paths){
  data <- GBM_data(nsim = 1, dt = 1/1000, S0 = 100, sigma = 0.2) %>% 
    filter(time %in% c(0:4/4)) %>% 
    mutate(d1 = (1/(sigma*sqrt(1-time)))*((log(value/K))+(r+sigma^2/2)*(1-time)),
           d2 = d1-sigma*sqrt(endT-time),
           optionprice = value*pnorm(d1)-exp(-r*(endT-time))*K*pnorm(d2),
           Delta = pnorm(d1)) %>%
    mutate(cashflow = case_when(
      time == 0 ~ optionprice - Delta*value,
      time != 0 & time != 1 ~ -(Delta-lag(Delta))*value,
      time == 1 ~ lag(Delta)*value
    ),
    cashflow = cashflow*exp(r*(1-time)),
    cumflow = cumsum(cashflow)) %>% tail()
  
  Hedge1[i, 1] <- data %>% 
    filter(time == 1) %>% 
    pull(value)
  
  Hedge1[i, 2] <- data %>% 
    filter(time == 1) %>% 
    pull(cumflow)
  
  Hedge1[i, 3] <- abs(max(Hedge1[i, 1]-K, 0)-Hedge1[i, 2])
}

#Hedging 1000 times
Hedge2 <- cbind(S = rep(NA, Paths), value = rep(NA, Paths), error =  rep(NA, Paths))

for(i in 1:Paths){
  data <- GBM_data(nsim = 1, dt = 1/1000, S0 = 100, sigma = 0.2) %>% 
    mutate(d1 = (1/(sigma*sqrt(1-time)))*((log(value/K))+(r+sigma^2/2)*(1-time)),
           d2 = d1-sigma*sqrt(endT-time),
           optionprice = value*pnorm(d1)-exp(-r*(endT-time))*K*pnorm(d2),
           Delta = pnorm(d1)) %>%
    mutate(cashflow = case_when(
      time == 0 ~ optionprice - Delta*value,
      time != 0 & time != 1 ~ -(Delta-lag(Delta))*value,
      time == 1 ~ lag(Delta)*value
    ),
    cashflow = cashflow*exp(r*(1-time)),
    cumflow = cumsum(cashflow)) %>% tail()
  
  Hedge2[i, 1] <- data %>% 
    filter(time == 1) %>% 
    pull(value)
  
  Hedge2[i, 2] <- data %>% 
    filter(time == 1) %>% 
    pull(cumflow)
  
  Hedge2[i, 3] <- abs(max(Hedge2[i, 1]-K, 0)-Hedge2[i, 2])
}

  Hedge1 %>% 
    as.data.frame() %>% 
    ggplot(aes(x = S, y = value))+
    xlab("")+ylab("")+
   # ggtitle(paste0("Resultat af 1000 simulerede Delta afdækningenr hver med 4 rebalanceringer (SE=",round(sd(Hedge1[,3]),2),")"))+
    stat_function(fun = Vectorize(call,"S"),
                  args = list("K" = 100),
                  size = 1)+
    geom_point(size = 0.75,
               color = "salmon")+
    geom_hline(aes(yintercept = 0))+
    scale_x_continuous(limit = c(75, 175),
                       expand = c(0,0))+
    scale_y_continuous(limit = c(-20, 80),
                       expand = c(0,0))+
    
    annotate("label", x = 100, y = 50, label = paste0("SE=",round(sd(Hedge1[,3]),2)), size = 6)+ 
    ggsave("./Output/Plots/Deltahedge4.PNG",
           width = 16,
           height = 16,
           units = "cm")
  
  Hedge2 %>% 
    as.data.frame() %>% 
    ggplot(aes(x = S, y = value))+
    xlab("")+ylab("")+
    stat_function(fun = Vectorize(call,"S"),
                  args = list("K" = 100),
                  size = 1)+
    geom_point(size = 0.75,
               color = "salmon")+
    geom_hline(aes(yintercept = 0))+
    scale_x_continuous(limit = c(75, 175),
                       expand = c(0,0))+
    scale_y_continuous(limit = c(-20, 80),
                       expand = c(0,0))+
    
    annotate("label", x = 100, y = 50, label = paste0("SE=",round(sd(Hedge2[,3]),2)), size = 6)+
    ggsave("./Output/Plots/Deltahedge1000.PNG",
           width = 16,
           height = 16,
           units = "cm")

#Takes long time to run so output has been saved
#hedge 1:1000 times 
# simhedges <- data.frame(n = c(1:9,1:9*10,1:9*100,1:10*1000), SE = c(1:9,1:9*10,1:9*100,1:10*1000))
# for (j in c(1:9,1:9*10,1:9*100,1:10*1000)){
#   loop_time <- Sys.time()
#   print(j)
#   Hedge3 <- cbind(S = rep(NA, Paths), value = rep(NA, Paths), error = rep(NA, Paths))
#   for(i in 1:100){
#     data <- GBM_data(nsim = 1, dt = 1/j, S0 = 100, sigma = 0.2) %>%
#       rowid_to_column() %>%
#       mutate(d1 = (1/(sigma*sqrt(1-time)))*((log(value/K))+(r+sigma^2/2)*(1-time)),
#              d2 = d1-sigma*sqrt(endT-time),
#              optionprice = value*pnorm(d1)-exp(-r*(endT-time))*K*pnorm(d2),
#              Delta = pnorm(d1)) %>%
#       mutate(cashflow = case_when(
#         time == 0 & rowid == 1~ optionprice - Delta*value,
#         time != 0 & time != 1 & rowid != j + 1 & rowid != 1~ -(Delta-lag(Delta))*value,
#         rowid == max(rowid) ~ lag(Delta)*value
#       ),
#       cashflow = cashflow*exp(r*(1-time)),
#       cumflow = cumsum(cashflow))
#     
#     Hedge3[i, 1] <- (data %>% pull(value) %>% rev())[1]
#     
#     Hedge3[i, 2] <- (data %>%
#                        pull(cumflow) %>% 
#                        rev())[1]
#     
#     Hedge3[i, 3] <- abs(max(Hedge3[i, 1]-K, 0)-Hedge3[i, 2])
#   }
#   print(sd(Hedge3[,3]))
#   simhedges$SE[[j]] <- sd(Hedge3[,3])
#   print(Sys.time()-loop_time)
# }
# end_time <- Sys.time()
# end_time-start_time
#Saving data for recreation
# save(simhedges, file = "./Output/simhedges.Rdata")
load("./Output/simhedges.Rdata")


simhedges[1:1000,] %>%
  drop_na() %>% 
  ggplot(aes(x = n, y = SE))+
  xlab("Antal rebalanceringer")+
  geom_point(size = 0.75)+
  # geom_hline(yintercept = 0,
  #            size = 1)+
  scale_x_log10(expand = c(0.01,0,0.01,0))+
  scale_y_log10()+
  geom_smooth(se=F,
              method = "lm")+
  annotate("label", x = 100, y = 3, label = paste("Slope=-0.5"), size = 6)+
  ggsave("./Output/Plots/Hedgeerror.PNG",
         width = 16,
         height = 16,
         units = "cm")
