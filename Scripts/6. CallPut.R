#Call Put
  ggplot(data.frame(S = c(80, 120), V = c(80-100,120-100)), aes(x = S, y = V))+
    ylab("") + xlab("")+
    ggtitle("Call Option")+
    geom_line(color = brewer.pal(3,"Dark2")[3],
              size = 1.2)+
    stat_function(fun = Vectorize(call, "S"),
                  args = list(K=100,
                              premium = 0),
                  color = brewer.pal(3,"Dark2")[1],
                  size = 1.2)+
    geom_hline(yintercept = 0)+
    scale_x_continuous(expand = c(0,0,0.1,0))+
    scale_y_continuous(expand = c(0,0))+
    ggsave("./Output/Plots/Call.PNG",
           width = 8,
           height = 8,
           units = "cm")
  
  ggplot(data.frame(S = c(80, 120), V = c(80-100,120-100)), aes(x = S, y = V))+
    ylab("") + xlab("")+
    ggtitle("Put Option")+
    geom_line(color = brewer.pal(3,"Dark2")[3],
              size = 1.2)+
    stat_function(fun = Vectorize(put, "S"),
                  args = list(K=100,
                              premium = 0),
                  color = brewer.pal(3,"Dark2")[2],
                  size = 1.2)+
    geom_hline(yintercept = 0)+
    scale_x_continuous(expand = c(0,0,0.1,0))+
    scale_y_continuous(expand = c(0,0))+
    
    ggsave("./Output/Plots/Put.PNG",
           width = 8,
           height = 8,
           units = "cm")

  #Changing Params
  #Computing Prices of different Call options
    #Changing T-t
    ggplot(data.frame(S = c(80, 120)), aes(x = S))+
      ylab("") + xlab("")+
      stat_function(fun = Vectorize(call, "S"),
                    args = list(K=100),
                    size = 1)+
      stat_function(fun = Vectorize(eu_option_price, c("S")),
                    args = list(t = 0.25, endT = 1, K = 100, sigma = 0.2, r = 0.05),
                    aes(color = "0.25"),
                    size = 0.75)+
      stat_function(fun = Vectorize(eu_option_price, c("S")),
                    args = list(t = 0.50, endT = 1, K = 100, sigma = 0.2, r = 0.05),
                    aes(color = "0.50"),
                    size = 0.75)+
      stat_function(fun = Vectorize(eu_option_price, c("S")),
                    args = list(t = 0.75, endT = 1, K = 100, sigma = 0.2, r = 0.05),
                    aes(color = "0.75"),
                    size = 0.75)+
      scale_colour_manual(name = "Tid (t):",
                          values = c("0.25" = brewer.pal(3,"Dark2")[1],
                                     "0.50" = brewer.pal(3,"Dark2")[2],
                                     "0.75" = brewer.pal(3,"Dark2")[3]),
                          labels = c("0.25", "0.50", "0.75"))+
      scale_y_continuous(limits = c(0, 25))+
      
      theme(legend.position = "top")+
      ggsave("./Output/Plots/CallExpiry.PNG",
             width = 12,
             height = 12,
             units = "cm")
    #Changing r
    ggplot(data.frame(S = c(80, 120)), aes(x = S))+
      ylab("") + xlab("")+
      stat_function(fun = Vectorize(call, "S"),
                    args = list(K=100),
                    size = 1)+
      stat_function(fun = Vectorize(eu_option_price, c("S")),
                    args = list(t = 0.25, endT = 1, K = 100, sigma = 0.2, r = 0.05),
                    aes(color = "0.00"),
                    size = 0.75)+
      stat_function(fun = Vectorize(eu_option_price, c("S")),
                    args = list(t = 0.25, endT = 1, K = 100, sigma = 0.2, r = 0.1),
                    aes(color = "0.05"),
                    size = 0.75)+
      stat_function(fun = Vectorize(eu_option_price, c("S")),
                    args = list(t = 0.25, endT = 1, K = 100, sigma = 0.2, r = 0.15),
                    aes(color = "0.10"),
                    size = 0.75)+
      scale_colour_manual(name = "Risiko fri rente (r):",
                          values = c("0.00" = brewer.pal(3,"Dark2")[1],
                                     "0.05" = brewer.pal(3,"Dark2")[2],
                                     "0.10" = brewer.pal(3,"Dark2")[3]),
                          labels = c("0.05", "0.1", "0.15"))+
      scale_y_continuous(limits = c(0, 25))+
      
      theme(legend.position = "top")+
      ggsave("./Output/Plots/CallRiskfree.PNG",
             width = 12,
             height = 12,
             units = "cm")
    
    #Changing Volatility
    ggplot(data.frame(S = c(80, 120)), aes(x = S))+
      ylab("") + xlab("")+
      stat_function(fun = Vectorize(call, "S"),
                    args = list(K=100),
                    size = 1)+
      stat_function(fun = Vectorize(eu_option_price, c("S", "r")),
                    args = list(t = 0.25, endT = 1, K = 100, sigma = 0.2, r = 0.05),
                    aes(color = "0.2"),
                    size = 0.75)+
      stat_function(fun = Vectorize(eu_option_price, c("S")),
                    args = list(t = 0.25, endT = 1, K = 100, sigma = 0.4, r = 0.05),
                    aes(color = "0.4"),
                    size = 0.75)+
      stat_function(fun = Vectorize(eu_option_price, c("S")),
                    args = list(t = 0.25, endT = 1, K = 100, sigma = 0.6, r = 0.05),
                    aes(color = "0.6"),
                    size = 0.75)+
      scale_colour_manual(name = "Volatiliet (\u03C3)",
                          values = c("0.2" = brewer.pal(3,"Dark2")[1],
                                     "0.4" = brewer.pal(3,"Dark2")[2],
                                     "0.6" = brewer.pal(3,"Dark2")[3]),
                          labels = c("0.2", "0.4", "0.6"))+
      scale_y_continuous(limits = c(0, 25))+
      
      theme(legend.position = "top")+
      ggsave("./Output/Plots/CallVolatility.PNG",
             width = 12,
             height = 12,
             units = "cm")
    
    
    
    

#MC_AV estimation
#Changing time
set.seed("1337")
melt(BS_MC_Price_estimator(mu = 0.05, sigma = 0.2, S = 100, endT = 0.75, K = 100),
     id.vars = "n") %>% 
  ggplot(aes(x = n, y = value, group = variable))+
  ylab("") + xlab("")+
  geom_line(aes(color = "0.25"))+
  geom_line(aes(color = "0.50"),
            melt(BS_MC_Price_estimator(mu = 0.05, sigma = 0.2, S = 100, endT = 0.5, K = 100),id.vars = "n"))+
      geom_line(aes(color = "0.75"),
                melt(BS_MC_Price_estimator(mu = 0.05, sigma = 0.2, S = 100, endT = 0.25, K = 100),
                     id.vars = "n"))+
      scale_y_continuous(limits = c(4,20), expand = c(0,0))+
      scale_x_continuous(expand = c(0,0,0.01,0))+
      geom_hline(yintercept = c(eu_option_price(S = 100, K = 100, r = 0.05, sigma = 0.2, t = 0, endT = 0.75),
                                eu_option_price(S = 100, K = 100, r = 0.05, sigma = 0.2, t = 0, endT = 0.50),
                                eu_option_price(S = 100, K = 100, r = 0.05, sigma = 0.2, t = 0, endT = 0.25)),
                 color = "black")+
      scale_colour_manual(name = "Tid (t):",
                          values = c("0.25" = brewer.pal(3,"Dark2")[1],
                                     "0.50" = brewer.pal(3,"Dark2")[2],
                                     "0.75" = brewer.pal(3,"Dark2")[3]),
                          labels = c("0.25", "0.50", "0.75"))+
      
      theme(legend.position = "top")+
  ggsave("./Output/Plots/MCCalltime.PNG",
         width = 12,
         height = 12,
         units = "cm")
    
    #Changing risk free rate
    melt(BS_MC_Price_estimator(mu = 0.05, sigma = 0.2, S = 100, endT = 0.75, K = 100),
         id.vars = "n") %>% 
      ggplot(aes(x = n, y = value, group = variable))+
      ylab("") + xlab("")+
      geom_line(aes(color = "0.05"))+
      geom_line(aes(color = "0.15"),
                melt(BS_MC_Price_estimator(mu = 0.15, sigma = 0.2, S = 100, endT = 0.75, K = 100),
                     id.vars = "n"))+
      geom_line(aes(color = "0.25"),
                melt(BS_MC_Price_estimator(mu = 0.25, sigma = 0.2, S = 100, endT = 0.75, K = 100),
                     id.vars = "n"))+
      scale_y_continuous(limits = c(4,20), expand = c(0,0))+
      scale_x_continuous(expand = c(0,0,0.01,0))+
      geom_hline(yintercept = c(eu_option_price(S = 100, K = 100, r = 0.05, sigma = 0.2, t = 0, endT = 0.75),
                                eu_option_price(S = 100, K = 100, r = 0.15, sigma = 0.2, t = 0, endT = 0.75),
                                eu_option_price(S = 100, K = 100, r = 0.25, sigma = 0.2, t = 0, endT = 0.75)),
                 color = "black")+
      scale_colour_manual(name = "Risiko fri rente (r):",
                          values = c("0.05" = brewer.pal(3,"Dark2")[1],
                                     "0.15" = brewer.pal(3,"Dark2")[2],
                                     "0.25" = brewer.pal(3,"Dark2")[3]),
                          labels = c("0.05", "0.15", "0.25"))+
      
      theme(legend.position = "top")+
      ggsave("./Output/Plots/MCCallriskfree.PNG",
             width = 12,
             height = 12,
             units = "cm")
    
    #Changing volatility
    BS_MC_Price_estimator(mu = 0.05, sigma = 0.2, S = 100, endT = 0.75, K = 100) %>% 
      melt(id.vars = "n") %>% 
      ggplot(aes(x = n, y = value, group = variable))+
      ylab("") + xlab("")+
      geom_line(aes(color = "0.2"),
                #alpha = 0.8
      )+
      geom_line(aes(color = "0.3"),
                #alpha = 0.6,
                melt(BS_MC_Price_estimator(mu = 0.05, sigma = 0.3, S = 100, endT = 0.75, K = 100),
                     id.vars = "n"))+
      geom_line(aes(color = "0.4"),
                #alpha = 0.4,
                melt(BS_MC_Price_estimator(mu = 0.05, sigma = 0.4, S = 100, endT = 0.75, K = 100),
                     id.vars = "n"))+
      scale_y_continuous(limits = c(4,20), expand = c(0,0))+
      scale_x_continuous(expand = c(0,0,0.01,0))+
      geom_hline(yintercept = c(eu_option_price(S = 100, K = 100, r = 0.05, sigma = 0.2, t = 0.25, endT = 1),
                                eu_option_price(S = 100, K = 100, r = 0.05, sigma = 0.3, t = 0.25, endT = 1),
                                eu_option_price(S = 100, K = 100, r = 0.05, sigma = 0.4, t = 0.25, endT = 1)),
                 color = "black")+
      scale_colour_manual(name = "Volatilitet (\u03C3):",
                          values = c("0.2" = brewer.pal(3,"Dark2")[1],
                                     "0.3" = brewer.pal(3,"Dark2")[2],
                                     "0.4" = brewer.pal(3,"Dark2")[3]),
                          labels = c("0.2", "0.3", "0.4"))+
      
      theme(legend.position = "top")+
      ggsave("./Output/Plots/MCCallVolatility.PNG",
             width = 12,
             height = 12,
             units = "cm")
    
    