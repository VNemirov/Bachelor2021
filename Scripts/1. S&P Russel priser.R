# Sampler data fra Yahoo!
environment_sp500 <- new.env()
suppressWarnings(getSymbols(c("^GSPC"), 
                            env = environment_sp500,
                            src = "yahoo",
                            from = as.Date("2010-01-01"),
                            to = as.Date("2021-02-01")))

sp500 <- environment_sp500$GSPC

environment_rus2000 <- new.env()
suppressWarnings(getSymbols(c("^RUT"), 
                            env = environment_rus2000,
                            src = "yahoo",
                            from = as.Date("2010-01-01"),
                            to = as.Date("2021-02-01")))

sp500 <- environment_sp500$GSPC
rus2000 <- environment_rus2000$RUT

df_sp500 <- data.frame(date = index(sp500), coredata(sp500)) 
df_rus2000 <- data.frame(date = index(rus2000), coredata(rus2000)) 

#Plotter priser og gemmer
ggplot(data = df_sp500, 
       aes(x = date, y = GSPC.Close, color = "turquoise"))+
  geom_line(size = 0.25)+
  geom_line(data = df_rus2000, 
            aes(y = RUT.Close, color = "salmon"),
            size = 0.25)+
  ylab("Lukke pris")+
  xlab("Dato")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_date(expand = c(0,0))+
  theme(legend.position=c(0.16,0.85),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  scale_color_manual("",
                     values = c("turquoise", "salmon"),
                     labels = c("Russel 2000", "S&P 500"))+
  ggsave("./Output/Plots/S&P500-Russel2000 priser.PNG",
         width = 15,
         height = 5.5,
         units = "cm")
