#Adding log increments to data
df_sp500 <- df_sp500 %>% 
  mutate(log_increments = log(GSPC.Close/lag(GSPC.Close))) %>% 
  drop_na()

df_rus2000 <- df_rus2000 %>% 
  mutate(log_increments = log(RUT.Close/lag(RUT.Close))) %>% 
  drop_na()

#Plotting
ggplot(df_sp500, aes(x = log_increments, y=..density../100))+
  geom_histogram(aes(fill = "turquoise"),
                 color = "black",
                 bins = 100)+
  geom_histogram(data = df_rus2000,
                 aes(fill = "salmon"),
                 color = "black",
                 bins = 100)+
  scale_fill_manual("",
                    values = c("turquoise", "salmon"),
                    labels = c("Russel 2000", "S&P 500"))+
  scale_y_continuous(labels = scales::percent,
                     expand = c(0.01,0,0.01,0)) +
  scale_x_continuous(limits = c(-0.1,0.1),
                     expand = c(0,0,0.01,0))+
  
  theme(legend.position=c(0.16,0.85),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  ggsave("./Output/Plots/Log inkrementer.PNG",
         width = 12,
         height = 12,
         units = "cm")

#QQnorms
ggplot(df_sp500, aes(sample = log_increments))+
  geom_qq(aes(color = "turquoise"),
          size = 0.5)+
  geom_qq(data = df_rus2000, aes(color = "salmon"),
          size = 0.5)+
  geom_qq_line(aes(color = "turquoise"))+
  geom_qq_line(data = df_rus2000, aes(color = "salmon"))+
  scale_y_continuous(limits = c(-0.1,0.1),
                     expand = c(0.01,0,0.01,0))+
  scale_color_manual("",
                    values = c("turquoise", "salmon"),
                    labels = c("Russel 2000", "S&P 500"))+
  
  theme(legend.position=c(0.16,0.85),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  ggsave("./Output/Plots/qqnorm.PNG",
         width = 12,
         height = 12,
         units = "cm")

#Shapiro tests:
shapiro.test(df_sp500$log_increments)
shapiro.test(df_rus2000$log_increments)
