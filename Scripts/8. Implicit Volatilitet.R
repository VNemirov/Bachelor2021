# Loading Option Prices for (EU options) SPX and (AM options) SPY and saving it once
#Date of save 26/05/2021 ~ 17:00
# #SPX Options
# SPX_Options <-  getOptionChain(Symbols = "^SPX",
#                                Exp = c("2021", "2022"),
#                                data_source = "yahoo")
# save(SPX_Options, file = "./Output/SPX_Options_26052021.Rdata")
# 
# #SPY Options
# SPY_Options <- getOptionChain(Symbols = "SPY",
#                               Exp = c("2021", "2022"),
#                               data_source = "yahoo")
# save(SPY_Options, file = "./Output/SPY_Options_26052021.Rdata")
# #Commented out s.t. it is not run again

#Download date (Before open! so actually prices are from 2021-03-19)
download_date <- as.Date("2021-05-26")

#Loading downloaded data
load("./Output/SPX_Options_26052021.Rdata")
load("./Output/SPY_Options_26052021.Rdata")

#Getting Prices
Prices26052021 <- read_table2("Input/Option Prices/Prices26052021.txt")

#Testing with June 18 Calls s.t. T-t~0.25
#Remowing options that have notbeen traded for more than 3 trading days
#Remowing where the Volume is NA
#Remowing those that have strike +-50% of the underlyings price
SPX_options_25062021 <- rbind(cbind(SPX_Options$Jun.25.2021$calls,Option="call"),
                              cbind(SPX_Options$Jun.25.2021$puts,Option="put")) %>% 
  as.data.frame() %>% 
  filter(abs(as.Date(LastTradeTime) - download_date) <= 3,#Remowing options that have not been traded for more than 3 days
         !is.na(Vol),#Remowing where the Volume is NA
         Strike >= Prices26052021$Price[1]*0.75 & Strike <= Prices26052021$Price[1]*1.25)#Remowing those that have strike +-25% of the underlyings price
SPY_options_25062021 <- rbind(cbind(SPY_Options$Jun.25.2021$calls,Option="call"),
                              cbind(SPY_Options$Jun.25.2021$puts,Option="put")) %>% 
  as.data.frame()%>% 
  filter(abs(as.Date(LastTradeTime) - download_date) <= 3,
         !is.na(Vol),
         Strike >= Prices26052021$Price[2]*0.75 & Strike <= Prices26052021$Price[2]*1.25)

#Risk free rate 
risk_free_asset <- 0.0001 #13 week coupon rate

#Finding Implied Volatility1
SPX_options_25062021 <- SPX_options_25062021 %>% 
  rowwise() %>% 
  mutate(CalcImpliedVol = implied_volatility(S=pull(Prices26052021[1,2]),
                                             K=Strike,
                                             r=risk_free_asset,
                                             t=0, 
                                             endT=1/12,
                                             obsPrice=mean(Bid,Ask),
                                             option = Option))

SPY_options_25062021 <- SPY_options_25062021 %>% 
  rowwise() %>% 
  mutate(CalcImpliedVol = implied_volatility(S=pull(Prices26052021[2,2]),
                                             K=Strike,
                                             r=risk_free_asset,
                                             t=0, 
                                             endT=1/12,
                                             obsPrice=mean(Bid,Ask),
                                             option = Option))

SPX_options_25062021 %>% filter(CalcImpliedVol >= 0.01) %>%
  ggplot(aes(x = log(Prices26052021$Price[1]/Strike), y = CalcImpliedVol, color = Option, size = (Vol)))+
  ylab(expression(sigma^i))+xlab("log-Moneyness")+ggtitle("SPX")+
  geom_point(show.legend = F)+
  # geom_point(aes(y = IV, shape = Option))+ #Comparing with supplied IV (looks fine)
  geom_vline(xintercept = 0)+
  geom_smooth(se = FALSE,
              size = 0.4)+
  
  ggsave("./Output/Plots/SPX.PNG",
         width = 30,
         height = 12,
         units = "cm")


SPY_options_25062021 %>% filter(CalcImpliedVol >= 0.01) %>% 
  ggplot(aes(x = log(Prices26052021$Price[2]/Strike), y = CalcImpliedVol, color = Option, size = (Vol)))+
  xlab("log-Moneyness")+ylab(expression(sigma^i))+ggtitle("SPY")+
  geom_point(show.legend = F)+
  geom_vline(xintercept = 0)+
  geom_smooth(se = FALSE,
              size = 0.4)+
  
  ggsave("./Output/Plots/SPY.PNG",
         width = 30,
         height = 12,
         units = "cm")

#Comparing with the supplied IV's
# rbind(
#   cbind(SPX_Options$Jun.25.2021$calls, Option = "call"),
#   cbind(SPX_Options$Jun.25.2021$puts, Option = "put")
# ) %>% 
#   as.data.frame() %>%
#   filter(IV >= 0.01,
#          abs(as.Date(LastTradeTime) - download_date) <= 3,
#          !is.na(Vol),
#          Strike >= Prices26052021$Price[1]*0.75 & Strike <= Prices26052021$Price[1]*1.25) %>% 
#   ggplot(aes(x=log(Prices26052021$Price[1]/Strike), y = IV, color = Option))+
#     ylab(expression(sigma^i))+xlab("log-Moneyness")+ggtitle("SPX")+
#     geom_point()+
#     geom_vline(xintercept = 0)+
#     geom_smooth(se = FALSE,
#                 size = 0.4)+
#     
#   ggsave("./Output/Plots/SPX.PNG",
#          width = 16,
#          height = 6,
#          units = "cm")

