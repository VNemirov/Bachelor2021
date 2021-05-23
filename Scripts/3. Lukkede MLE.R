#Lukkede MLE
#All Data:
m_hat <- mean(df_sp500$log_increments)
ssquared_hat <- 1/(length(df_sp500$log_increments)-1)*sum((df_sp500$log_increments-m_hat)^2)

mu_hat <- (m_hat+ssquared_hat/2)/252
sigma_hat <- sqrt(ssquared_hat)/252


#2020 Data:
df_sp500_2020 <- filter(df_sp500, substr(date, 0, 4) == "2020")
m_hat_2020 <- mean(df_sp500_2020$log_increments)
ssquared_hat_2020 <- 1/(length(df_sp500_2020$log_increments)-1)*sum((df_sp500_2020$log_increments-m_hat)^2)

mu_hat_2020 <- (m_hat_2020+ssquared_hat_2020/2)/252
sigma_hat_2020 <- sqrt(ssquared_hat_2020)/252

data.frame(all = c(m_hat, sigma_hat),
           `2020` = c(m_hat_2020, sigma_hat_2020))*252

#Test change