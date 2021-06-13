#Lukkede MLE
#All Data (S&P500):
m_hat <- mean(df_sp500$log_increments)
ssquared_hat <- 1/(length(df_sp500$log_increments)-1)*sum((df_sp500$log_increments-m_hat)^2)

mu_hat <- (m_hat+ssquared_hat/2)/252
sigma_hat <- sqrt(ssquared_hat)/252


#2020 Data:
df_sp500_2020 <- filter(df_sp500, substr(date, 0, 4) == "2020")
m_2020 <- mean(df_sp500_2020$log_increments)
v_2020 <- sqrt(1/(nrow(df_sp500_2020)-1)*sum((df_sp500_2020$log_increments-m_2020)^2))
mu_hat_2020 <- (m_2020+v_2020^2/2)*nrow(df_sp500_2020)
s_hat_2020 <- v_2020/sqrt(1/nrow(df_sp500_2020))

#Test change
m <- mean(df_sp500$log_increments)
v <- sqrt(1/(nrow(df_sp500)-1)*sum((df_sp500$log_increments-m)^2))
mu_hat <- (m+v^2/2)*nrow(df_sp500)
s_hat <- v/sqrt(1/nrow(df_sp500))


MLE_dataSP500 <- data.frame(
  est = c("mu", "sigma"),
  `2020` = c(mu_hat_2020, s_hat_2020),
  `Al data` = c(mu_hat/(11+1/12), s_hat/sqrt(11+1/12))
)

#Russel
m_hat <- mean(df_rus2000$log_increments)
ssquared_hat <- 1/(length(df_rus2000$log_increments)-1)*sum((df_rus2000$log_increments-m_hat)^2)

mu_hat <- (m_hat+ssquared_hat/2)/252
sigma_hat <- sqrt(ssquared_hat)/252


#2020 Data:
df_rus2000_2020 <- filter(df_rus2000, substr(date, 0, 4) == "2020")
m_2020 <- mean(df_rus2000_2020$log_increments)
v_2020 <- sqrt(1/(nrow(df_rus2000_2020)-1)*sum((df_rus2000_2020$log_increments-m_2020)^2))
mu_hat_2020 <- (m_2020+v_2020^2/2)*nrow(df_rus2000_2020)
s_hat_2020 <- v_2020/sqrt(1/nrow(df_rus2000_2020))

#Test change
m <- mean(df_rus2000$log_increments)
v <- sqrt(1/(nrow(df_rus2000)-1)*sum((df_rus2000$log_increments-m)^2))
mu_hat <- (m+v^2/2)*nrow(df_rus2000)
s_hat <- v/sqrt(1/nrow(df_rus2000))


MLE_dataRUS2000 <- data.frame(
  est = c("mu", "sigma"),
  `2020` = c(mu_hat_2020, s_hat_2020),
  `Al data` = c(mu_hat/(11+1/12), s_hat/sqrt(11+1/12))
)
