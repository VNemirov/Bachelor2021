#Defining a function for a call/put option (with option to add custom price):
put <- function(S, K, premium = 0){max(K-S, 0)-premium}