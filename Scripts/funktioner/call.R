#Defining a function for a call/put option (with option to add custom price):
call <- function(S, K, premium = 0){max(S-K, 0)-premium}