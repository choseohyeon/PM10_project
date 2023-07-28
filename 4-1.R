daily <- daily_max_pm10$max_pm10
daily

f <- function(x,mu,sigma){
  return((1/sigma)*exp(-(x-mu)/sigma)*
           exp(-exp(-(x-mu)/sigma)))
}


g <- function(x,mu,sigma){
  return(-(1/n)*sum(log(f(x,mu,sigma))))
}


g_diff_mu <- function(x,mu,sigma){
  n <- length(x)
  return(-(1/n)*sum(((1/sigma) - (1/sigma) * exp(-(x-mu)/sigma))))
}
g_diff_mu(daily,1,1)


g_diff_sigma <- function(x,mu,sigma){
  n <- length(x)
  return(-(sum(((1 - exp(-((x-mu)/sigma))) * (x-mu)/sigma - 1)/sigma)/n))
}
g_diff_sigma(daily,1,1)


gradg <- function(x,mu,sigma){
  return(c(g_diff_mu(x,mu,sigma),g_diff_sigma(x,mu,sigma)))
}
gradg(data,1,1)