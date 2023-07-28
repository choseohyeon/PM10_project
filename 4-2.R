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

g_diff_sigma <- function(x,mu,sigma){
  n <- length(x)
  return(-(sum(((1 - exp(-((x-mu)/sigma))) * (x-mu)/sigma - 1)/sigma)/n))
}

gradg <- function(x,mu,sigma){
  return(c(g_diff_mu(x,mu,sigma),g_diff_sigma(x,mu,sigma)))
}

mu <- sigma <- Inf
mu.new <- mu_hat; sigma.new <- sigma_hat
n <- length(daily)
while((abs(mu.new-mu)+abs(sigma.new-sigma))>=1e-05){
  eta <- 1
  mu <- mu.new;sigma <- sigma.new
  new <- c(mu,sigma)-eta*gradg(daily,mu,sigma)
  mu.new <- new[1];sigma.new <- new[2]
  
  while(g(daily,mu.new,sigma.new)>g(daily,mu,sigma)){
    eta <- eta*0.5
    new <- c(mu,sigma)-eta*gradg(daily,mu,sigma)
    mu.new <- new[1];sigma.new <- new[2]
  }
}
c(mu.new,sigma.new)

