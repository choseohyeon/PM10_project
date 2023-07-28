daily <- daily_max_pm10$max_pm10
daily

f <- function(x,mu,sigma){
  return((1/sigma)*exp(-(x-mu)/sigma)*
           exp(-exp(-(x-mu)/sigma)))
}

hist(daily_max_pm10$max_pm10,prob=TRUE)

# 그래프 그리기
x <- seq(0,800,by=1.)
y1 <- f(x,mu_hat,sigma_hat)  # 확률밀도함수 계산
y2 <- f(x,mu.new,sigma.new)
lines(x, y1, type="l", col="blue", xlab="x", ylab="Density", main="Probability Density Function")
lines(x, y2, type="l", col="red", xlab="x", ylab="Density", main="Probability Density Function")
