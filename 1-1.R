install.packages("tidyverse")

library(tidyverse)
pm10 <- data.frame(NULL)

for (year in 2009:2022) {
  file_path <- paste0("C:/Users/Samsung/Desktop/PM10/",year,".csv")
  data <- read.csv(file_path, fileEncoding='euc-kr')
  data <- data[,3:4]
  pm10 <- rbind(pm10, data)
}

pm10