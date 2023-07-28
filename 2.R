library(tidyverse)
pm10 <- data.frame()
for (year in 2009:2022) {
  file_path <- paste0("C:/Users/Samsung/Desktop/PM10/",year,".csv")
  data <- read.csv(file_path, fileEncoding='euc-kr')
  data <- data[,3:4]
  pm10 <- rbind(pm10, data)
}
pm10

df <- pm10 %>%
  separate(일시, c("Year", "Month", "Time"), "-") %>%
  separate(Time,c("Date","Time")," ") %>%
  filter(Month %in% c('03','04','05')) %>%
  unite(일시,Year,Month,Date,sep="-") %>%
  filter(!is.na(X1시간평균.미세먼지농도.....))
df

daily_max_pm10 <- df %>%
  mutate(일시=as.Date(일시),
         X1시간평균.미세먼지농도.....=as.numeric(X1시간평균.미세먼지농도.....)) %>%
  group_by(일시) %>%
  summarise(max_pm10=max(X1시간평균.미세먼지농도.....,na.rm=TRUE))
daily_max_pm10

daily_max_pm10 %>% summary(daily_max_pm10)
daily_max_pm10 %>%
  summarise(mean_max=mean(max_pm10),
            var_max=var(max_pm10),
            sd_max=sd(max_pm10))
hist(daily_max_pm10$max_pm10)
boxplot(daily_max_pm10$max_pm10)