library(tidyverse)
library(tidyquant)
library(lubridate)
library(dplyr)
#Problem 1
# 1.1 SMA
data <- c(0, 2, 4, 6, 7, 3, 2, 9, 17, 14, 15, 0, 4, 3)
for (i in 1:length(data)){
  d <- c(d, mean(data[i:(i+3)]))}
d <- ifelse(is.na(d), NA, sss)
# 1.2 cor
m <- c(41, 19, 23, 40, 55, 57, 33)
n <- c(94, 60, 74, 71, 82, 76, 61)
k <- (mean(sum(m-mean(m))*sum(n-mean(n))))/(sd(m)*sd(n))
l <- cor(m, n)

#Problem 2
x = (1:100)
prime_numbers=c()
for (i in 2:100) {
  if (any(x == i)) {
    prime_numbers = c(prime_numbers, i)
    x = c(x[(x %% i) != 0], i)
  }
}
print(prime_numbers)
# I looked for this one in the interned. I got the logic behind it,
# but I couldn't understand what is exactly happening in x = c(x[(x %% i) != 0], i)
# I get the idea, but i don't understand the code itself here

#Problem 3
library(tidyquant)
#1-5
data <- tidyquant::tq_get("FB",
                          get = "stock.prices",
                          from = "2019-01-01",
                          to   = "2021-04-01")%>%
  dplyr::select(symbol, date, adjusted)

Dates <- data.frame(Date = rep(seq.Date(from = ymd('2019-01-01'), 
                                        to = ymd('2021-04-01'), 
                                        by = 'days'), 3),
                    Symbol = rep("FB", 822))

Join <- Dates %>%
  dplyr::left_join(data, by = c("Date" = "date", "Symbol" = "symbol")) %>%
  group_by(Symbol) %>%
  fill(adjusted, .direction = "downup")

JoinNew <- Join %>%
  mutate(EMA26 = EMA(adjusted, n = 26),
         EMA12 = EMA(adjusted, n = 12),
         MACD = EMA12 - EMA26,
         LagMACD = lag(MACD),
         SignalLine = EMA(MACD, 9),
         LagSignalLine = lag(SignalLine)) %>%
  ungroup() %>%
  filter(!is.na(LagSignalLine)) %>%
  mutate(Crossed = case_when(LagMACD > LagSignalLine & MACD < SignalLine ~ "crossed from above",
                             LagMACD < LagSignalLine & MACD > SignalLine ~ "crossed from below",
                             TRUE ~ "no cross"),
         Signal = case_when(Crossed == "crossed from above" ~ "sell",
                            Crossed == "crossed from below" ~ "buy",
                            TRUE ~ "do nothing"),
         signal = case_when(Signal == "sell" ~ "do not have stocks",
                            Signal == "buy" ~ "have stocks",
                            TRUE ~ "it depends"))
#6
MM <- JoinNew %>%
  mutate(BenchmarkMoney = 100,
         sss = adjusted/lag(adjusted),
         sss = ifelse(is.na(sss), 1, sss),
         BenchmarkMoney1 = cumprod(sss),
         StrategyMoney = 100,
         sss1 = case_when(signal == "do not have stocks" ~ 1,
                          signal == "have stocks" ~ sss,
                          TRUE ~ 999999999),
         StrategyMoney1 = cumprod(sss1))