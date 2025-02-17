library(tidyverse)
library(tidyquant)
library(lubridate)
library(dplyr)

data <- tidyquant::tq_get(c("FB", "AMZN", "NFLX"),
                          get = "stock.prices",
                          from = "2019-01-01",
                          to   = "2021-04-01")%>%
  dplyr::select(symbol, date, adjusted)

Dates <- data.frame(Date = rep(seq.Date(from = ymd('2019-01-01'), 
                                        to = ymd('2021-04-01'), 
                                        by = 'days'), 3),
                    Symbol = c(rep("AMZN", 822),rep("FB", 822),rep("NFLX", 822)))

Join <- Dates %>%
  dplyr::left_join(data, by = c("Date" = "date", "Symbol" = "symbol")) %>%
  group_by(Symbol) %>%
  fill(adjusted, .direction = "downup")

DataFrame <- Join %>%
  filter(Symbol %in% c("AMZN", "FB"),
         between(Date, ymd("2019-01-01"), ymd("2019-07-01")) | 
           between(Date, ymd("2020-04-01"), ymd("2020-07-01"))) %>%
  dplyr::arrange(Symbol, desc(Date))

a <- DataFrame %>%
  filter((Date == min(ymd(Date)) | Date == max(ymd(Date))) &
           (Symbol == "AMZN" | Symbol == "FB"))

b <- NULL
b <- DataFrame %>%
  mutate(year = substring(Date, 1, 4),
         month = substring(Date, 6, 7),
         day = substring(Date, 9, 10)) %>%
  group_by(year, month) %>%
  filter(day == max(day)) %>%
  arrange(year, month)
view(b)

#Problem 2
Crossing <- DataFrame %>%
  mutate(SMA10 = SMA(adjusted, n = 10),
         SMA26 = SMA(adjusted, n = 26),
         LagSMA10 = lag(SMA10),
         LagSMA26 = lag(SMA26)) %>%
  filter(!is.na(LagSMA26)) %>%
  mutate(Crossed = case_when(LagSMA10 > LagSMA26 & SMA10 < SMA26 ~ "crossed from below",
                             LagSMA10 < LagSMA26 & SMA10 > SMA26 ~ "crossed from above",
                             TRUE ~ "no cross"),
         Signal = case_when(Crossed == "crossed from below" ~ "sell",
                            Crossed == "crossed from above" ~ "buy",
                            TRUE ~ "do nothing"))

sum(Crossing$Crossed == "crossed from below")

sum(Crossing$Crossed == "crossed from above")