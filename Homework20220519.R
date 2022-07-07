library(tidyverse)
library(tidyquant)
library(lubridate)
library(dplyr)

#Problem 1#
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
  mutate(SMA = SMA(adjusted, n = 20),
         SD20 = RcppRoll: roll_sd(adjusted, n=20, alight= "right"),
         Upper.bond = SMA+2*SD20,
         Lower.bond = SMA-2*SD20)) %>%
  ungroup() %>%
  mutate(Signal = case_when(adjusted > Upper.bond & lag(adjusted) < lag(Upper.bond) ~ "sell",
                            adjusted < Lower.bond & lag(adjusted) > lag(Lower.bond)~ "buy",
                            TRUE ~ "hold"))
#Problem 1#

#Problem 2#
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

JoinNew2 <- Join %>%
  mutate(RSI = RSI(adjusted, n = 20)) %>%
  ungroup() %>%
  mutate(Signal = case_when(RSI > 65 ~ "sell",
                            RSI < 35 ~ "buy",
                            TRUE ~ "hold"))