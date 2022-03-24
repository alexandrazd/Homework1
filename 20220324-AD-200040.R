#Problem 1#
i <- 0
sum <- 0
for(i in 1:1000) {
  currAmount <- 100
  currBet <- 1
  numGames <- 0
  
  while(currAmount > 0) {
    if (currAmount < currBet) currBet <- currAmount
    
    if (sample (c(0,1), 1, prob = c(0.486, 0.514)) == 0){
      currAmount <- currAmount + currBet
    }
    else{ 
      currAmount <- currAmount - currBet
      currBet <- currBet*2
    }
    
    numGames <- numGames + 1
  }
  
  sum <- sum + numGames
}

print(sum / 1000)


#Problem 5.2.4
setwd("D:\\My Documents\\Desktop\\uni\\quantitative methods")
flights = read.csv("nycflights.csv", sep = ",", dec = ".",na.strings = c("NA","",""," "))
view(flights)
filter(flights, arr_delay >= 120)
filter(flights, dest == "IAH" | dest == "HOU")
filter(flights, dest %in% c("IAH", "HOU"))
filter(flights, carrier %in% c("AA", "DL", "UA"))
filter(flights, month >= 7, month <= 9)
filter(flights, month %in% 7:9)
filter(flights, arr_delay > 120, dep_delay <= 0)
filter(flights, dep_delay >= 60, dep_delay - arr_delay > 30)
summary(flights$dep_time)
filter(flights, dep_time <= 600 | dep_time == 2400)

#Problem 5.3.1
arrange(flights, dep_time) %>%
  tail()
arrange(flights, desc(dep_time))
arrange(flights, desc(is.na(dep_time)), dep_time)

#Problem 5.4.1
select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, "dep_time", "dep_delay", "arr_time", "arr_delay")
select(flights, 4, 6, 7, 9)
select(flights, all_of(c("dep_time", "dep_delay", "arr_time", "arr_delay")))
variables <- c("dep_time", "dep_delay", "arr_time", "arr_delay")
select(flights, all_of(variables))
select(flights, starts_with("dep_"), starts_with("arr_"))
select(flights, matches("^(dep|arr)_(time|delay)$"))
variables <- c("dep_time", "dep_delay", "arr_time", "arr_delay")
select(flights, !!variables)
variables <- syms(c("dep_time", "dep_delay", "arr_time", "arr_delay"))
select(flights, !!!variables)

#Problem 5.5.2
flights_airtime <-
  mutate(flights,
         dep_time = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
         arr_time = (arr_time %/% 100 * 60 + arr_time %% 100) %% 1440,
         air_time_diff = air_time - arr_time + dep_time
  )
nrow(filter(flights_airtime, air_time_diff != 0))
ggplot(flights_airtime, aes(x = air_time_diff)) +
  geom_histogram(binwidth = 1)
ggplot(filter(flights_airtime, dest == "LAX"), aes(x = air_time_diff)) +
  geom_histogram(binwidth = 1)
