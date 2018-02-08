library(tidyverse)
library(lubridate)
library(nycflights13)

today()
now()

ymd("2017-01-31")
mdy("January 31st, 2017")
mdy("01-31-2017")
dmy("31-Jan-2017")
ymd(20170131)

ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")
ymd(20170131, tz = "UTC")
flights %>% 
  select(year, month, day, hour, minute)

flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute)
         )

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}
flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(dep_time = make_datetime_100(year, month, day, dep_time),
         arr_time = make_datetime_100(year, month, day, arr_time),
         sched_dep_time = make_datetime_100(
           year, month, day, sched_dep_time
         ),
         sched_arr_time = make_datetime_100(
           year, month, day, sched_arr_time
         )
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))
         
flights_dt %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 86400)

flights_dt %>% 
  filter(dep_time < ymd(20130102)) %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 600)

as_datetime(60*60*10)  
as_date(365*10+2)


# Exercises p. 243
ymd(c("2010-10-10", "bananas"))


datetime <- ymd_hms("2016-07-08 12:34:56")

year(datetime)
month(datetime)
mday(datetime)
yday(datetime)
wday(datetime)

month(datetime, label = T, locale = "US")
wday(datetime, label = T, abbr = F, locale = "US")

flights_dt %>% 
  mutate(wday = wday(dep_time, label = T)) %>% 
  ggplot(aes(x = wday)) +
  geom_bar()

dep_time <- flights_dt %>% 
  mutate(minute = minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarize(
    avg_delay = mean(arr_delay, na.rm = T),
    n = n()) 

dep_time %>% 
  ggplot(aes(minute, avg_delay)) +
    geom_line()

sched_dep <- flights_dt %>% 
  mutate(minute = minute(sched_dep_time)) %>% 
  group_by(minute) %>% 
  summarize(
    avg_delay = mean(arr_delay, na.rm = T),
    n = n()
  )

ggplot(sched_dep, aes(minute, avg_delay)) +
  geom_line()

ggplot(sched_dep, aes(minute, n)) +
  geom_line()

flights_dt %>% 
  count(week = floor_date(dep_time, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line()
  
(datetime <- ymd_hms("2016-07-08 12:34:56"))
year(datetime) <- 2020
datetime  

month(datetime) <- 01
datetime

hour(datetime) <- hour(datetime) + 1

update(datetime, year = 2020, month = 2, mday = 2, hour = 2)
ymd("2015-02-01") %>% 
  update(mday = 30)

ymd("2015-02-01") %>% 
  update(hour = 400)

flights_dt %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>% 
  ggplot(aes(dep_hour)) +
  geom_freqpoly(binwidth = 300)


o_age <- today() - ymd(19800618)
o_age

as.duration(o_age)

dseconds(15)
dminutes(10)
dhours(c(12,24))
ddays(0:5)
dweeks(3)
dyears(1)

2 * dyears(1)
dyears(1) + dweeks(12) + dhours(15)
tomorrow <- today() + ddays(1)
last_year <- today() - dyears(1)         

one_pm <- ymd_hms("2016-03-12 13:00:00", tz = 'America/Denver')
one_pm
one_pm + ddays(1)







