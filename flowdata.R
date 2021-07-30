#Organize X2 adn Outflwo data

library(tidyverse)
library(readxl)
library(lubridate)
library(smonitr)

#X2 data from Hutton et al. for old stuff, Dayflow for more recent stuff

X2 = read_excel("X2.xlsx")

#subset just Sep-Nov and take the average

fallX2 = mutate(X2, Month = month(Date), Year = year(Date)) %>%
  filter(Month %in% c(9,10,11)) %>%
  group_by(Year) %>%
  summarize(FallX2 = mean(X2))


Dayflow = get_odp_data(pkg_id = "dayflow", fnames = "Dayflow Results")

DF1997_2020 =  Dayflow$`Dayflow Results 1997 - 2020` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y"), Month = month(Date), Year = year(Date)) %>%
  select(Date, OUT, X2, Month, Year)

DFX2fall = filter(DF1997_2020, Year >2012, Month %in% c(9,10,11)) %>%
  group_by(Year) %>%
  summarize(FallX2 = mean(X2))

X2fall = rbind(fallX2, DFX2fall)

#now do it for spring

spX2 = mutate(X2, Month = month(Date), Year = year(Date)) %>%
  filter(Month %in% c(3,4,5)) %>%
  group_by(Year) %>%
  summarize(SpX2 = mean(X2))


DFX2sp = filter(DF1997_2020, Year >2012, Month %in% c(3,4,5)) %>%
  group_by(Year) %>%
  summarize(SpX2 = mean(X2, na.rm = T))

X2sp = rbind(spX2, DFX2sp)

########################################################################
#and now for outflow

DF1956_1969 = Dayflow$`Dayflow Results 1956 - 1969` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  select(Date, OUT)

DF1970_1983 = Dayflow$`Dayflow Results 1970 - 1983` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  select(Date, OUT)

DF1984_1996 = Dayflow$`Dayflow Results 1984 - 1996` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  select(Date, OUT)

DF1997_2020 =  Dayflow$`Dayflow Results 1997 - 2020` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  select(Date, OUT)

DF = bind_rows(DF1956_1969, DF1970_1983, DF1984_1996, DF1997_2020)

DFspring = mutate(DF, Year = year(Date), Month = month(Date)) %>%
  filter(Month %in% c(3,4,5)) %>%
  group_by(Year) %>%
  summarize(SpNDOI = mean(OUT))

DFfall = mutate(DF, Year = year(Date), Month = month(Date)) %>%
  filter(Month %in% c(9,10,11)) %>%
  group_by(Year) %>%
  summarize(FALLNDOI = mean(OUT))

flowdata = left_join(DFspring, DFfall) %>%
  left_join(X2sp) %>%
  left_join(X2fall)
