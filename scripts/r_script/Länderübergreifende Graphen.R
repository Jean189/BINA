## Libraries

# filters & arrange
library("dplyr")
library("tidyr")

# visualization

library("ggplot2")
library("ggExtra")
library("tidyverse")
library("ggpubr")
library("patchwork")
library("hrbrthemes")

# descriptive statistics

library("ggplot2")
library("car")
library("caret")

# Long data
library("reshape2")

# seven avg
library("zoo")

# --- Set standards 
# turn-off scientific notation like 1e+48
options(scipen=999) 
# pre-set the bw theme.
theme_set(theme_bw())


## Import Data

d_c <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/full_data.csv")

vacc <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")

d_m <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/total_deaths_per_million.csv")

c_m <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/total_cases_per_million.csv")

# Lubridate

d_c_lub <- d_c %>%
  mutate(date=lubridate::parse_date_time(date, "ymd"))

vacc_lub <- vacc %>%
  mutate(date=lubridate::parse_date_time(date, "ymd"))

d_m_lub <- d_m %>%
  mutate(date=lubridate::parse_date_time(date, "ymd"))

c_m_lub <- c_m %>%
  mutate(date=lubridate::parse_date_time(date, "ymd"))

# Pivot Longer

d_m_pl <-  d_m_lub %>% 
  pivot_longer(!date, names_to = "location", values_to = "deaths_p_m")

c_m_pl <-  c_m_lub %>% 
  pivot_longer(!date, names_to = "location", values_to = "cases_p_m")

# Vaccinations filter

vacc_country <- filter(vacc_lub, location == "Switzerland" | location == "European Union" | location =="United Kingdom" | location == "Turkey" | location == "Israel")

d_c_country <- filter(d_c_lub, location == "Switzerland" | location == "European Union" | location =="United Kingdom" | location == "Turkey" | location == "Israel")

d_m_country <- filter(d_m_pl, location == "Switzerland" | location == "European.Union" | location =="United.Kingdom" | location == "Turkey" | location == "Israel")

c_m_country <- filter(c_m_pl, location == "Switzerland" | location == "European.Union" | location =="United.Kingdom" | location == "Turkey" | location == "Israel")

# 7-day moving average

d_c_rollmean <- d_c_country %>%
  arrange(desc(location)) %>%
  group_by(location) %>%
  mutate(deaths_rm = rollmean(new_deaths, k = 7, fill = NA)) %>%
  mutate(cases_rm = rollmean(new_cases, k = 7, fill = NA)) %>%
  ungroup()


d_m_rollmean <- d_m_country %>%
  arrange(desc(location)) %>%
  group_by(location) %>%
  mutate(deaths_rm = rollmean(deaths_p_m, k = 7, fill = NA)) %>%
  ungroup()


c_m_rollmean <- c_m_country %>%
  arrange(desc(location)) %>%
  group_by(location) %>%
  mutate(cases_rm = rollmean(cases_p_m, k = 7, fill = NA)) %>%
  ungroup()

vacc_rollmean <- vacc_country %>%
  arrange(desc(location)) %>%
  group_by(location) %>%
  mutate(vacc_t_rm = rollmean(daily_vaccinations, k = 7, fill = NA)) %>%
  mutate(vacc_rm = rollmean(daily_vaccinations_per_million, k = 7, fill = NA)) %>%
  ungroup()


# filter columns

vacc_f <- select(vacc_country, date, location, total_vaccinations_per_hundred)

vacc_f2 <- select(vacc_country, date, location, total_vaccinations)

vacc_f3 <- select(vacc_country, date, location, daily_vaccinations_per_million)

vacc_f4 <- select(vacc_rollmean, date, location, vacc_rm)

vacc_f5 <- select(vacc_rollmean, date, location, vacc_t_rm)

cases_f <- select(d_c_rollmean, date, location, total_cases, cases_rm)

deaths_f <- select(d_c_rollmean, date, location, total_deaths, deaths_rm)




# ggplot vacc

g <- ggplot(data = vacc_f, aes(x = date, y = total_vaccinations_per_hundred, col = location)) +
  geom_line() + 
  labs(x = "Datum", y = "Total verabreichte Impfdosen", title = "Total verabreichte Impfdosen pro hundert Einwohner - EU, CH, GBR, ISR and TUR", col = "Länder / Regionen") +
  scale_y_continuous(labels = function(x) format(x, big.mark = "'", decimal.mark = ",", scientific = FALSE))

plot(g)

g <- ggplot(data = vacc_f2, aes(x = date, y = total_vaccinations, col = location)) +
  geom_line() + 
  labs(x = "Datum", y = "Total verabreichte Impfdosen", title = "Total verabreichte Impfdosen - EU, CH, GBR, ISR and TUR", col = "Länder / Regionen") +
  scale_y_continuous(labels = function(x) format(x, big.mark = "'", decimal.mark = ",", scientific = FALSE))

plot(g)

g <- ggplot(data = vacc_f3, aes(x = date, y = daily_vaccinations_per_million, col = location)) +
  geom_line() + 
  labs(x = "Datum", y = "Täglich verabreichte Impfdosen pro Million Einwohner", title = "Täglich verabreichte Impfdosen pro Million Einwohner - EU, CH, GBR, ISR and TUR", col = "Länder / Regionen") +
  scale_y_continuous(labels = function(x) format(x, big.mark = "'", decimal.mark = ",", scientific = FALSE))

plot(g)

g <- ggplot(data = vacc_f4, aes(x = date, y = vacc_rm, col = location)) +
  geom_line() + 
  labs(x = "Datum", y = "Täglich verabreichte Impfdosen pro Million Einwohner", title = "Täglich verabreichte Impfdosen pro Million Einwohner - EU, CH, GBR, ISR and TUR", subtitle = "7-day moving average", col = "Länder / Regionen") +
  scale_y_continuous(labels = function(x) format(x, big.mark = "'", decimal.mark = ",", scientific = FALSE))

plot(g)

g <- ggplot(data = vacc_f5, aes(x = date, y = vacc_t_rm, col = location)) +
  geom_line() + 
  labs(x = "Datum", y = "Täglich verabreichte Impfdosen", title = "Täglich verabreichte Impfdosen - EU, CH, GBR, ISR and TUR", subtitle = "7-day moving average", col = "Länder / Regionen") +
  scale_y_continuous(labels = function(x) format(x, big.mark = "'", decimal.mark = ",", scientific = FALSE))

plot(g)

# ggplot cases

g <- ggplot(data = cases_f, aes(x = date, y = cases_rm, col = location)) +
  geom_line() +
  labs(x = "Datum", y = "Tägliche Neuinfektionen", title = "Tägliche COVID Neuinfektionen - EU, CH, GBR, ISR and TUR",
       subtitle = "7-day moving average", col = "Länder / Regionen") +
  scale_y_continuous(labels = function(x) format(x, big.mark = "'", decimal.mark = ",", scientific = FALSE))

plot(g)


g <- ggplot(data = cases_f, aes(x = date, y = total_cases, col = location)) +
  geom_line() +
  labs(x = "Datum", y = "Total Infektionen", title = "Kumulative COVID Infektionen - EU, CH, GBR, ISR and TUR", col = "Länder / Regionen") +
  scale_y_continuous(labels = function(x) format(x, big.mark = "'", decimal.mark = ",", scientific = FALSE))

plot(g)


g <- ggplot(data = c_m_rollmean, aes(x = date, y = cases_rm, col = location)) +
  geom_line() +
  labs(x = "Datum", y = "Tägliche Neuinfektionen pro Million Einwohner", 
       title = "Tägliche COVID Neuinfektionen pro Million Einwohner - EU, CH, GBR, ISR and TUR",
       subtitle = "7-day moving average", col = "Länder / Regionen") +
  scale_y_continuous(labels = function(x) format(x, big.mark = "'", decimal.mark = ",", scientific = FALSE))

plot(g)


# ggplot deaths

g <- ggplot(data = deaths_f, aes(x = date, y = deaths_rm, col = location)) +
  geom_line() +
  labs(x = "Datum", y = "Tägliche Todesfälle", title = "Tägliche COVID Todesfälle - EU, CH, GBR, ISR and TUR",
       subtitle = "7-day moving average", col = "Länder / Regionen") +
  scale_y_continuous(labels = function(x) format(x, big.mark = "'", decimal.mark = ",", scientific = FALSE))

plot(g)


g <- ggplot(data = deaths_f, aes(x = date, y = total_deaths, col = location)) +
  geom_line() +
  labs(x = "Datum", y = "Total Totesfälle", title = "Kumulative COVID Todesfälle - EU, CH, GBR, ISR and TUR", col = "Länder / Regionen") +
  scale_y_continuous(labels = function(x) format(x, big.mark = "'", decimal.mark = ",", scientific = FALSE))

plot(g)


g <- ggplot(data = d_m_rollmean, aes(x = date, y = deaths_rm, col = location)) +
  geom_line() +
  labs(x = "Datum", y = "Tägliche Todesfälle pro Million Einwohner", 
       title = "Tägliche COVID Todesfälle pro Million Einwohner - EU, CH, GBR, ISR and TUR",
       subtitle = "7-day moving average", col = "Länder / Regionen") +
  scale_y_continuous(labels = function(x) format(x, big.mark = "'", decimal.mark = ",", scientific = FALSE))

plot(g)

