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

# Load Data from Github

cases <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_cases_per_million.csv")

vacc <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")

deaths <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_deaths_per_million.csv")


# Select Country to Analyse
country <- "Switzerland"
country2 <- "Switzerland" # Für United.Kingdom mit Punkt
plot_title <- "COVID Neuinfektionen und Impfungen im Vereinigten Königreich"
plot_title2 <- "COVID Todesfälle und Impfungen im Vereinigten Königreich"

# Lubridate

deaths_lub <- deaths %>%
  mutate(date=lubridate::parse_date_time(date, "ymd"))

vacc_lub <- vacc %>%
  mutate(date=lubridate::parse_date_time(date, "ymd"))

cases_lub <- cases %>%
  mutate(date=lubridate::parse_date_time(date, "ymd"))

# select countries & pivot longer corona deaths

d_s <- select(deaths_lub, date, country2)

deaths_long <-  d_s %>% 
  pivot_longer(!date, names_to = "location", values_to = "deaths_p_m")

deaths_filter <- deaths_long %>%
  arrange(desc(location)) %>%
  group_by(location) %>%
  mutate(deaths_rm = rollmean(deaths_p_m, k = 7, fill = NA)) %>%
  ungroup()

c_s <- select(cases_lub, date, country2)

cases_long <-  c_s %>% 
  pivot_longer(!date, names_to = "location", values_to = "deaths_p_m")

cases_filter <- cases_long %>%
  arrange(desc(location)) %>%
  group_by(location) %>%
  mutate(cases_rm = rollmean(deaths_p_m, k = 7, fill = NA)) %>%
  ungroup()


# Filter corona

deaths_filter <- filter(deaths_filter, date >= "2021-01-24" & date <= "2021-05-17")

cases_filter <- filter(cases_filter, date >= "2021-01-24" & date <= "2021-05-17")

# Filter corona vacc

vacc_l <- filter(vacc_lub, (location == country)
                 & date >= "2021-01-24" & date <= "2021-05-17")

# filter columns

vacc_lf <- select(vacc_l, date, location, total_vaccinations_per_hundred)

# Plot Time Series

b <- select(deaths_filter, date, deaths_rm)

c <- select(cases_filter, date, cases_rm)

d <- select(vacc_lf, date, total_vaccinations_per_hundred)

e <- merge(x = b, y = d, by = "date")

f <- merge(x = c, y = d, by = "date")

coeff <- 10

g <- ggplot(data = f, aes(x = date)) + 
  geom_line(aes(y = cases_rm, colour = "COVID Neuinfektionen: 7-day moving average"), size = 2) +
  geom_line(aes(y = total_vaccinations_per_hundred * coeff, colour = "Impfdosen: Total verabreicht"), size = 2) +
  labs(x = "Datum", title = plot_title, subtitle = "Betrachtungszeitraum 24.01.2021 bis 17.05.2021", color = "") +
  scale_color_manual("",
                     breaks = c("COVID Neuinfektionen: 7-day moving average", "Impfdosen: Total verabreicht"),
                     values = c("red", "blue")) +
  scale_y_continuous(
  name = "Neuinfektionen pro Million Einwohner",
  sec.axis = sec_axis(~./coeff, name = "Impfungen pro hundert Einwohner")) +
  theme_bw() +
  theme(
    axis.title.y = element_text(color = "red", size = 13),
    axis.title.y.right = element_text(color = "blue", size = 13),
    legend.position = c("bottom"))

plot(g)

g <- ggplot(data = e, aes(x = date)) + 
  geom_line(aes(y = deaths_rm, colour = "COVID Todesfälle: 7-day moving average"), size = 2) +
  geom_line(aes(y = total_vaccinations_per_hundred / coeff, colour = "Impfdosen: Total verabreicht"), size = 2) +
  labs(x = "Datum", title = plot_title2, subtitle = "Betrachtungszeitraum 24.01.2021 bis 17.05.2021", color = "") +
  scale_color_manual("",
                     breaks = c("COVID Todesfälle: 7-day moving average", "Impfdosen: Total verabreicht"),
                     values = c("red", "blue")) +
  scale_y_continuous(
    name = "Tägliche Todesfälle pro Million Einwohner",
    sec.axis = sec_axis(~.*coeff, name = "Impfungen pro hundert Einwohner")) +
  theme_bw() +
  theme(
    axis.title.y = element_text(color = "red", size = 13),
    axis.title.y.right = element_text(color = "blue", size = 13),
    legend.position = c("bottom"))

plot(g)