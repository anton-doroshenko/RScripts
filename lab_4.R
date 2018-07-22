#install.packages("lubridate", dependencies=TRUE, repos='http://cran.rstudio.com/')
library(lubridate)
library(dplyr)
library(ggplot2)

setwd("/media/anton/Data/R")

crime <- read.csv("crimes.csv", header = TRUE)
str(crime)

crime$POSIX <- ymd_hms(as.character(crime$Dates))
crime$Dates <- as.Date(ymd_hms(as.character(crime$Dates)))

moon <- read.csv("moon.csv", header = TRUE)
str(moon)
moon$date <- as.Date(moon$date, "%m/%d/%Y")

full_data <- inner_join(crime, moon, by = c("Dates" = "date"))

date_phase <- full_data %>%
  group_by(Dates, phase) %>%
  count() %>%
  arrange(desc(n))

glimpse(date_phase)

ggplot(date_phase, aes(Dates, n)) +
  geom_line(alpha = 0.5) +
  labs(title = "Злочини в Сан-Франциско (2003-2015)",
       x = "Дата",
       y = "Кількість злочинів") +
  geom_point(data = date_phase[date_phase$phase == "Full Moon", ], color = "red") +
  geom_smooth()

x <- mean(date_phase$n[date_phase$phase == "Full Moon"])
x

mu <- mean(date_phase$n[date_phase$phase != "Full Moon"])
mu

n <- length(date_phase$n[date_phase$phase == "Full Moon"])
n
s <- sd(date_phase$n[date_phase$phase == "Full Moon"])
s

p_value <- 2*pt(0.839, df=76, lower.tail = FALSE)
p_value

x_vector <- date_phase$n[date_phase$phase == "Full Moon"]

t.test(x_vector, mu = 391.75, alternative = "two.sided", conf.level = 0.95)

day_of_week_crimes <- full_data %>%
  group_by(DayOfWeek) %>%
  count()

glimpse(day_of_week_crimes)

day_of_week_crimes$DayOfWeek <- factor(day_of_week_crimes$DayOfWeek,
                                       levels = c("Monday", "Tuesday",
                                                  "Wednesday", "Thursday",
                                                  "Friday", "Saturday", "Sunday"))

ggplot(data = day_of_week_crimes, aes(x = DayOfWeek, y = n)) +
  geom_bar(stat = "identity", fill = "lightblue")

crimes_by_day <- full_data %>%
  group_by(Dates, DayOfWeek) %>%
  count()

sample_vector <- crimes_by_day$n[crimes_by_day$DayOfWeek == "Friday"]

str(sample_vector)
length(sample_vector)

x <- mean(crimes_by_day$n[crimes_by_day$DayOfWeek == "Friday"])
x

x1 <- mean(crimes_by_day$n[crimes_by_day$DayOfWeek != "Friday"])
x1
s <- sd(crimes_by_day$n[crimes_by_day$DayOfWeek == "Friday"])
s

t.test(sample_vector, mu = 391.75, alternative = "two.sided", conf.level = 0.99)

p_val <- 2*pt(4.4319, df = 45, lower.tail = FALSE)
p_val
