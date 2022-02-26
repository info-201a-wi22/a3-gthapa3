library(ggplot2)
library(tidyverse)

incarceration = read.csv(file = "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

plot_data <- incarceration %>%
  group_by(year) %>%
  filter(year > 1980) %>%
  summarise(aapi = sum(aapi_jail_pop, na.rm = TRUE),
            black = sum(black_jail_pop, na.rm = TRUE),
            latinx = sum(latinx_jail_pop, na.rm = TRUE),
            native = sum(native_jail_pop, na.rm = TRUE),
            white = sum(white_jail_pop, na.rm = TRUE)) %>%
  select(year, aapi, black, latinx, white)

chart <- ggplot(plot_data, aes(x = year)) + 
  geom_line(aes(y = aapi), color = "darkred", size = 1) +
  geom_line(aes(y = black), color = "darkgreen", size = 1) +
  geom_line(aes(y = latinx), color = "orange", size = 1) +
  geom_line(aes(y = white), color = "blue", size = 1) +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Total Numbers in Jail by Race") +
  ylab("Count") +
  xlab("Year") 
