# Assignment 3: Incarceration

library(tidyverse)
library(ggplot2)
library(usmap)

incarceration = read.csv(file = "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Summary functions

# 1. Which is the ratio of black vs white people in prison?
black_jail_pop <- incarceration %>%
  summarise(num = sum(black_prison_pop, na.rm = TRUE)) %>%
  pull(num)

white_jail_pop <- incarceration %>%
  summarise(num = sum(white_prison_pop, na.rm = TRUE)) %>%
  pull(num)

black_white_ratio <- black_jail_pop/white_jail_pop
  #1.22

# 2. Which state has highest ratio of jail population/total population?
highest_incarceration_rate_state <- incarceration %>%
  group_by(state) %>%
  summarise(pop_total = sum(total_pop, na.rm = TRUE), jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
  mutate(ratio = jail_pop/pop_total) %>% 
  arrange(desc(ratio)) %>%
  filter(row_number() == 1) %>%
  pull(state)
  #Louisiana has highest ratio
            
# 3. Wich year had the highest jail population in the nation?
highest_year <- incarceration %>%
  group_by(year) %>%
  summarise(pop = sum(total_pop, na.rm = TRUE)) %>%
  arrange(desc(pop)) %>%
  filter(row_number() == 1) %>%
  pull(year)
  # 2018

# 4. What is the ratio of female to male jail population nationally?
male_jail_pop <- incarceration %>%
  summarise(num = sum(male_jail_pop, na.rm = TRUE)) %>%
  pull(num)
  
female_jail_pop <- incarceration %>%
  summarise(num = sum(female_jail_pop, na.rm = TRUE)) %>%
  pull(num)

gender_ratio <- female_jail_pop/male_jail_pop
  #0.126 

# 5. Which state has highest juvenile incarceration?
highest_juvie_state <- incarceration %>%
  group_by(state) %>%
  summarise(female = sum(female_juvenile_jail_pop, na.rm = TRUE), male = sum(male_juvenile_jail_pop, na.rm = TRUE)) %>%
  mutate(total = female + male) %>% 
  arrange(desc(total)) %>%
  filter(row_number() == 1) %>%
  pull(state)
  #New York 


# Chart1 - Trends
plot_data <- incarceration %>%
  group_by(year) %>%
  filter(year > 1980, year < 2015) %>%
  summarise(AAPI = sum(aapi_prison_pop, na.rm = TRUE),
            Black = sum(black_prison_pop, na.rm = TRUE),
            Latinx = sum(latinx_prison_pop, na.rm = TRUE),
            Native = sum(native_prison_pop, na.rm = TRUE),
            White = sum(white_prison_pop, na.rm = TRUE)) %>%
  select(year, AAPI, Black, Latinx, Native, White)

plot_data <- gather(plot_data,
              key = "Race",
              value = "Count", AAPI, Black, Latinx, Native, White)

chart1 <- ggplot(plot_data, aes(x = year, y = Count, color = Race)) + 
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Total Population in Prison by Race") +
  labs(x="Year", y="Count") 

# Chart2 - Compare
pop <- incarceration %>%
  group_by(year) %>%
  summarise(female = sum(female_jail_pop, na.rm = TRUE),
            male = sum(male_jail_pop, na.rm = TRUE)) %>%
  filter(year >= 2000) %>%
  select(year, female, male)

pop <- gather(pop,
             key = "Gender",
             value = "Count", male, female)

chart2 <- ggplot(pop, aes(x = year, y = Count, color = Gender)) +
  geom_line(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Total Population in Jail by Gender") +
  labs(x="Year", y="Count")


# Map
pop_2018 <- incarceration %>%
  group_by(state) %>%
  summarise(pop = sum(total_jail_pop, na.rm = TRUE)) %>%
  mutate(pop)
  
map <- plot_usmap(data = pop_2018, values = "pop", color = "black") + 
  scale_fill_continuous(name = "Jail Population (2018)", label = scales::comma,
                        low = "steelblue1", high = "royalblue4") + 
  labs(title = "Jail Population by State in 2018") +
  theme(legend.position = "right")

