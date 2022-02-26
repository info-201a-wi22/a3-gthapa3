# Assignment 3: Incarceration

library(tidyverse)

incarceration = read.csv(file = "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Summary functions

# 1. Which state has highest jail population?
highest_incarceration_num_state <- incarceration %>%
  group_by(state) %>%
  summarise(total = sum(total_jail_pop, na.rm = TRUE)) %>%
  arrange(desc(total)) %>%
  filter(row_number() == 1) %>%
  pull(state)
  #California has highest jail population

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
