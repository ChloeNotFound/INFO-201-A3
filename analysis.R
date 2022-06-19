#Assignment 3 -- Chloe Gao

#Load the file.
df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv")
library(tidyverse)

#load the packages
library(dplyr)
library(ggplot2)
library(scales)

#discover the year range
max_year <- max(df$year)
min_year <- min(df$year)

#discover the observations
obs <- nrow(df)
print(obs)

#calculate the total population in jail in 1990
jail_pop_1990 <- df %>% 
  group_by(year = 1990)

#calculate the black population in jail in 1990
black_pop_1990 <- jail_pop_1990 %>% 
  summarize(black_jail_pop)
black_1990 <- sum(black_pop_1990, na.rm=TRUE)

#calculate the Asian American population in jail in 1990
aapi_pop_1990 <- jail_pop_1990 %>% 
  summarize(aapi_jail_pop)
aapi_1990 <- sum(aapi_pop_1990, na.rm=TRUE)

#calculate the white population in jail in 1990
white_pop_1990 <- jail_pop_1990 %>% 
  summarize(white_jail_pop)

white_1990 <- sum(white_pop_1990, na.rm = TRUE)

#calculate the total population in jail in 2018
jail_pop_2018 <- df %>% 
  group_by(year = 2018)

#calculate the black population in jail in 2018
black_pop_2018 <- jail_pop_2018 %>% 
  summarize(black_jail_pop)
black_2018 <- sum(black_pop_2018, na.rm=TRUE)

#calculate the Asian American population in jail in 2018
aapi_pop_2018 <- jail_pop_2018 %>% 
  summarize(aapi_jail_pop)
aapi_2018 <- sum(aapi_pop_2018, na.rm=TRUE)


#calculate the white population in jail in 2018
white_pop_2018 <- jail_pop_2018 %>% 
  summarize(white_jail_pop)
white_2018 <- sum(white_pop_2018, na.rm = TRUE)

#calculate the rate of the growth of black_pop from 1990 to 2018
black_pop_growth <- (black_2018 - black_1990) / black_1990 * 100
print(black_pop_growth)

#calculate the rate of the growth of white_pop from 1990 to 2018
white_pop_growth <- (white_2018 - white_1990) / white_1990 * 100
print(white_pop_growth)

#calculate the rate of the growth of aapi_pop from 1990 to 2018
aapi_pop_growth <- (aapi_2018 - aapi_1990) / aapi_1990 * 100
print(aapi_pop_growth)

#the chart that reflects the black population growth in jail by years.
ggplot(data=df, mapping=aes(x=year, y=black_jail_pop)) +
  geom_point()+
  labs(title="The Black Population Changed By Yea",
       x="Years",
       y="Black Jail Population")

#the chart that reflects the Asian American and pacific island population growth in jail by years.
ggplot(data=df, mapping=aes(x=year, y=aapi_jail_pop)) +
  geom_point()+
  labs(title="The American and Pacific Island population Changed By Year",
       x="Years",
       y="Asian American and Pacific Island Jail Population")

#the chart that reflects the white population growth in jail by years.
ggplot(data=df, mapping=aes(x=year, y=white_jail_pop)) +
  geom_point()+
  labs(title="The White Population Changed By Year",
       x="Years",
       y="White Population in Jail")

#the chart that reflects the Asian population and white population comparison
df_2018 <- df %>% 
  group_by(year = 2018)
ggplot(data=df_2018, mapping=aes(x=white_jail_pop, y=aapi_jail_pop)) +
  geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  labs(title="The Comparison of White Population and Asian American population in Jail in 2018",
       x="white population",
       y="Asian American and Pacific Island Population")

#The map which represent the female jail population changed in different states over 30 years. 

#Load incarceration data
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#Find out the female jail population rate in different states and regions
state_female_jail_pop_rate <- incarceration %>% 
  filter(year == 2018) %>%
  group_by(state) %>%
  summarize(female_jail_population_rate = mean(female_jail_pop_rate,na.rm=TRUE))

state_shape <- map_data("state")
state_abbrevs <- data.frame(state.abb, state.name)

state_female_jail_pop_rate <- state_female_jail_pop_rate %>%
  left_join(state_abbrevs, by = c('state' = 'state.abb')) %>%
  mutate(region = tolower(state.name))

state_shape <- left_join(state_shape, state_female_jail_pop_rate, by = "region")

differnce_comparison_2018 <- incarceration %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarize(differnce_comparison_2018 = sum(female_jail_pop,na.rm=TRUE))

differnce_comparison_1988 <- incarceration %>%
  filter(year == 1988) %>%
  group_by(state) %>%
  summarize(differnce_comparison_1988= sum(female_jail_pop,na.rm=TRUE))

differnce_comparison <- left_join(differnce_comparison_2018, differnce_comparison_1988, by = "state") %>%
  mutate(difference = differnce_comparison_2018 - differnce_comparison_1988)

differnce_comparison_shape <- differnce_comparison %>%
  left_join(state_abbrevs, by = c('state' = 'state.abb')) %>%
  mutate(region = tolower(state.name))

differnce_comparison_shape <- left_join(state_shape, differnce_comparison_shape, by = "region")

ggplot(differnce_comparison_shape) +
  geom_polygon(mapping = aes(x  = long, y = lat, group = group, fill = difference)) +
  coord_map() +
  labs(
    title = "Female Jail Population Change By State Over 30 Years", 
    subtitle = "From 1988 to 2018",
    caption = "map",
    fill = "Population Change"
  ) +
  scale_fill_continuous(low = '#c0ebff', high = '#375c6e', labels = scales::label_number_si()) 