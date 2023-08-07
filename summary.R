library(tidyverse)
library(dplyr)

# load data
new_data <- {
  data <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true")
  data <- select(data, "year", "state","county_name","total_pop", 
                 "total_prison_pop", "female_prison_pop",
                 "male_prison_pop","white_prison_pop", "black_prison_pop")
}
new_data <- filter(new_data, total_prison_pop > 0)

#Summary Information

total_prison_pop <- new_data$total_prison_pop


# Question 1: What is the highest rate of prison? Which city is it?

new_data$prison_rate <- (new_data$total_prison_pop / new_data$total_pop) * 100
highest_rate <- arrange(new_data, desc(prison_rate)) %>% slice(1)
highest_rate


# Question 2: What are the five states with the largest prison population?

top_five_prison_pop <- new_data %>%group_by(state) %>%summarise(total_prison_pop = sum(total_prison_pop, na.rm = TRUE))%>%
  arrange(desc(total_prison_pop)) %>% slice(1:5)
top_five_prison_pop


# Question 3: Which county of California has the largest population in prison in recent year?

county <- new_data %>%
  filter(state == "CA", year == max(year)) %>%
  group_by(year, county_name) %>%
  summarise(total_prison_pop = max(total_prison_pop, na.rm = TRUE)) %>%
  arrange(desc(total_prison_pop)) %>%
  slice(1)
county


# Question 4: Ratio of male to female prison population in recent years.

gender_ratio <-new_data %>%
  filter(year == max(year)) %>%
  summarise(ratio = sum(male_prison_pop, na.rm = TRUE) 
            / sum(female_prison_pop, na.rm = TRUE))
gender_ratio


# Question 5: Ratio of male to female prison population in 1970.

ratio_1970 <- new_data %>%
  filter(year == 1970) %>%
  summarise(ratio = sum(male_prison_pop, na.rm = TRUE) 
            / sum(female_prison_pop, na.rm = TRUE))
ratio_1970
