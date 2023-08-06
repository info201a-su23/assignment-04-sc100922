library(tidyverse)
library(dplyr)
library(ggplot2)
library(usmap)

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


# Question 2: What are the five states with the largest prison population?

top_five_prison_pop <- new_data %>%group_by(state) %>%summarise(total_prison_pop = sum(total_prison_pop, na.rm = TRUE))%>%
  arrange(desc(total_prison_pop)) %>% slice(1:5)


# Question 3: Which county of California has the largest population in prison in recent year?

county <- new_data %>%
  filter(state == "CA", year == max(year)) %>%
  group_by(year, county_name) %>%
  summarise(total_prison_pop = max(total_prison_pop, na.rm = TRUE)) %>%
  arrange(desc(total_prison_pop)) %>%
  slice(1)


# Question 4: Ratio of male to female prison population in recent years.

gender_ratio <-new_data %>%
  filter(year == max(year)) %>%
  summarise(ratio = sum(male_prison_pop, na.rm = TRUE) 
            / sum(female_prison_pop, na.rm = TRUE))


# Question 5: Ratio of male to female prison population in 1970.

ratio_1970 <- new_data %>%
  filter(year == 1970) %>%
  summarise(ratio = sum(male_prison_pop, na.rm = TRUE) 
            / sum(female_prison_pop, na.rm = TRUE))


#Trend over time chart
#display the trend over time of male and female prison population.

male_data <- new_data %>%
  group_by(year) %>%
  summarise(male = sum(male_prison_pop, na.rm = TRUE))

female_data <- new_data %>%
  group_by(year) %>%
  summarise(female = sum(female_prison_pop, na.rm = TRUE))

ggplot() +
  geom_line(data = male_data, aes(x = year, y = male, color = "Male")) +
  geom_line(data = female_data, aes(x = year, y = female, color = "Female")) +
  scale_y_continuous(labels = scales::comma) + 
  labs(
    x = "Year",
    y = "Total Prison Population",
    title = "Trend Over Time of Male and Female Prison Population"
  )


# Variable Comparison Chart
# Comparison of Black and White Prison Populations in Each State

black_prison_pop <- new_data %>%
  group_by(state) %>%
  summarise(prison_pop = sum(black_prison_pop, na.rm = TRUE))

white_prison_pop <- new_data %>%
  group_by(state) %>%
  summarise(prison_pop = sum(white_prison_pop, na.rm = TRUE))

black_prison_pop$race <- "Black"
white_prison_pop$race <- "White"

combine_data <- rbind(black_prison_pop, white_prison_pop)

ggplot(combine_data, aes(x = state, y = prison_pop, fill = race)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::comma) + 
  labs(
    x = "State",
    y = "Total Prison Population",
    fill = "Race",
    title = "Comparison of Black and White Prison Populations in Each State"
  )


# Map
# it shows the total prison population in each states in the US.
map <- {
  state_prison_pop <- new_data%>%
    group_by(state)%>%
    summarise(total=sum(total_prison_pop, na.rm = TRUE))
  
  state_map <- plot_usmap(data = state_prison_pop, values = "total", color = "red") +
    scale_fill_continuous(low = "white", high = "red", name = "Jail Population(2018)", label = scales::comma) +
    labs(title = "The Prison Population in US", subtitle = "from 1970 to 2018 ")+theme(legend.position = "right")
  
  print(state_map)
}