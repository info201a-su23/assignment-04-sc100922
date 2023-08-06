library(ggplot2)
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

