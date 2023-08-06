library(ggplot2)
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
