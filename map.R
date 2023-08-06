library(usmap)
library(ggplot2)
map <- {
  state_prison_pop <- new_data%>%
    group_by(state)%>%
    summarise(total=sum(total_prison_pop, na.rm = TRUE))
  
  state_map <- plot_usmap(data = state_prison_pop, values = "total", color = "red") +
    scale_fill_continuous(low = "white", high = "red", name = "Jail Population(2018)", label = scales::comma) +
    labs(title = "The Prison Population in US", subtitle = "from 1970 to 2018 ")+theme(legend.position = "right")
  
  print(state_map)
}

