library(MetBrewer)
library(plotly)
library(tidyverse)
avg_ticket_price <-broadway |>
  group_by(year) |>
  summarise(avg_ticket = mean(avg_ticket_2022))

plot_ticket_price_graph <- avg_ticket_price |>
  ggplot(mapping = aes(x = year, y =  avg_ticket, text = "")) +
  geom_smooth(color = "blue", se = FALSE, method = "loess", formula =  "y ~ x") +
  geom_point() + 
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020)) +
  scale_y_continuous(labels=scales::dollar_format()) + 
  labs(title = "Broadway Ticket Prices 1985 - 2022 (Adjusted for Inflation)",
       subtitle = "Ticket prices have been steadily increasing since 1985.",
       x = "Year", y = "Average Ticket Price",
       caption = "Data Source: Playbill.com") + 
  theme_minimal() 

write_rds(plot_ticket_price_graph, "plot_average_ticket_price.rds")

ggplotly(plot_ticket_price_graph)
