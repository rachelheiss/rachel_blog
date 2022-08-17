library(MetBrewer)
library(plotly)
library(tidyverse)
library(ggplot2)
library(gganimate)
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

# Animated ticket price/theater capacity graph
library(gganimate)

my_colors <- met.brewer("Veronese", 7)

plot_years_pct_cap_tickets_animated <- broadway |>
  mutate(price_bucket = case_when(
    avg_ticket_2022 < 100 ~ "Cheap",
    avg_ticket_2022 >=100 & avg_ticket_2022 < 200 ~ "Mid-priced",
    avg_ticket_2022 >=200 ~ "Expensive"
  )) |>
  mutate(pct_cap = pct_cap/100) |>
ggplot(aes(x = pct_cap, y = avg_ticket_2022, color = price_bucket)) + 
  geom_point(alpha = .5) +
  scale_color_manual(values = c(my_colors[1], my_colors[3], my_colors[7])) + 
  # geom_smooth() +
  theme_minimal() + 
  scale_x_continuous(labels = scales::percent_format()) + 
  scale_y_continuous(labels=scales::dollar_format()) +
  labs(title = "Year: {round(frame_time, 0)}",
       x = "Capacity Filled",
       y = "Ticket Price",
       color = "Price",
       subtitle = "Data Source: Playbill.com") + 
  transition_time(year) + 
  ease_aes("quadratic-in-out", interval = 1.00)

plot_years_pct_cap_tickets_animated_graph <- animate(plot_years_pct_cap_tickets_animated,
        start_pause = 15, 
        end_pause = 15, 
        nframes= 900, 
        fps = 18) 
anim_save(plot_years_pct_cap_tickets_animated_graph, filename = "animated_years_pct_cap_tickets.gif")

plot_years_pct_cap_tickets_animated_graph

animated_years_pct_cap_tickets_mp4 <- animate(plot_years_pct_cap_tickets_animated, 
                                     width = 1200, 
                                     height = 720, 
                                     res = 150,
                                     nframes = 900, 
                                     fps = 18,
                                     renderer = av_renderer("animated_years_pct_cap_tickets.mp4"))
