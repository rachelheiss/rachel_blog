avg_theatre_pct_cap <- broadway |>
  group_by(year) |>
  summarise(avg_pct_cap = mean(pct_cap)) |>
  mutate(avg_pct_cap = avg_pct_cap/100)

plot_pct_cap_graph <- avg_theatre_pct_cap  |>
  ggplot(mapping = aes(x = year, y =  avg_pct_cap)) +
  geom_smooth(color = "blue", se = FALSE, method = "loess", formula =  "y ~ x") +
  geom_point() + 
  scale_y_continuous(labels = scales::percent) + 
  theme_linedraw() + 
  labs(x = "Year", y = "Average Percentage of Theatre Capacity Filled")

write_rds(plot_pct_cap_graph, "plot_pct_cap.rds") 

Cross 