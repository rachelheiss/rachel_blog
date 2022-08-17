
# Filtering for top shows
weekly_shows <- broadway |>
  count(year, Show_name) |>
  filter(n >= 52) 

# Rearranging
top_shows_year_count <- weekly_shows |> 
  count(Show_name) |>
  filter(n >= 4) |> 
  arrange(desc(n)) |> 
  mutate(Show_name = fct_inorder(Show_name))

# Adding average capacity

avg_capacity <- broadway |> 
  filter(Show_name %in% top_shows_year_count$Show_name) |> 
  group_by(Show_name) |> 
  summarize(avg_cap = mean(pct_cap))

# Adding years running weekly vs total years running
total_years_running <- broadway |> 
  filter(Show_name %in% top_shows_year_count$Show_name) |> 
  group_by(Show_name) |> 
  slice(1) |> 
  mutate(years_running = 2021 - year) |> 
  select(Show_name, years_running)

# joining the two datasets

top_shows_cap <- top_shows_year_count |> 
  left_join(avg_capacity, by = "Show_name") |> 
  left_join(total_years_running, by = "Show_name") |> 
  mutate(pct_years_full = n / years_running) |>
  mutate(avg_cap = avg_cap/100)

# Avg capacity + pct years running weekly graph
plot_pct_vs_cap <- 
ggplot(top_shows_cap, aes(x = pct_years_full, y = avg_cap)) + 
  geom_smooth(se = FALSE, method = "lm", color = "blue") +
  geom_point(aes(text = Show_name)) +
  scale_x_continuous(labels = scales::percent_format()) + 
  scale_y_continuous(labels=scales::percent_format()) +
  labs(x = "Percent of Lifetime Running Weekly", 
       y = "Percent of Theatre Capacity Filled",
       title = "")

ggplotly(plot_pct_vs_cap, tooltip = "text")

write_rds(plot_pct_vs_cap, "plot_pct_vs_cap.rds")

# most played shows graph 
plot_top_shows <-
  ggplot(top_shows_year_count, mapping = aes(x = n, y = fct_rev(Show_name))) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 30, 4)) + 
  theme_minimal() +
  labs(y = NULL, 
       x = "Number of Years Ran Weekly", 
       title = "Most Played Shows",
       subtitle = "")

write_rds(plot_top_shows, "plot_top_shows.rds") 

