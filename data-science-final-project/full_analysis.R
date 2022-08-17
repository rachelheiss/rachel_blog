
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(MetBrewer)
library(plotly)


# Clean data --------------------------------------------------------------

broadway_raw <- read_csv("data/1985-2022.csv")

# https://econs22.classes.andrewheiss.com/resource/inflation/

cpi_target <- 281.933

fred_cpi <- read_csv("data/CPIAUCSL.csv") |> 
  mutate(month = month(DATE),
         year = year(DATE)) |> 
  filter(month == 1) |> 
  mutate(cpi_2022 = CPIAUCSL / (cpi_target / 100)) |> 
  select(year, cpi_2022)


broadway <- broadway_raw |> 
  # There are some rows with $0 for price; get rid of them
  filter(Average_ticket > 0) |>
  # Make this a date
  mutate(actual_date = dmy(Year)) |> 
  mutate(year = year(actual_date)) |> 
  rename(pct_cap = `%cap`) |> 
  left_join(fred_cpi, by = "year") |> 
  mutate(avg_ticket_2022 = Average_ticket / (cpi_2022 / 100))

write_rds(broadway, "clean_data.rds")



# Average ticket stuff ----------------------------------------------------

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

# Average theatre capacity stuff ------------------------------------------

avg_theatre_pct_cap <- broadway |>
  group_by(year) |>
  summarise(avg_pct_cap = mean(pct_cap)) |>
  mutate(avg_pct_cap = avg_pct_cap/100)

plot_pct_cap_graph <- avg_theatre_pct_cap  |>
  ggplot(mapping = aes(x = year, y =  avg_pct_cap)) +
  geom_smooth(color = "blue", se = FALSE, method = "loess", formula =  "y ~ x") +
  geom_point() + 
  scale_y_continuous(labels = scales::percent) + 
  theme_minimal() + 
  labs(x = "Year", y = "Theatre Capacity Filled", 
       title = "Average Percentage of Theatre Capacity Filled 1985 - 2022",
       subtitle = "More people have been ")

write_rds(plot_pct_cap_graph, "plot_pct_cap.rds") 



# Top shows stuff ---------------------------------------------------------

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

# Adding avg ticket price
top_shows_avg_ticket <- broadway |> 
  filter(Show_name %in% top_shows_year_count$Show_name) |> 
  group_by(Show_name) |> 
  summarize(avg_ticket_price = mean(avg_ticket_2022))

# joining the three datasets

top_shows_cap <- top_shows_year_count |> 
  left_join(avg_capacity, by = "Show_name") |> 
  left_join(total_years_running, by = "Show_name") |> 
  left_join(top_shows_avg_ticket, by = "Show_name") |> 
  mutate(pct_years_full = n / years_running) |>
  mutate(avg_cap = avg_cap/100) |>
  mutate(lifetime_category = case_when(
    years_running >= 30 ~ "More than 30",
    years_running >= 20 & years_running < 30 ~ "20 to 30",
    years_running >= 10 & years_running < 20 ~ "10 to 20",
    years_running < 10 ~ "Less than 10"
  )) |>
  mutate(lifetime_category = factor(lifetime_category, 
                               levels = c("Less than 10", 
                                          "10 to 20", 
                                          "20 to 30", 
                                          "More than 30"), 
                               ordered = TRUE))
# most played shows graph 
plot_top_shows <-
  ggplot(top_shows_year_count, mapping = aes(x = n, y = fct_rev(Show_name))) +
  geom_col(fill = my_colors[1]) +
  scale_x_continuous(breaks = seq(0, 30, 4)) + 
  theme_minimal() +
  labs(y = NULL, 
       x = "Number of Years Ran Weekly", 
       title = "Most Played Shows",
       subtitle = "")

write_rds(plot_top_shows, "plot_top_shows.rds") 

# Avg capacity + pct years running weekly graph (color dots by total l time running?)
plot_pct_vs_cap <- 
  ggplot(top_shows_cap, aes(x = pct_years_full, y = avg_cap)) + 
  geom_smooth(se = FALSE, method = "lm", color = my_colors[1]) +
  geom_point(aes(text = Show_name,  color = lifetime_category)) +
  scale_x_continuous(labels = scales::percent_format()) + 
  scale_y_continuous(labels=scales::percent_format()) +
  scale_color_manual(values = c(my_colors[4], 
                                my_colors[5], 
                                my_colors[6],
                                my_colors[7])) +
  theme_minimal() + 
  labs(x = "Percent of Lifetime Running Weekly", 
       y = "Percent of Theatre Capacity Filled",
       title = "Average Theatre Capacity Filled for Top Shows",
       color = "Total Years on Broadway")

plot_interactive_pct_vs_cap <- ggplotly(plot_pct_vs_cap, tooltip = "text")

write_rds(plot_pct_vs_cap, "plot_pct_vs_cap.rds") 
write_rds(plot_interactive_pct_vs_cap, "plot_interactive_pct_vs_cap.rds")

# Avg top shows ticket price graph (color dots by total time running?)

my_colors <- met.brewer("Veronese", 7)
plot_top_ticket_price <- ggplot(top_shows_cap, aes(y = avg_ticket_price, 
                                                   x = pct_years_full)) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, color = my_colors[1]) +
  geom_point(aes(text = Show_name, color = lifetime_category)) + 
  scale_x_continuous(labels = scales::percent_format()) + 
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_color_manual(values = c(my_colors[4], 
                                my_colors[5], 
                                my_colors[6],
                                my_colors[7])) + 
  theme_minimal() + 
  labs(title = "Average Ticket Prices for Top Shows",
       x = "Percent of Lifetime Running Weekly", 
       y = "Ticket Price",
       color = "Total Years on Broadway") 

plot_avg_top_shows_tickets <- ggplotly(plot_top_ticket_price, tooltip = "text") 

write_rds(plot_avg_top_shows_tickets, "plot_avg_top_shows_tickets_graph.rds")
   



