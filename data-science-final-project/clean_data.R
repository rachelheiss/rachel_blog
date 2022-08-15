library(tidyverse)
library(lubridate)

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
