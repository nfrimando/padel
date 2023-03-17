library(tidyverse)
library(googlesheets4)

dt <- read_sheet(
  'https://docs.google.com/spreadsheets/d/1wEG4Z8zCMVmBjroEG5X3wS6PISXJDMfJu8TCWOm99KY/edit#gid=868738369',
  sheet = 'Padel Games Analysis'
)

# Overall Performance
dt %>%
  mutate(point_type_bool = case_when(
    point_type == "Unforced Error" ~ -1,
    point_type == "Winner" ~ 1,
    TRUE ~ as.numeric(NA)
  )) %>%
  group_by(player) %>%
  mutate(order = row_number()) %>%
  mutate(cumulative_perf = cumsum(point_type_bool)) %>%
  ungroup(player) %>%
  select(player, cumulative_perf, order) %>%
  ggplot() +
  geom_line(
    mapping = aes(x = order, y = cumulative_perf)
  ) +
  facet_wrap(~player, ncol = 4)

# Shot Stats
dt %>%
  group_by(player, point_type, shot_type) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(player, point_type, desc(count)) %>% View()

