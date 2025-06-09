full_names <- rio::import(here::here("data/fish_data.xlsx"))
df <- rio::import(here::here("data/fish_simple.csv"))
full_names <- full_names %>% 
  janitor::clean_names()
names(full_names)
full_names <- full_names %>% 
  mutate(
    full_name = name
  ) %>% 
  select(pos_number, full_name)
full_names <- unique(full_names)
df <- left_join(df, full_names, by = "pos_number")
df$name <- df$full_name
df <- df %>% 
  select(-full_name)
df$location_collapsed <- factor(df$location_collapsed, levels = c("river", "ocean", "pond"), labels = c("River", "Ocean", "Pond"))

write_csv(df, here::here("fish_simple.csv"))

rm(list = ls())
df <- rio::import(here::here("fish.csv"))
