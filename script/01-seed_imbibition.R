# libraries ----
library(readxl)  # for reading excel files
library(here)    # for creating relative file-paths
library(dplyr)   # for manipulating data
library(ggplot2) # for visualizing data

# import ----

seeds_df <- read_excel(
  here("data", "raw_data", "phd_exp_seed_imbibition_test.xlsx")
)

# check packaging ----
str(seeds_df)
head(seeds_df, n = 5)
tail(seeds_df, n = 5)

# clean data ----

seeds_tidy <- seeds_df %>%
  filter(Spp != "RUHI")

# visualize data ----

hehe <- seeds_tidy %>%
  filter(Spp == "HEHE") 

ciar <- seeds_tidy %>%
  filter(Spp == "CIAR")

seed_hehe <- hehe %>%
  mutate(Date_ymd = as.character(Date_ymd)) %>%
  ggplot(aes(x = Seed_mass_mg, fill = Date_ymd)) +
  geom_density(alpha = 0.1) +
  theme_bw()

seed_hehe2 <- hehe %>%
  ggplot(aes(x = Seed_mass_mg, y = ID, col = Date_ymd)) + 
  geom_point()

seed_ciar <- ciar %>%
  mutate(Date_ymd = as.character(Date_ymd)) %>%
  ggplot(aes(x = Seed_mass_mg, fill = Date_ymd)) +
  geom_density(alpha = 0.1) +
  theme_bw()

seed_ciar2 <- ciar %>%
  ggplot(aes(x = Seed_mass_mg, y = ID, col = Date_ymd)) +
  geom_point()

# t-test ----

# hehe

hehe_t1 <- hehe %>%
  mutate(Date_ymd = as.character(Date_ymd)) %>%
  filter(Date_ymd == "2022-08-08") %>%
  pull(Seed_mass_mg)

hehe_t2 <- hehe %>%
  mutate(Date_ymd = as.character(Date_ymd)) %>%
  filter(Date_ymd == "2022-08-09") %>%
  pull(Seed_mass_mg)

t_test_hehe <- t.test(
  x = hehe_t1, 
  y = hehe_t2, 
  alternative = "two.sided",
  paired = TRUE,
  conf.level = 0.95
)

# ciar

ciar_t1 <- ciar %>%
  mutate(Date_ymd = as.character(Date_ymd)) %>%
  filter(Date_ymd == "2022-08-08") %>%
  pull(Seed_mass_mg)

ciar_t2 <- ciar %>%
  mutate(Date_ymd = as.character(Date_ymd)) %>%
  filter(Date_ymd == "2022-08-09") %>%
  pull(Seed_mass_mg)

t_test_ciar <- t.test(
  x = ciar_t1, 
  y = ciar_t2, 
  alternative = "two.sided",
  paired = TRUE
)

ggsave(
  filename = here("output", "seed_mass.png"), 
  plot = seed_mass, 
  device = "png", 
  units = "in", 
  height = 5, 
  width = 5
)