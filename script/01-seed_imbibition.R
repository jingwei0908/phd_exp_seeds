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

ciar <- seeds_tidy %>%
  filter(Spp == "CIAR" & !is.na(Section))

seed_ciar <- ciar %>%
  mutate(Date_ymd = as.character(Date_ymd)) %>%
  mutate(time = case_when(
    Date_ymd == "2022-08-15" ~ "t1",
    Date_ymd == "2022-08-16" ~ "t2",
    Date_ymd == "2022-08-17" ~ "t1", 
    Date_ymd == "2022-08-18" ~ "t2", 
    TRUE ~ Date_ymd)
  ) %>%
  
  mutate(
    Seed_mass_mg = as.numeric(Seed_mass_mg),
    Seed_mass_mg = round(Seed_mass_mg, digits = 2)
  ) %>%
  
  mutate(
    time = factor(time, levels = c("t1", "t2")), 
    Section = factor(
      Section, 
      levels = c("1", "2", "3", "4", "5", "6")
      )
    ) 

# visualize data ----

(seeds_ciar_plot <- seed_ciar %>%
  ggplot(aes(y = ID, x = Seed_mass_mg)) + 
  geom_point(aes(color = time)) + 
  facet_wrap(~Section) + 
  theme_bw() + 
  scale_color_discrete(
    name = "Time", 
    labels = c("Hour 0", "Hour 24")
  )
)

(seeds_ciar_plot2 <- seed_ciar %>%
    ggplot(aes(x = Section, y = Seed_mass_mg)) + 
    geom_boxplot() + 
    geom_point(alpha = 0.1) + 
    facet_wrap(~time) +
    labs(y = "Seed mass (mg)") +
    theme_bw()
)

# analyze data ----

aov_ciar <- aov(
  formula = Seed_mass_mg ~ time + Section, 
  data = seed_ciar
)
