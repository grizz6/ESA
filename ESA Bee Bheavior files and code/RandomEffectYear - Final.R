# Load the packages, if you wanna know which function uses what package; just google i'll be honest
library(ggplot2)
library(stringr)
library(dplyr)
library(lme4)
library(readr)
library(tidyverse)
library(janitor)
library(ggmosaic) 
library(car)
library(gridExtra)
library(rlang)
library(pbkrtest)

# result imp for rubbing body and scraping, calculate species, location, proportion of individual that did the behavior (subset)

# Everything is the same as ByYear - final.R so you can use that as a comment reference 
# However the only difference in this code is all Years are taken into consideration instead of running the model through each individual year. 

df1 <- read.csv("~/Documents/Ecology /USDA Grant/Pheno Files/Bee Behavior /Camcorder Ethogram Behavior Metadata - Behavior 2022 (bees only).csv")
df2 <- read.csv("~/Documents/Ecology /USDA Grant/Pheno Files/Bee Behavior /Camcorder Ethogram Behavior Metadata - Behavior 2023 (bees).csv")
df3 <- read.csv("~/Documents/Ecology /USDA Grant/Pheno Files/Bee Behavior /Camcorder Ethogram Behavior Metadata - Behavior 2024 (bees).csv")
df4 <- read.csv("~/Documents/Ecology /USDA Grant/Pheno Files/Bee Behavior /Orchards_GISdata_October_2023 (USE THIS) - Site information.csv")

df1 <- df1 %>%
  clean_names() %>%
  mutate(bee_id = as.character(bee_id)) 
df1$Year<-2022

df2 <- df2 %>%
  clean_names() %>%
  mutate(bee_id = as.character(bee_id)) 
df2$Year<-2023

df3 <- df3 %>%
  clean_names() %>%
  mutate(bee_id = as.character(bee_id)) 
df3$Year<-2024

df4 <- df4 %>%
  mutate(ImperviousPercent = (URBAN_IMPERVIOUS_500m / BuffSize_500) * 100)

site_df <- df4 %>%
  rename(location = Project.Name)

site_df <- site_df %>%
  mutate(location = case_when(
    location %in% c("Our Lady of the Holy Cross Catholic Church Orchard") ~ "Holy Cross",
    location %in% c("COLA Victory Garden") ~ "COLA",
    location %in% c("Rustic Roots (formerly GROW)") ~ "Rustic Roots",
    location %in% c("EarthDance") ~ "EarthDance",
    location %in% c("Old Ferguson West") ~ "Ferguson",
    location %in% c("Emmanuel Episcopal Church Orchard") ~ "Emmanuel",
    location %in% c("Kellogg Park Community Garden and Orchard") ~ "Kellogg",
    location %in% c("House of Living Stone") ~ "HOLS",
    location %in% c("Florissant Community Garden") ~ "Florissant",
    location %in% c("St. Louis University Teaching Garden and Orchard") ~ "SLU",
    location %in% c("McKinley Meadows") ~ "McKinley",
    location %in% c("13th Street Community Garden ") ~ "13th Street",
    location %in% c("The Orchard on Virginia") ~ "Virginia",
    location %in% c("Carondalet Food Forest") ~ "Carondelet",
    location %in% c("Thies Farm and Greenhouses, Inc.") ~ "Thies Farm",
    TRUE ~ location
  )) 

behavior_df <- bind_rows(df1, df2, df3) %>%
  mutate(
    what_insect = tolower(trimws(what_insect)),
    classification = tolower(trimws(classification)),
    behavior = tolower(trimws(behavior))
  ) 

behavior_df <- behavior_df %>%
  mutate(location = case_when(
    location %in% c("Earth Dance") ~ "EarthDance",
    location %in% c("HLS") ~ "HOLS",
    location %in% c("Viriginia") ~ "Virginia",
    TRUE ~ location
  )) 

sitename <- behavior_df %>%
  select(location) %>%
  distinct()

behavior_list <- c("scraping", "land", "leave", "unclear", "tapping", "stay", "interaction", 
                   "rubbing body", "rubbing face", "anther contact", 
                   "head frontal per", "head side per", "body side per", "pollinator combo")

behavior_required <- c("scraping", "tapping", "interaction", 
                       "rubbing body", "rubbing face", "anther contact", 
                       "head frontal per", "head side per", "body side per", "pollinator combo")

insect_list <- c("apis mellifera", "osmia", "andrena", "other")

behavior_df_clean <- behavior_df %>%
  filter(!is.na(what_insect), !is.na(behavior)) %>%
  mutate(
    what_insect = case_when(
      what_insect %in% c("ptilothrix bombiformis", "colletes", "lassioglossum") ~ "other",
      TRUE ~ what_insect
    )
  ) %>%
  filter(what_insect %in% insect_list)

new_data <- behavior_df_clean %>%
  left_join(site_df, by = "location")

bee_impervious <- new_data %>%
  filter(!is.na(bee_id)) %>%
  select(location, Year, what_insect, bee_id, ImperviousPercent) %>%
  distinct()

current_data <- behavior_df_clean %>%
  filter(!is.na(bee_id)) %>%
  select(location, Year, what_insect, bee_id, behavior) %>%
  group_by(location, Year, what_insect, bee_id, behavior) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(Y.N = ifelse(n >= 1, 1, 0))

unique_bees <- current_data %>%
  select(location, Year, what_insect, bee_id) %>%
  distinct()

complete_data <- unique_bees %>%
  cross_join(data.frame(behavior = behavior_list)) %>%
  left_join(current_data, by = c("location", "Year", "what_insect", "bee_id", "behavior")) %>%
  mutate(
    n = ifelse(is.na(n), 0, n),
    Y.N = ifelse(is.na(Y.N), 0, Y.N)
  ) %>%
  left_join(bee_impervious, by = c("location", "Year", "what_insect", "bee_id"))

for (behavior_name in behavior_required) {

  cat("========\n")
  cat("Behavior:", behavior_name, "\n")
  
  behavior_data <- complete_data %>%
    filter(behavior == behavior_name)
  
  model <- glmer(Y.N ~ what_insect * ImperviousPercent + (1 | Year), data = behavior_data, family = binomial, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e1)))
  
  summ <- summary(model)
  print(summ)
  
  anova_res <- Anova(model, type = "III", test = "Chisq")
  print(anova_res)
  
  test_stat <- anova_res$Chisq[3]  
  p_value <- anova_res$`Pr(>Chisq)`[3]
   
  cat("Test Statistic:", test_stat, "\n")
  cat("P-value:", p_value, "\n")
  cat("\n")
}
