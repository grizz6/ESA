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

# Load the dataset (Change the directory)
df1 <- read.csv("~/Documents/Ecology /USDA Grant/Pheno Files/Bee Behavior /Camcorder Ethogram Behavior Metadata - Behavior 2022 (bees only).csv")
df2 <- read.csv("~/Documents/Ecology /USDA Grant/Pheno Files/Bee Behavior /Camcorder Ethogram Behavior Metadata - Behavior 2023 (bees).csv")
df3 <- read.csv("~/Documents/Ecology /USDA Grant/Pheno Files/Bee Behavior /Camcorder Ethogram Behavior Metadata - Behavior 2024 (bees).csv")
df4 <- read.csv("~/Documents/Ecology /USDA Grant/Pheno Files/Bee Behavior /Orchards_GISdata_October_2023 (USE THIS) - Site information.csv")

# df1 belongs to the year 2022 so making a new colomn named Year and assigning value 2022 
df1 <- df1 %>%
  clean_names() %>%
  mutate(bee_id = as.character(bee_id)) 
df1$Year<-2022

# df2 belongs to the year 2023 so making a new colomn named Year and assigning value 2023 
df2 <- df2 %>%
  clean_names() %>%
  mutate(bee_id = as.character(bee_id)) 
df2$Year<-2023

# df3 belongs to the year 2023 so making a new colomn named Year and assigning value 2023 
df3 <- df3 %>%
  clean_names() %>%
  mutate(bee_id = as.character(bee_id)) 
df3$Year<-2024

# Calculating the ImperviousPercent (as shown below) and creating new colomn on the dataset called ImperviousPercent
df4 <- df4 %>%
  mutate(ImperviousPercent = (URBAN_IMPERVIOUS_500m / BuffSize_500) * 100)

# Changing the name of the colomn Project.Name into location so its the same in every dataset 
site_df <- df4 %>%
  rename(location = Project.Name)

# Renaming the site's name to its common name. (eg, House of Living Stone is HOLS)
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

# Merging the dataset (df1, df2, df3) and lower casing all the string values
behavior_df <- bind_rows(df1, df2, df3) %>%
  mutate(
    what_insect = tolower(trimws(what_insect)),
    classification = tolower(trimws(classification)),
    behavior = tolower(trimws(behavior))
  ) 

# Renaming or correcting the site's name accordingly 
behavior_df <- behavior_df %>%
  mutate(location = case_when(
    location %in% c("Earth Dance") ~ "EarthDance",
    location %in% c("HLS") ~ "HOLS",
    location %in% c("Viriginia") ~ "Virginia",
    TRUE ~ location
  )) 

# Selecting distinct data and merging 
sitename <- behavior_df %>%
  select(location) %>%
  distinct()

# Creating a list with all the behaviors name and required insect name
behavior_list <- c("scraping", "land", "leave", "unclear", "tapping", "stay", "interaction", 
                   "rubbing body", "rubbing face", "anther contact", 
                   "head frontal per", "head side per", "body side per", "pollinator combo")

insect_list <- c("apis mellifera", "osmia", "andrena", "other")

# Creating a list to run regression for required behavior only 
behavior_required <- c("scraping", "tapping", "interaction", 
                       "rubbing body", "rubbing face", "anther contact", 
                       "head frontal per", "head side per", "body side per", "pollinator combo")

# Removing missing insect and behavior. Also recoding some insects as "others"
behavior_df_clean <- behavior_df %>%
  filter(!is.na(what_insect), !is.na(behavior)) %>%
  mutate(
    what_insect = case_when(
      what_insect %in% c("ptilothrix bombiformis", "colletes", "lassioglossum") ~ "other",
      TRUE ~ what_insect
    )
  ) %>%
  filter(what_insect %in% insect_list)

# Merging the data by location as that the common colomn between the datasets
new_data <- behavior_df_clean %>%
  left_join(site_df, by = "location")

# Removing the missing ids and selecting required colomns
bee_impervious <- new_data %>%
  filter(!is.na(bee_id)) %>%
  dselect(location, Year, what_insect, bee_id, ImperviousPercent) %>%
  distinct()

# Counting behaviors per bee and assigning 1 and 0 if the bee did the behavior or not (1 = bee did it, 0 = bee didnt do it)
current_data <- behavior_df_clean %>%
  # Keep only rows where bee_id is not missing
  filter(!is.na(bee_id)) %>%
  select(location, Year, what_insect, bee_id, behavior) %>%
  # Group by unique bee and its behavior per site/year/species
  group_by(location, Year, what_insect, bee_id, behavior) %>%
  # Count the number of times each bee showed each behavior
  summarise(n = n(), .groups = 'drop') %>%
  # Create a binary indicator (Y.N):
  #   - 1 if the behavior was observed at least once
  #   - 0 if not
  mutate(Y.N = ifelse(n >= 1, 1, 0))

# Selecting unique bee across the sites
unique_bees <- current_data %>%
  select(location, Year, what_insect, bee_id) %>%
  distinct()

# Pivoting so all bees have all behaviors, and replacing the missing counts (NAs) as 0
complete_data <- unique_bees %>%
  cross_join(data.frame(behavior = behavior_list)) %>%
  # Join with current_data to attach observed behavior data
  # Matches on location, Year, insect type, bee_id, and behavior
  left_join(current_data, by = c("location", "Year", "what_insect", "bee_id", "behavior")) %>%
  # Replace missing values:
  # - If 'n' (count) is missing, set it to 0
  # - If 'Y.N' (binary outcome) is missing, set it to 0
  mutate(
    n = ifelse(is.na(n), 0, n),
    Y.N = ifelse(is.na(Y.N), 0, Y.N)
  ) %>%
  # Join with bee_impervious dataset to attach site-level impervious surface data
  left_join(bee_impervious, by = c("location", "Year", "what_insect", "bee_id"))

# Looping through each behavior in the list 'behavior_required'
for (behavior_name in behavior_required) {
  
  cat("========\n")
  cat("Behavior:", behavior_name, "\n") # Printing which behavior us currently shown in output
  
  # Filter data for the current behavior
  behavior_data <- complete_data %>%
    filter(behavior == behavior_name)
  
  # Exclude "osmia" records for all years except 2024 and remove "other" insects
  behavior_data <- behavior_data %>%
    filter(!(what_insect == "osmia" & Year != 2024)) %>%
    filter(what_insect != "other")

  years <- unique(behavior_data$Year)
  
  # Loop through each year separately
  for (year in years) {
    
    cat("Year:", year, "\n")
    
    # Filter dataset for that specific year
    year_data <- behavior_data %>%
      filter(Year == year)
    
    # Fit a logistic regression model (binomial family)
    # Response: Y.N (binary outcome)
    # Predictors: what_insect, ImperviousPercent, and their interaction
    model <- glm(Y.N ~ what_insect * ImperviousPercent, data = year_data, family = binomial)
    
    # Print model summary
    summ <- summary(model)
    print(summ)
    
    # Run anova Type III (YESS THERES A DIFFERENCE BETWEEN anova and Anova) 
    # Chi-square test for significance of predictors
    anova_res <-anova(model, type = "III", test = "Chisq")
    print(anova_res)
    
    # Extract test statistic and p-value for the interaction term
    test_stat <- anova_res$Dev[3]  
    p_value <- anova_res$`Pr(>Chi)`[3]
    
    # Print test results
    cat("  Test Statistic:", test_stat, "\n")
    cat("  P-value:", p_value, "\n")
    cat("\n")
  }
  
  cat("\n")
}




