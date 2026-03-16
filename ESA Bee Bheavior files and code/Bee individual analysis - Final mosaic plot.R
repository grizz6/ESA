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

# Load the dataset (Change the directory)
df1 <- read.csv("~/Documents/Ecology /Presentation code /ESA Bee Bheavior files and code/Camcorder Ethogram Behavior Metadata - Behavior 2022 (bees only).csv")
df2 <- read.csv("~/Documents/Ecology /Presentation code /ESA Bee Bheavior files and code/Camcorder Ethogram Behavior Metadata - Behavior 2023 (bees).csv")
df3 <- read.csv("~/Documents/Ecology /Presentation code /ESA Bee Bheavior files and code/Camcorder Ethogram Behavior Metadata - Behavior 2024 (bees).csv")
df4 <- read.csv("~/Documents/Ecology /Presentation code /ESA Bee Bheavior files and code/Orchards_GISdata_October_2023 (USE THIS) - Site information.csv")

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

# df3 belongs to the year 2024 so making a new colomn named Year and assigning value 2024
df3 <- df3 %>%
  clean_names() %>%
  mutate(bee_id = as.character(bee_id)) 
df3$Year<-2024

# Calculating the ImperviousPercent (as shown below) and creating new colomn on the dataset called ImperviousPercent
df4 <- df4 %>%
  mutate(ImperviousPercent = (URBAN_IMPERVIOUS_500m / BuffSize_500) * 100)

# Merging the dataset (df1, df2, df3) and lower casing all the string values
behavior_df <- bind_rows(df1, df2, df3) %>%
  mutate(
    what_insect = tolower(trimws(what_insect)),
    behavior = tolower(trimws(behavior))
  ) 

# Changing the name of the colomn Project.Name into location so its the same in every dataset 
site_df <- df4 %>%
  rename(location = Project.Name)

# Creating a list with all the behaviors name and required insect name
behavior_list <- c("scraping", "land", "leave", "unclear", "tapping", "stay", "interaction", 
                   "rubbing body", "rubbing face", "anther contact", 
                   "head frontal per", "head side per", "body side per", "pollinator combo")

insect_list <- c("apis mellifera", "osmia", "andrena", "other")

# Creating a list to run regression for required behavior only 
behavior_required <- c("scraping", "tapping", "interaction", 
                   "rubbing body", "rubbing face", "anther contact", 
                   "head frontal per", "head side per", "body side per", "pollinator combo")

# Creating a list to plot required behaviors
behavior_plot_required <- c("rubbing face", "anther contact", "head frontal per", "pollinator combo")

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

current_data <- behavior_df_clean %>%
  filter(!is.na(bee_id)) %>%
  dplyr::select(location, Year, what_insect, bee_id, behavior) %>%
  group_by(location, Year, what_insect, bee_id, behavior) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(Y.N = ifelse(n >= 1, 1, 0))

# Counting behaviors per bee and assigning 1 and 0 if the bee did the behavior or not (1 = bee did it, 0 = bee didnt do it)
# Refer to BeeVisual.R for further detailed comments
unique_bees <- current_data %>%
  dplyr::select(location, Year, what_insect, bee_id) %>%
  distinct()

# Pivoting so all bees have all behaviors, and replacing the missing counts (NAs) as 0
# Refer to BeeVisual.R for further detailed comments
complete_data <- unique_bees %>%
  cross_join(data.frame(behavior = behavior_list)) %>%
  left_join(current_data, by = c("location", "Year", "what_insect", "bee_id", "behavior")) %>%
  mutate(
    n = ifelse(is.na(n), 0, n),
    Y.N = ifelse(is.na(Y.N), 0, Y.N)
  )


# Loop over each behavior in the required list
for (behavior_name in behavior_required) {
  
  cat("========\n")
  cat("Behavior:", behavior_name, "\n")
  
  # Filter dataset for the current behavior
  behavior_data <- complete_data %>%
    filter(behavior == behavior_name)
  
  # Fit logistic regression model (binary outcome: Y.N ~ insect species)
  model <- glm(Y.N ~ what_insect, data = behavior_data, family = binomial)
  
  summ <- summary(model)
  print(summ)
  
  # Perform anova test on the model
  anova_res <- anova(model, test = "Chisq")
  print(anova_res)
  
  # Extract test statistic and p-value
  test_stat <- anova_res$Deviance[2]
  p_value <- anova_res$`Pr(>F)`[2]
  
  cat("Test Statistic:", test_stat, "\n")
  cat("P-value:", p_value, "\n")
  cat("\n")
}

# Plotting 
for (behavior_name in behavior_plot_required) {
  
  behavior_data <- complete_data %>%
    filter(behavior == behavior_name) %>%
    mutate(
      Behavior_YN = ifelse(Y.N == 1, "Yes", "No"),       # Convert numeric Y.N to categorical labels
      Species = case_when(                               # Standardize insect species names for cleaner plots
        what_insect == "apis mellifera" ~ "Apis mellifera",     
        what_insect == "osmia" ~ "Osmia", 
        what_insect == "andrena" ~ "Andrena",
        what_insect == "other" ~ "Other",
        TRUE ~ what_insect
      )
    )
  
  # Create mosaic plot by species for each behavior
  plot <- ggplot(data = behavior_data) +
    geom_mosaic(aes(x = product(Species), fill = Behavior_YN)) +
    scale_fill_manual(values = c("No" = "#F8B4B4", "Yes" = "#7DD3C0")) +
    labs(
      title = paste("Mosaic by Species -", str_to_title(behavior_name)),
      x = "Species",
      y = str_to_title(behavior_name),
      fill = str_to_title(behavior_name)
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
  
  print(plot)
}

# anther contact, rubbing face, head frontal PER, pollinator combo



