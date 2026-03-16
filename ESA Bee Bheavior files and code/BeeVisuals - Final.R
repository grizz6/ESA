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
library(patchwork)

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

# df3 belongs to the year 2024 so making a new colomn named Year and assigning value 2024
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
  dplyr::select(location) %>%
  distinct()

# Creating a list with all the behaviors name and required insect name
behavior_list <- c("scraping", "land", "leave", "unclear", "tapping", "stay", "interaction", 
                   "rubbing body", "rubbing face", "anther contact", 
                   "head frontal per", "head side per", "body side per", "pollinator combo")

insect_list <- c("apis mellifera", "andrena", "osmia", "other")

# Creating a list to plot out regression line for required behavior
behaviors_to_plot <- c("scraping",  "head side per", "anther contact")

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
  select(location, Year, what_insect, bee_id, ImperviousPercent) %>%
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

# Calculating the proportion of bee's behavior 
behavior_proportions <- complete_data %>%
  filter(behavior %in% behaviors_to_plot) %>%
  group_by(location, Year, what_insect, ImperviousPercent, behavior) %>%
  summarise(
    total_individuals = n_distinct(bee_id),
    individuals_with_behavior = sum(Y.N),
    proportion = individuals_with_behavior / total_individuals,
    .groups = 'drop'
  ) %>%
  filter(!is.na(ImperviousPercent))

# Scaling the percentage proportion 
behavior_proportions <- behavior_proportions %>%
  mutate(impervious_proportion = ImperviousPercent / 100)

# Creating a list to use for function thats wayyy below (at the end of the code)
plot_list <- list()

# Statistical test results for behaviors across years from BYYear - final.R code 
stat_results <- list(
  "scraping" = list(
    "2022" = list(test_stat = 3.357, p_value = 0.067),
    "2023" = list(test_stat = 9.668, p_value = 0.002),
    "2024" = list(test_stat = 0.310, p_value = 0.578)
  ),
  "head side per" = list(
    "2022" = list(test_stat = 1.952, p_value = 0.162),
    "2023" = list(test_stat = 0.802, p_value = 0.371),
    "2024" = list(test_stat = 1.605, p_value = 0.205)
  ),
  "anther contact" = list(
    "2022" = list(test_stat = 0.507, p_value = 0.477),
    "2023" = list(test_stat = 2.161, p_value = 0.142),
    "2024" = list(test_stat = 0.366, p_value = 0.545)
  )
)

# Helper function for formatting p-values
format_p_value <- function(p_val) {
  if (is.na(p_val)) return("p = NA")
  if (p_val < 0.001) {
    return("p < 0.001")
  } else if (p_val < 0.01) {
    return(paste0("p = ", sprintf("%.3f", p_val)))
  } else {
    return(paste0("p = ", sprintf("%.2f", p_val)))
  }
}

# Helper function for formatting significance 
get_significance <- function(p_val) {
  if (is.na(p_val)) return("")
  if (p_val < 0.001) {
    return("***")
  } else if (p_val < 0.01) {
    return("**")
  } else if (p_val < 0.05) {
    return("*")
  } else {
    return("")
  }
}

# ---------------------------------------------------------------
# Looping through each behavior, filtering relevant data, 
# plotting proportion vs impervious surface, adding regression lines for selected cases,
# and annotated with test statistics.
# ---------------------------------------------------------------

for(behavior_name in behaviors_to_plot) {
  
  plot_data <- behavior_proportions %>%
    filter(behavior == behavior_name)
  
  plot_data <- plot_data %>%
    filter(!(what_insect == "osmia" & Year != 2024)) %>% # remove Osmia except in 2024
    filter(what_insect != "other")   # drop "other" category
  
  # scatter plot
  p <- ggplot(plot_data, aes(x = impervious_proportion, y = proportion, 
                             color = what_insect, shape = what_insect)) +
    geom_point(size = 3.5, alpha = 0.7)
  
  # Adding regression lines for certain behaviors/years
  if(behavior_name == "scraping") {
    p <- p + 
      geom_smooth(data = filter(plot_data, Year == 2023), 
                  method = "lm", se = FALSE, color = "black", linetype = "solid", 
                  inherit.aes = FALSE, 
                  aes(x = impervious_proportion, y = proportion))
    
  } else if(behavior_name == "rubbing body") {
    p <- p + 
      geom_smooth(data = filter(plot_data, Year == 2022), 
                  method = "lm", se = FALSE, color = "black", linetype = "solid", 
                  inherit.aes = FALSE,
                  aes(x = impervious_proportion, y = proportion))
    
  }  else if(behavior_name == "head frontal per") {
    p <- p + 
      geom_smooth(data = filter(plot_data, Year == 2022), 
                  method = "lm", se = FALSE, linetype = "solid", 
                  aes(x = impervious_proportion, y = proportion, 
                      color = what_insect, group = what_insect))
    
  } else if(behavior_name == "head side per") {
    
  } else if(behavior_name == "body side per") {
    p <- p + 
      geom_smooth(data = filter(plot_data, Year == 2023), 
                  method = "lm", se = FALSE, linetype = "solid", 
                  aes(x = impervious_proportion, y = proportion, 
                      color = what_insect, group = what_insect))
    
  } else if(behavior_name == "pollinator combo") {
    p <- p + 
      geom_smooth(data = filter(plot_data, Year == 2023), 
                  method = "lm", se = FALSE, linetype = "solid", 
                  aes(x = impervious_proportion, y = proportion, 
                      color = what_insect, group = what_insect))
  }
  
  # Add statistical results in caption
  if (behavior_name %in% names(stat_results)) {
    all_stats <- c()
    for (year in c("2022", "2023", "2024")) {
      if (year %in% names(stat_results[[behavior_name]])) {
        stats <- stat_results[[behavior_name]][[year]]
        test_stat <- sprintf("%.3f", stats$test_stat)
        p_val <- stats$p_value
        p_formatted <- format_p_value(p_val)
        
        year_text <- paste0(year, ": χ² = ", test_stat, ", ", p_formatted)
        all_stats <- c(all_stats, year_text)
      }
    }
    
    fig_caption <- paste(all_stats, collapse = "\n")
  }
  
  # Style, facet by year, adding custom colors/shapes, captions, and themes
  p <- p +
    facet_wrap(~ Year, ncol = 3) +
    scale_color_manual(values = c("apis mellifera" = "#E74C3C", 
                                  "osmia" = "#27AE60", 
                                  "andrena" = "#3498DB", 
                                  "other" = "#9B59B6"),
                       name = "Species",
                       labels = c("apis mellifera" = "Apis mellifera",
                                  "osmia" = "Osmia",
                                  "andrena" = "Andrena",
                                  "other" = "Other")) +
    scale_shape_manual(values = c("apis mellifera" = 16, 
                                  "osmia" = 17, 
                                  "andrena" = 15, 
                                  "other" = 18),
                       name = "Species",
                       labels = c("apis mellifera" = "Apis mellifera",
                                  "osmia" = "Osmia",
                                  "andrena" = "Andrena",
                                  "other" = "Other")) +
    labs(
      title = paste("Behavior:", str_to_title(behavior_name)),
      x = "Impervious Surface Proportion",
      y = "Proportion of Individuals",
      caption = if(exists("fig_caption")) fig_caption else NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
      plot.caption = element_text(size = 10, hjust = 0, color = "black", 
                                  margin = margin(t = 10)),
      plot.caption.position = "plot",
      strip.text = element_text(size = 12, face = "bold", 
                                margin = margin(t = 10, b = 10)),
      legend.position = "right",
      legend.title = element_text(size = 13, face = "bold"),
      legend.text = element_text(size = 10),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "grey90", size = 0.5),
      panel.grid.minor = element_line(color = "grey95", size = 0.3),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    guides(color = guide_legend(override.aes = list(size = 3.5),
                                title.position = "top",
                                title.hjust = 0.5,
                                ncol = 1),
           shape = guide_legend(override.aes = list(size = 3.5),
                                title.position = "top", 
                                title.hjust = 0.5,
                                ncol = 1)) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

  plot_list[[behavior_name]] <- p
}

# Print all generated plots through function
for(i in 1:length(plot_list)) {
  print(plot_list[[i]])
  if(i < length(plot_list)) cat("\n\n")
}