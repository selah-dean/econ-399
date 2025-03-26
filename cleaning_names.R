setwd("C:/Users/sgdea/ECON399")
library(tidyverse)
library(naniar)
library(scales)
library(gridExtra)
library(grid)
library(stringr)
library(stringi)
library(stringdist)

payroll_data <- read.csv("payroll_data_from_2015.csv") %>% select(-X)

nats_roster <- read.csv("nats_2024_roster.csv") %>% select(-X)

nats_payroll_data <- payroll_data %>% filter(team == "washington-nationals" & year==2024)

nats_roster <- nats_roster %>% 
  mutate(
    player_name_clean = stri_trans_general(Name, "Latin-ASCII")
    )

nats_payroll_data <- nats_payroll_data %>% 
  mutate(
    player_name_clean = stri_trans_general(player, "Latin-ASCII")
    )

roster <- nats_roster
payroll <- nats_payroll_data

# Function to match names
match_players <- function(roster, payroll) {
  
  # Step 1: Clean the names by removing accents
  roster <- roster %>% mutate(player_name_clean = stri_trans_general(Name, "Latin-ASCII"))
  payroll <- payroll %>% mutate(player_name_clean = stri_trans_general(player, "Latin-ASCII"))
  
  # Step 2: Initial exact match on the cleaned names
  exact_matches <- roster %>%
    inner_join(payroll, by = join_by(player_name_clean, year))
  
  # Step 3: Find unmatched rows in roster and payroll 
  unmatched_roster <- roster %>%
    anti_join(exact_matches, by = "player_name_clean")
  unmatched_payroll <- payroll %>%
    anti_join(exact_matches, by = "player_name_clean")
  
  # Step 4: Fuzzy matching for unmatched names in roster
  fuzzy_matches <- unmatched_roster %>%
    rowwise() %>%
    mutate(
      best_match_index = which.min(stringdist(player_name_clean, payroll$player_name_clean, method = "jw")),
      best_dist = min(stringdist(player_name_clean, payroll$player_name_clean, method = "jw"))
    ) %>%
    mutate(best_match_name = payroll$player_name_clean[best_match_index]) %>%
    left_join(payroll, by = join_by(best_match_name == player_name_clean, year))  
  
  # Step 5: Determine the remaining unmatched names in payroll 
  matched_names <- c(exact_matches$player, fuzzy_matches$player)
  unmatched_payroll <- unmatched_payroll %>%
    filter(!player %in% matched_names)
  
  # Step 6: Combine exact matches, fuzzy matches, and remaining players from payroll 
  # The remaining players are those who did not make a major league appearance for the team
  final_result <- exact_matches %>%
    bind_rows(fuzzy_matches %>% select(-best_match_index, -best_dist, -best_match_name)) %>%
    bind_rows(unmatched_payroll)
  
  return(final_result)
}


# Run the function
result <- match_players(nats_roster, nats_payroll_data)


