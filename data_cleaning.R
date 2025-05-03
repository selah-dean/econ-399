library(tidyverse)
library(stringr)
library(stringi)

# Load datasets
payroll_data <- read.csv("datasets/payroll_data_from_2015.csv") %>% select(-X)
payroll_ranks <- read.csv("datasets/payroll_ranks_from_2015.csv") %>% select(-X)
forty_man <- read.csv("datasets/bb_ref_forty_man_roster_from_2015.csv") %>% select(-X)
standings <- read.csv("datasets/standings_from_2012.csv") %>% select(-X) %>% filter(Tm != "Average")
team_league_map <- read.csv("datasets/team_league_map.csv")

# Join standings data with team_league_map
standings <- inner_join(standings, team_league_map, by = join_by(Tm == team))

# Join forty man data with team_league_map
# Only include the newest names to match spotrac
forty_man <- inner_join(
  forty_man,
  team_league_map %>%
    filter(team != "Cleveland Indians" &
             team != "Los Angeles Angels of Anaheim"),
  by = join_by(team == team_id)
) %>%
  rename(team_id = team, team = team.y)

# modify the format of the team name to match with spotrac
team_league_map <- team_league_map %>% mutate(
  team = str_to_lower(team) %>%
    str_replace_all(" ", "-") %>%
    str_replace_all("\\.", "") %>%
    str_replace_all("-of-anaheim", "") %>%
    str_replace_all("indians", "guardians") %>%
    str_replace_all("oakland-", "")
)  %>% distinct()

# Join payroll data on team_league_map
payroll_ranks <- inner_join(payroll_ranks, team_league_map, by = join_by(team))
payroll_data <- inner_join(payroll_data, team_league_map, by = join_by(team))

# Convert payroll columns to numeric
clean_currency <- function(x)
  as.numeric(str_remove_all(x, "[\\$,]"))
payroll_ranks <- payroll_ranks %>% mutate(amount = clean_currency(amount), rank = as.numeric(str_remove_all(rank, "[a-zA-Z]")))

# Clean payroll rank so amount and rank are numeric
payroll_ranks <- payroll_ranks %>% mutate(amount = as.numeric(gsub("[\\$,]", "", amount)), rank = as.numeric(gsub("[a-zA-Z]", "", rank)))
payroll_data <- payroll_data %>%
  mutate(across(
    c(
      payroll_salary,
      payroll_salary_adjusted,
      base_salary,
      signing_bonus,
      incentives_likely,
      incentives_unlikely
    ),
    ~ as.numeric(gsub("[\\$,]", "", na_if(., "-")))
  )) %>%
  mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
  mutate(roster = str_remove(roster, "^\\d{4} ") %>%
           str_remove(" Payroll$"))

# Determine total payroll
total_payroll <- payroll_ranks %>% filter(player_type == "Total Adjusted Payroll Allocations")

# Clean 40-man roster data
forty_man <- forty_man %>%
  rename(all_star = X.1) %>%
  mutate(
    all_star = if_else(all_star == "All-Star", TRUE, FALSE),
    Yrs = as.numeric(str_remove_all(Yrs, "[a-zA-Z]"))
  )

# Add win variability to standings
determine_win_variability <- function(years, wins, current_year) {
  valid_years <- years[years <= current_year & years != 2020]
  if (length(valid_years) < 3)
    return(NA)
  sd(wins[years %in% tail(valid_years, 3)])
}

# Add attribute for win variability
standings <- standings  %>%
  arrange(team_id, year) %>%  # Sort by team and year
  group_by(team_id) %>%       # Group by team
  mutate(win_variability = map_dbl(year, ~ determine_win_variability(year, W, .x))) %>%
  ungroup()

# Assign player positions
forty_man <- forty_man %>%
  mutate(
    position = apply(select(., P:DH) %>% select(-OF), 1, function(row)
      names(row)[which.max(row)]),
    position = case_when(
      position == "P" & GS >= 0.8 * G ~ "SP",
      position == "P" ~ "RP",
      TRUE ~ position
    ),
    position = str_remove_all(position, "X"),
    position = if_else(Name == "Shohei Ohtani", "SP/DH", position),
    pos_simple = case_when(position %in% c("RP", "SP") ~ "pitcher", TRUE ~ "hitter")
  )

payroll_data <- payroll_data %>%
  mutate(pos_simple = case_when(
    pos == "SP/DH" ~ "two-way",
    pos %in% c("RP", "SP", "SP1", "P", "CL") ~ "pitcher",
    TRUE ~ "hitter"
  ))

combine_duplicates <- function(x) {
  if (all(is.na(x))) {
    return(NA)  # return NA if all values are NA
  } else if (is.numeric(x)) {
    return(sum(x, na.rm = TRUE))  # sum any numeric values
  } else if (is.character(x)) {
    return(paste(unique(na.omit(x)), collapse = "/"))  # concat diff values
  } else {
    return(unique(na.omit(x)))  # keep the value if the same between rows
  }
}

payroll_data <- payroll_data %>%
  group_by(player, team, year, pos_simple) %>%
  summarize(across(everything(), combine_duplicates), .groups = "drop")

# Function to clean player names
clean_name <- function(Name) {
  Name <- stri_trans_general(Name, "Latin-ASCII")
  
  # Remove suffixes
  suffixes <- c("Jr.", "Sr.", "II", "III", "IV", "V", "HOF")
  suffix_pattern <- paste0("\\s*(", paste(suffixes, collapse = "|"), ")\\s*$")
  Name <- stri_replace_all_regex(Name, suffix_pattern, "")
  
  # Remove periods and dashes
  Name <- stri_replace_all_regex(Name, "[.]", "")
  Name <- stri_replace_all_regex(Name, "[-]", " ")
  
  # Convert to title case
  Name <- str_to_title(Name)
  Name <- gsub('[\t\n]', '', Name)
  
  return(Name)
}

# Function to extract last name
extract_last_name <- function(full_name) {
  name_parts <- str_split(full_name, " ")[[1]]
  tail(name_parts, 1)
}


match_players <- function(roster, payroll) {
  # Clean the names by removing accents and create column for last name
  roster <- roster %>% mutate(
    player_name_clean = sapply(Name, clean_name),
    last_name = sapply(player_name_clean, extract_last_name)
  )
  
  payroll <- payroll %>% mutate(
    player_name_clean = sapply(player, clean_name),
    last_name = sapply(player_name_clean, extract_last_name)
  )
  
  # Exact match on cleaned names
  exact_matches <- roster %>%
    inner_join(
      payroll,
      by =
        join_by(player_name_clean, team_id, year, pos_simple),
      suffix = c("", "")
    )
  
  print("First step matches:")
  print(nrow(exact_matches)/nrow(roster))
  
  # Identify unmatched rows
  unmatched_roster <- roster %>%
    anti_join(exact_matches,
              by = join_by(player_name_clean, team_id, year, pos_simple))
  
  unmatched_payroll <- payroll %>%
    anti_join(exact_matches,
              by = join_by(player_name_clean, team_id, year, pos_simple))
  
  # Fuzzy matching based on last name
  last_name_matches <- unmatched_roster %>%
    inner_join(
      unmatched_payroll,
      by = join_by(last_name, team_id, year, pos_simple),
      suffix = c("", "")
    )
  
  # Combine exact and last name matches
  combined_matches <- exact_matches %>%
    bind_rows(last_name_matches)
  
  print("Second step matches:")
  print(nrow(combined_matches)/nrow(roster))
  
  # Identify remaining unmatched rows
  unmatched_roster_2 <- unmatched_roster %>%
    anti_join(combined_matches, by = join_by(Name, team_id, year, pos_simple))
  
  unmatched_payroll_2 <- unmatched_payroll %>%
    anti_join(combined_matches, by = join_by(player, team_id, year, pos_simple))
  
  # Match remaining players ignoring positions
  wo_position_matches <- unmatched_roster_2 %>%
    inner_join(
      unmatched_payroll_2,
      by =
        join_by(player_name_clean, team_id, year),
      suffix = c("_bbref", "_spotrac")
    ) %>%
    rename(team = team_spotrac) %>%
    select(-contains("_spotrac"), -team_bbref) %>%
    rename_with(~ str_replace(.x, "_bbref$", ""), ends_with("_bbref"))
  
  # Combine all matches
  combined_matches_2 <- combined_matches %>%
    bind_rows(wo_position_matches)
  
  print("Third step matches:")
  print(nrow(combined_matches_2)/nrow(roster))
  
  # Final unmatched players
  unmatched_roster_3 <- unmatched_roster_2 %>%
    anti_join(combined_matches_2, by = join_by(Name, team_id, year, pos_simple))
  
  # Final result including remaining unmatched roster playears
  final_result <- bind_rows(combined_matches_2, unmatched_roster_3)
  
  return(final_result)
}

roster_payroll_data <- match_players(forty_man, payroll_data)

# Impute missing values for player's status
determine_status <- function(roster_payroll_data) {
  # Case 1: Has exp value
  # Case 2: Missing exp level but in one of the listed roster designations
  roster_payroll_data <- roster_payroll_data %>%
    mutate(status = case_when(
      (is.na(status) & !is.na(exp)) ~ ifelse(
        exp <= 3,
        "Pre-Arbitration",
        ifelse(exp > 3 &
                 exp <= 6, "Arbitration", "Veteran")
      ),
      (
        is.na(status) & is.na(exp) &
          roster %in%
          c("Minor", "Active Roster", "Injured List", "Retained/Minor")
      ) ~ "Pre-Arbitration",
      TRUE ~ status
    ))
  
  # Case 3: Find player in the dataset for a different team from the same year
  copy <- roster_payroll_data %>% select(player, DoB, year, exp, status) %>%
    filter(!is.na(status)) %>% distinct()
  
  status_lookup <- roster_payroll_data %>% filter(is.na(status) &
                                                    is.na(exp)) %>%
    left_join(copy,
              by = join_by(player, DoB, year),
              suffix = c("", "_matched")) %>%
    mutate(
      exp = ifelse(is.na(exp), exp_matched, exp),
      status = ifelse(is.na(status), status_matched, status)
    ) %>%
    select(-contains("_matched")) %>% distinct()
  
  roster_payroll_data <- bind_rows(roster_payroll_data %>% filter(!(is.na(status) &
                                                                      is.na(exp))), status_lookup)
  
  # Case 4: Find player in the dataset from the previous year
  copy <- roster_payroll_data %>% select(player, DoB, year, exp, status) %>%
    filter(!is.na(status)) %>% distinct()
  
  to_join <- roster_payroll_data %>% filter(is.na(status) &
                                              roster == "Retained") %>% mutate(prev_year = year - 1)
  
  by_prev <- to_join %>%
    left_join(
      copy,
      by = join_by(player, DoB, prev_year == year),
      suffix = c("", "_matched")
    ) %>%
    mutate(
      prev_exp = ifelse(is.na(exp), exp_matched, exp),
      prev_status = ifelse(is.na(status), status_matched, status),
      status = case_when(
        (prev_status == "Veteran" | prev_exp >= 5) ~ "Veteran",
        (prev_status == "Pre-Arbitration" &
           (is.na(prev_exp) |
              prev_exp < 2)) ~ "Pre-Arbitration",
        (is.na(prev_status) & is.na(prev_exp)) ~
          ifelse(
            Yrs <= 3,
            "Pre-Arbitration",
            ifelse(Yrs >= 8, "Veteran", "Arbitration")
          ),
        TRUE ~ "Arbitration"
      )
    ) %>%
    select(-contains("_matched")) %>% distinct()
  
  roster_payroll_data <- bind_rows(
    roster_payroll_data %>% filter(!(is.na(status) &
                                       roster == "Retained")),
    roster_payroll_data %>% filter(is.na(player)),
    by_prev
  )
  
}

roster_payroll_data <- determine_status(roster_payroll_data)

# Simplify status to be either Pre-Arbitration, Arbitration, or Veteran
roster_payroll_data <- roster_payroll_data %>%
  mutate(
    status_simple = case_when(
      grepl("Pre-Arbitration", status) ~ "Pre-Arbitration",
      (grepl("Veteran", status) |
         status == "Club Exercised") ~ "Veteran",
      (grepl("Arbitration", status) |
         status == "Arb Avoided") ~ "Arbitration",
      status == "minor-buried" ~ ifelse(exp < 3, "Pre-Arbitration", "Arbitration"),
      exp < 3 ~ "Pre-Arbitration",
      exp >= 3 & exp < 6 ~ "Arbitration",
      exp >= 6 ~ "Veteran",
      TRUE ~ status
    )
  )

# Combine payroll and standings data
payroll_with_standings <- inner_join(
  total_payroll, standings, 
  by = join_by(team_id, year, league, division, market_size)
) %>% distinct()



# Saved cleaned files for modeling 
write.csv(roster_payroll_data, "C:/Users/sgdea/ECON399/datasets/cleaned/roster_payroll_data.csv", row.names = FALSE)
write.csv(payroll_with_standings, "C:/Users/sgdea/ECON399/datasets/cleaned/payroll_with_standings.csv", row.names = FALSE)
