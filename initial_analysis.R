setwd("C:/Users/sgdea/ECON399")
library(tidyverse)
library(naniar)
library(scales)
library(gridExtra)
library(grid)
library(stringr)


payroll_data <- read.csv("payroll_data_from_2015.csv") %>% select(-X)
payroll_ranks <- read.csv("payroll_ranks_from_2015.csv") %>% select(-X)


forty_man <- read.csv("bb_ref_forty_man_roster_from_2015.csv") %>% select(-X)

standings <- read.csv("standings_from_2015.csv") %>% select(-X) %>%
  filter(Tm != "Average")

team_league_map <- read.csv("team_league_map.csv")

team_league_map_no_duplicates <- team_league_map %>% filter(team != "Cleveland Indians" & team != "Los Angeles Angels of Anaheim")

forty_man <- inner_join(forty_man, team_league_map_no_duplicates, 
                        by=join_by(team == abbreviation)) %>%
  rename(
    abbreviation = team, 
    team = team.y
  )

diff_rows <- anti_join(forty_man_with_league, forty_man)
standings_with_league <- merge(standings, team_league_map, by.x="Tm", by.y="team")

team_league_map <- team_league_map %>% mutate(
  team = str_to_lower(team) %>% 
    gsub(" ", "-", .) %>% 
    gsub("\\.", "", .) %>%
    gsub("-of-anaheim", "", .) %>%
    gsub("indians", "guardians", .) %>%
    gsub("oakland-", "", .)
)  %>% distinct()

total_payroll <- payroll_ranks %>% 
  filter(player_type == "Total Adjusted Payroll Allocations") %>%
  mutate(
    amount = as.numeric(gsub("[\\$,]", "", amount)), 
    rank = as.numeric(gsub("[a-zA-Z]", "", rank))
  ) %>% merge(., team_league_map, by="team")

payroll_data <- merge(payroll_data, team_league_map, by="team")

standings <- standings %>% mutate(
  Tm = str_to_lower(Tm) %>% 
    gsub(" ", "-", .) %>% 
    gsub("\\.", "", .) %>%
    gsub("-of-anaheim", "", .) %>%
    gsub("indians", "guardians", .) %>%
    gsub("oakland-", "", .)
)

payroll_with_standings <- merge(
  total_payroll, standings, 
  by.x = c("team", "year"), 
  by.y = c("Tm", "year")
)

# Payroll Rank vs Regular Season Rank
ggplot(payroll_with_standings, aes(x=rank, y=Rk)) + 
  geom_point() + 
  xlab("Payroll Rank") + ylab("Regular Season Rank") + 
  xlim(1, 30) + ylim(1,30) + 
  theme_bw() + ggtitle("Payroll vs Team Performance (Regular Season Rank)") + 
  theme(plot.title = element_text(hjust = 0.5))

# Payroll Rank vs Win Percentage
ggplot(payroll_with_standings, aes(x=rank, y=W.L.)) + 
  geom_point() + 
  xlab("Payroll Rank") + ylab("Win Percentage") + 
  xlim(1, 30) + 
  theme_bw() + ggtitle("Payroll vs Team Performance (Win Percentage)") + 
  theme(plot.title = element_text(hjust = 0.5))

# Payroll Amount vs Win Percentage
ggplot(payroll_with_standings, aes(x=amount, y=W.L.)) + 
  geom_point() + 
  xlab("Payroll Rank") + ylab("Win Percentage") + 
  #xlim(1, 30) + 
  theme_bw() + ggtitle("Payroll vs Team Performance (Win Percentage)") + 
  theme(plot.title = element_text(hjust = 0.5))

# Payroll Rank vs Regular Season Rank split by Season 
ggplot(payroll_with_standings, aes(x=rank, y=Rk)) + 
  geom_point() + 
  xlab("Payroll Rank") + ylab("Regular Season Rank") + 
  xlim(1, 30) + ylim(1,30) + 
  theme_bw() + facet_wrap(vars(year), ncol=5) + 
  ggtitle("Payroll vs Team Performance (Regular Season Rank) by Season") + 
  theme(plot.title = element_text(hjust = 0.5))

# Payroll Amount vs Regular Season Rank split by Season 
ggplot(payroll_with_standings, aes(x=amount, y=W.L.)) + 
  geom_point() + 
  xlab("Payroll Rank") + ylab("Regular Season Rank") + 
  scale_x_continuous(labels = label_dollar(scale = 1/1e6, suffix = "M"), guide = guide_axis(angle = 45)) + 
  theme_bw() + facet_wrap(vars(year), ncol=5) + 
  ggtitle("Payroll vs Team Performance (Regular Season Rank) by Season") + 
  theme(plot.title = element_text(hjust = 0.5))

# Payroll Rank vs Win Percentage split by Season 
ggplot(payroll_with_standings, aes(x=rank, y=W.L.)) + 
  geom_point() + 
  xlab("Payroll Rank") + ylab("Win Percentage") + 
  xlim(1, 30) + 
  theme_bw() + facet_wrap(vars(year), ncol=5) + 
  ggtitle("Payroll vs Team Performance (Win Percentage) by Season") + 
  theme(plot.title = element_text(hjust = 0.5))


team_payroll_by_year <- total_payroll %>% group_by(year) %>%
  summarize(
    avg_team_payroll = mean(amount), 
    max_team_payroll = max(amount), 
    min_team_payroll = min(amount)
  )

team_payroll_by_year_by_league <- total_payroll %>% group_by(league, year) %>%
  summarize(
    avg_team_payroll = mean(amount), 
    max_team_payroll = max(amount), 
    min_team_payroll = min(amount)
  )

# Avg, Max, Min League Payroll by Season 
ggplot(team_payroll_by_year, aes(x = factor(year))) + 
  geom_point(aes(y = avg_team_payroll, color = "Average Payroll")) + 
  geom_line(aes(y = avg_team_payroll, color = "Average Payroll", group = 1)) + 
  geom_point(aes(y = max_team_payroll, color = "Max Payroll")) + 
  geom_line(aes(y = max_team_payroll, color = "Max Payroll", group = 1)) + 
  geom_point(aes(y = min_team_payroll, color = "Min Payroll")) + 
  geom_line(aes(y = min_team_payroll, color = "Min Payroll", group = 1)) + 
  scale_color_manual(values = c("Average Payroll" = "blue", 
                                "Max Payroll" = "red", 
                                "Min Payroll" = "green")) +
  scale_y_continuous(labels = label_dollar(scale = 1/1e6, suffix = "M")) + 
  theme_bw() + 
  xlab("Season") + 
  ylab("Team Payroll") + 
  labs(color = "Payroll Type") + 
  ggtitle("League Payroll by Season") + 
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(team_payroll_by_year_by_league, aes(x = factor(year))) + 
  geom_point(aes(y = avg_team_payroll, color = "Average Payroll")) + 
  geom_line(aes(y = avg_team_payroll, color = "Average Payroll", group = 1)) + 
  geom_point(aes(y = max_team_payroll, color = "Max Payroll")) + 
  geom_line(aes(y = max_team_payroll, color = "Max Payroll", group = 1)) + 
  geom_point(aes(y = min_team_payroll, color = "Min Payroll")) + 
  geom_line(aes(y = min_team_payroll, color = "Min Payroll", group = 1)) + 
  scale_color_manual(values = c("Average Payroll" = "blue", 
                                "Max Payroll" = "red", 
                                "Min Payroll" = "green")) +
  scale_y_continuous(labels = label_dollar(scale = 1/1e6, suffix = "M")) + 
  theme_bw() + 
  xlab("Season") + 
  ylab("Team Payroll") + 
  labs(color = "Payroll Type") + 
  ggtitle("League Payroll by Season") + 
  theme(plot.title = element_text(hjust = 0.5)) + facet_wrap(vars(league), ncol=2)

team_payroll_by_year_by_league_long <- team_payroll_by_year_by_league %>%
  pivot_longer(
    cols = c(avg_team_payroll, max_team_payroll, min_team_payroll), # Columns to convert
    names_to = "payroll_type",  # New column for variable names
    values_to = "payroll_value" # New column for values
  )

ggplot(team_payroll_by_year_by_league_long, aes(x = factor(year), group = interaction(league, payroll_type))) +
  geom_point(aes(y = payroll_value, color = payroll_type)) +
  geom_line(aes(y = payroll_value, color = payroll_type, linetype = league)) +
  scale_color_brewer(palette = "Set1") +  # Use color for league distinction
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) + # Different line types for avg, max, min
  scale_y_continuous(labels = scales::label_dollar(scale = 1/1e6, suffix = "M")) +
  theme_bw() +
  xlab("Season") +
  ylab("Team Payroll") +
  labs(color = "", linetype = "League") +
  ggtitle("League Payroll by Season") +
  theme(plot.title = element_text(hjust = 0.5))


top_payroll <- payroll_data %>% 
  replace_with_na(replace = list(payroll_salary = "-")) %>%
  mutate(
    payroll_salary = as.numeric(gsub("[\\$,]", "", payroll_salary))
  ) %>% filter(grepl("Active Roster Payroll", roster) | grepl("Injured List Payroll", roster)) %>%
  group_by(team, league, year) %>%
  summarize(
    max_salary = max(payroll_salary, na.rm = TRUE),
    min_salary = min(payroll_salary, na.rm = TRUE),
    avg_salary = mean(payroll_salary, na.rm = TRUE)
  )

highest_paid_player <- payroll_data %>%
  replace_with_na(replace = list(payroll_salary = "-")) %>%
  mutate(
    payroll_salary = as.numeric(gsub("[\\$,]", "", payroll_salary))
  ) %>% group_by(year) %>%
  slice_max(payroll_salary)

highest_paid_player_by_pos <- payroll_data %>%
  replace_with_na(replace = list(payroll_salary = "-")) %>%
  mutate(
    payroll_salary = as.numeric(gsub("[\\$,]", "", payroll_salary)), 
    pos = case_when(
      pos %in% c("1B", "2B", "3B", "SS") ~ "INF", 
      pos %in% c("LF", "RF", "CF") ~ "OF", 
      pos %in% c("RP", "CL","P") ~ "RP", 
      pos %in% c("SP", "SP1") ~ "SP", 
      pos == "DH/DH" ~ "SP/DH", 
      TRUE ~ pos 
    )
  ) %>% group_by(year, pos) %>%
  slice_max(payroll_salary)

payroll_by_pos <- payroll_data %>%
  replace_with_na(replace = list(payroll_salary = "-")) %>%
  mutate(
    payroll_salary = as.numeric(gsub("[\\$,]", "", payroll_salary)), 
    pos = case_when(
      pos %in% c("1B", "2B", "3B", "SS") ~ "INF", 
      pos %in% c("LF", "RF", "CF") ~ "OF", 
      pos %in% c("RP", "CL","P") ~ "RP", 
      pos %in% c("SP", "SP1") ~ "SP", 
      pos == "DH/DH" ~ "SP/DH", 
      TRUE ~ pos 
    )
  ) %>% filter(grepl("Active Roster Payroll", roster) | grepl("Injured List Payroll", roster)) %>%
  group_by(year, pos) %>%
  summarize(
    max_salary = max(payroll_salary, na.rm = TRUE),
    min_salary = min(payroll_salary, na.rm = TRUE),
    avg_salary = mean(payroll_salary, na.rm = TRUE), 
    median_salary = median(payroll_salary, na.rm = TRUE), 
    num_players = n()
  )

payroll_by_pos_by_league <- payroll_data %>%
  replace_with_na(replace = list(payroll_salary = "-")) %>%
  mutate(
    payroll_salary = as.numeric(gsub("[\\$,]", "", payroll_salary)), 
    pos = case_when(
      pos %in% c("1B", "2B", "3B", "SS") ~ "INF", 
      pos %in% c("LF", "RF", "CF") ~ "OF", 
      pos %in% c("RP", "CL","P") ~ "RP", 
      pos %in% c("SP", "SP1") ~ "SP", 
      pos == "DH/DH" ~ "SP/DH", 
      TRUE ~ pos 
    )
  ) %>% filter(grepl("Active Roster Payroll", roster) | grepl("Injured List Payroll", roster)) %>%
  group_by(league, year, pos) %>%
  summarize(
    max_salary = max(payroll_salary, na.rm = TRUE),
    min_salary = min(payroll_salary, na.rm = TRUE),
    avg_salary = mean(payroll_salary, na.rm = TRUE), 
    median_salary = median(payroll_salary, na.rm = TRUE), 
    num_players = n()
  )

payroll_by_pos_fielding <- payroll_by_pos %>% filter((pos != "SP/DH") & (pos != "DH"))

ggplot(payroll_by_pos_fielding, aes(x = factor(year))) + 
  geom_point(aes(y = avg_salary, color = factor(pos))) + 
  geom_line(aes(y = avg_salary, color = factor(pos), group = factor(pos))) +
  scale_y_continuous(labels = label_dollar(scale = 1/1e6, suffix = "M")) + 
  theme_bw() + 
  xlab("Season") + 
  ylab("Average Salary") + 
  labs(color = "Position") + 
  ggtitle("Average Salary by Season") + 
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(payroll_by_pos_fielding, aes(x = factor(year))) + 
  geom_point(aes(y = max_salary, color = factor(pos))) + 
  geom_line(aes(y = max_salary, color = factor(pos), group = factor(pos))) +
  scale_y_continuous(labels = label_dollar()) + 
  theme_bw() + 
  xlab("Season") + 
  ylab("Highest Salary") + 
  labs(color = "Position") + 
  ggtitle("Highest Salary by Season") + 
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(payroll_by_pos  %>% filter(pos != "SP/DH"), aes(x = factor(year))) + 
  geom_point(aes(y = avg_salary, color = factor(pos))) + 
  geom_line(aes(y = avg_salary, color = factor(pos), group = factor(pos))) +
  scale_y_continuous(labels = label_dollar()) + 
  theme_bw() + 
  xlab("Season") + 
  ylab("Average Salary") + 
  labs(color = "Position") + 
  ggtitle("Average Salary by Season") + 
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(payroll_by_pos_by_league  %>% filter(pos != "SP/DH" & pos!="DH"), aes(x = factor(year))) + 
  geom_point(aes(y = avg_salary, color = pos, shape=league)) + 
  geom_line(aes(y = avg_salary, group = interaction(league, pos), color = pos, linetype = league)) +
  scale_y_continuous(labels = label_dollar(scale = 1/1e6, suffix = "M")) + 
  theme_bw() + 
  xlab("Season") + 
  ylab("Average Salary") + 
  labs(color = "Position", linetype="League") + 
  ggtitle("Average Salary by Season") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(payroll_by_pos_by_league  %>% filter(pos != "SP/DH" & pos!="DH"), aes(x = factor(year))) + 
  geom_point(aes(y = max_salary, color = pos, shape=league)) + 
  geom_line(aes(y = max_salary, group = interaction(league, pos), color = pos, linetype = league)) +
  scale_y_continuous(labels = label_dollar(scale = 1/1e6, suffix = "M")) + 
  theme_bw() + 
  xlab("Season") + 
  ylab("Max Salary") + 
  labs(color = "Position", shape = "League", linetype="League") + 
  ggtitle("Max Salary by Season") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(payroll_by_pos %>% filter(pos != "SP/DH"), aes(x = factor(year))) + 
  geom_point(aes(y = max_salary, color = factor(pos))) + 
  geom_line(aes(y = max_salary, color = factor(pos), group = factor(pos))) +
  scale_y_continuous(labels = label_dollar()) + 
  theme_bw() + 
  xlab("Season") + 
  ylab("Highest Salary") + 
  labs(color = "Position") + 
  ggtitle("Highest Salary by Season") + 
  theme(plot.title = element_text(hjust = 0.5)) 


highest_paid_player_by_pos_to_display <- highest_paid_player_by_pos %>%
  select(player, pos, payroll_salary, team, year) %>%
  group_by(year, pos, payroll_salary) %>% 
  reframe(
    player = paste(player, collapse = "/"), 
    team = paste(team, collapse = "/")
  ) %>% mutate(
    payroll_salary = dollar(payroll_salary), 
    team = gsub("-", " ", team) %>% str_to_title(), 
  ) %>% 
  group_by(year) %>%
  arrange(pos) %>%
  group_split() 

years <- sapply(highest_paid_player_by_pos_to_display, function(df) unique(df$year))

# Convert each dataframe to a tableGrob and add a title
table_grobs <- mapply(
  function(tbl, yr) {
    tbl <- tbl %>% select(-year)  # Remove year column
    colnames(tbl) <- c("Position", "Salary", "Player", "Team")
    table <- tableGrob(tbl)
    title <- textGrob(yr, gp = gpar(fontsize = 14, fontface = "bold"))
    arrangeGrob(title, table, ncol = 1)
  },
  highest_paid_player_by_pos_to_display, years, SIMPLIFY = FALSE
)

grid.newpage()  # Clear the plot window
grid.draw(table_grobs[[1]])

grid.arrange(grobs = table_grobs, ncol = 2)

payroll_data_simple <- payroll_data %>% select(player, team, league, year) %>% 
  rename(
    name = player 
  )

forty_man_simple <- forty_man %>% select(Name, team, league, year) %>%
  rename(
    name = Name, 
  ) %>% mutate(
    team = str_to_lower(team) %>% 
      gsub(" ", "-", .) %>% 
      gsub("\\.", "", .) %>%
      gsub("-of-anaheim", "", .) %>%
      gsub("indians", "guardians", .) %>%
      gsub("oakland-", "", .)
  )

test <- anti_join(forty_man_simple, payroll_data_simple)
