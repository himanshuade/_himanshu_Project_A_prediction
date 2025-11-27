library(tidyverse)
library(dplyr)
library(inspectdf)

tennis_0 <- read_csv("./data/atp_matches_2000.csv")
tennis_1 <- read_csv("./data/atp_matches_2001.csv")
tennis_2 <- read_csv("./data/atp_matches_2002.csv")
tennis_3 <- read_csv("./data/atp_matches_2003.csv")
tennis_4 <- read_csv("./data/atp_matches_2004.csv")
tennis_5 <- read_csv("./data/atp_matches_2005.csv")
tennis_6 <- read_csv("./data/atp_matches_2006.csv")
tennis_7 <- read_csv("./data/atp_matches_2007.csv")
tennis_8 <- read_csv("./data/atp_matches_2008.csv")
tennis_9 <- read_csv("./data/atp_matches_2009.csv")
tennis_10 <- read_csv("./data/atp_matches_2010.csv")
tennis_11 <- read_csv("./data/atp_matches_2011.csv")
tennis_12 <- read_csv("./data/atp_matches_2012.csv")
tennis_13 <- read_csv("./data/atp_matches_2013.csv")
tennis_14 <- read_csv("./data/atp_matches_2014.csv")
tennis_15 <- read_csv("./data/atp_matches_2015.csv")
tennis_16 <- read_csv("./data/atp_matches_2016.csv")
tennis_17 <- read_csv("./data/atp_matches_2017.csv")
tennis_18 <- read_csv("./data/atp_matches_2018.csv")
tennis_19 <- read_csv("./data/atp_matches_2019.csv")

combined <- bind_rows(tennis_0, tennis_1, tennis_2,tennis_3,tennis_4
                       ,tennis_5,tennis_6,tennis_7,
                       tennis_8,tennis_9,tennis_10,tennis_11,tennis_12,tennis_13,
                      tennis_14,tennis_15,tennis_16,tennis_17,tennis_18,tennis_19)

t_data <- combined %>% 
  select(winner_name, loser_name, winner_rank, loser_rank,
         winner_rank_points, loser_rank_points, surface, score, tourney_date,tourney_level) %>%
  mutate(year = as.numeric(substr(tourney_date, 1, 4)))
  


inspect_na(t_data)

t_data <- drop_na(t_data)

inspect_na(t_data)

filter(t_data, winner_rank < 0, loser_rank < 0 , score < 0)


t_data <- t_data %>%
  mutate(rank_point_diff = winner_rank_points - loser_rank_points) # calulation the ranking point difference between the winning and losing player.

#adding output colomn 1 where the higher ranked player is the winner and 0 where higher ranked player lost the match.
t_data <- t_data %>%
  mutate(outcome = ifelse(winner_rank < loser_rank, 1, 0)) %>% relocate(outcome,.before = winner_name)


t_data <- t_data %>% mutate( outcome = as.factor(outcome),
                                       winner_rank = as.integer(winner_rank),
                                       loser_rank = as.integer(loser_rank),
                                       winner_rank_points = as.integer(winner_rank_points),
                                       loser_rank_points = as.integer(loser_rank_points),
                                       surface = as.factor(surface),
                                       rank_point_diff = as.integer(rank_point_diff),
                                       tourney_level = as.factor(tourney_level))


colnames(t_data)


write_csv(t_data, "data/test_clean.csv")


ggsave("matches_per_year.png", width = 8, height = 5, dpi = 300)


ggplot(elo_data, aes(x = surface)) +
  geom_bar(fill = "#6A5ACD") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Match Count by Surface Type",
    x = "Surface",
    y = "Match Count"
  )

ggsave("matches_by_surface.png", width = 7, height = 5, dpi = 300)




ggplot(t_data, aes(x = year)) +
  geom_bar(fill = "#4B9CD3") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Number of ATP Matches per Year (2000–2019)",
    x = "Year",
    y = "Match Count"
  )

