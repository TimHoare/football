library(tidyverse)

get_matches <- function(league = c("ENG", "ITA", "SPA", "GER", "FRA", "RUS"), season = 2018, type = c("all", "fixtures", "results")) {
  
  if(league == "ENG") {
    league <- "https://understat.com/league/EPL/"
  } else if(league == "ITA") {
    league <- "https://understat.com/league/Serie_A/"
  } else if(league == "SPA") {
    league <- "https://understat.com/league/La_liga/"
  } else if(league == "GER") {
    league <- "https://understat.com/league/Bundesliga/"
  } else if (league == "FRA") {
    league <- "https://understat.com/league/Ligue_1/"
  } else {
    league <- "https://understat.com/league/RFPL/"
  }
  
  league <- paste0(league, season)
  
  if(type[1] == "fixtures") {
    df <- get_league_matches(league) %>%
      filter(is.na(h_goals))
    
  } else if(type[1] == "results") {
    df <- get_league_matches(league) %>%
      filter(!is.na(h_goals))
    
  } else {
    df <- get_league_matches(league)
  }
    
  return(df)
}
