library(tidyverse)
library(rvest)
library(jsonlite)

get_player_data <- function(player, type = "all") {
  
if(is.double(player)){
  player <- paste0("https://understat.com/player/", as.character(player))
}

text <- read_html(player) %>%
  html_text()

text <- str_split(text, ",")[[1]]

shot_data <- text[which(str_detect(text, pattern = "shotsData")):which(str_detect(text, pattern = "\\\\x22\\\\x7D\\\\x5D'\\)"))[1]]

split <- unlist(str_split(shot_data, "JSON.parse\\('\\\\x5B|\\\\x5D'\\)"))

split <- split[- c(1, 2, length(split))]

gre <- gregexpr("\\\\x[0-9a-fA-F]{2}", split)
regm <- regmatches(split, gre)

for (i in seq_along(split)) {
  regmatches(split[i], gre[i]) <- list(sapply(as.raw(strtoi(substr(regm[[i]], 3, 4), 16L)), rawToChar))
}

df <- as_tibble(fromJSON(sprintf("[%s]", paste(read_lines(split), collapse=","))))

df[, c("id", "minute", "X", "Y", "xG",
       "player_id", "season", "match_id",
       "h_goals", "a_goals")] <- map(df[, c("id", "minute", "X", "Y", "xG",
                                            "player_id", "season", "match_id",
                                            "h_goals", "a_goals")], parse_number)

df[, "date"] <- map(df[, "date"], parse_datetime)

if(type == "goals"){
  df <- df %>%
    filter(result == "Goal")
}

return(df)

}



