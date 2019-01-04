library(tidyverse)
library(rvest)
library(jsonlite)

get_league_matches <- function(league_url) {

text <- read_html(league_url) %>%
  html_text()

text <- str_split(text, ",")[[1]]

match_data <- text[which(str_detect(text, pattern = "datesData")):which(str_detect(text, pattern = "\\\\x7D\\\\x5D'\\)"))[1]]

split <- unlist(str_split(match_data, "JSON.parse\\('\\\\x5B|\\\\x5D'\\)"))
split <- split[- c(1, length(split))]

gre <- gregexpr("\\\\x[0-9a-fA-F]{2}", split)
regm <- regmatches(split, gre)

for (i in seq_along(split)) {
  regmatches(split[i], gre[i]) <- list(sapply(as.raw(strtoi(substr(regm[[i]], 3, 4), 16L)), rawToChar))
}

df <- fromJSON(sprintf("[%s]", paste(read_lines(split), collapse=",")))

right_df <- bind_cols(df$h, df$a, df$goals, df$xG, df$forecast)
left_df <- select(df, id,  isResult, datetime)

matches_processed <- as.tibble(bind_cols(left_df, right_df)) %>%
  rename(h_id = id2,
         h_title = title,
         h_short_title = short_title,
         a_id = id1,
         a_title = title1,
         a_short_title = short_title1,
         h_goals = h,
         a_goals = a,
         h_xg = h1,
         a_xg = a1,
         w_forecast = w,
         d_forecast = d,
         l_forecast = l)
}
