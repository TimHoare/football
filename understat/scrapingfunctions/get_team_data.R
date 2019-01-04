library(tidyverse)
library(rvest)
library(jsonlite)

get_team_data <-function(team_url){

text <- read_html(team_url) %>%
  html_text()

text <- str_split(text, ",")[[1]]

team_data <- text[which(str_detect(text, pattern = "playersData")):which(str_detect(text, pattern = "\\\\x7D\\\\x5D"))[2]]

team_data <- unlist(str_split(team_data, "JSON.parse\\('\\\\x5B|\\\\x5D'\\);"))

team_data <- team_data[- c(1, length(team_data))]

gre <- gregexpr("\\\\x[0-9a-fA-F]{2}", team_data)
regm <- regmatches(team_data, gre)

for (i in seq_along(team_data)) {
  regmatches(team_data[i], gre[i]) <- list(sapply(as.raw(strtoi(substr(regm[[i]], 3, 4), 16L)), rawToChar))
}

df <- as_tibble(fromJSON(sprintf("[%s]", paste(read_lines(team_data), collapse=","))))
return(df)
}
