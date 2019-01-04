library(tidyverse)
library(rvest)
library(jsonlite)

get_match_xG <- function(match) {
  
if(is.double(match)){
    match <- paste0("https://understat.com/match/", as.character(match))
}
  
text <- read_html(match) %>%
  html_text()

text <- str_split(text, ",")[[1]]

match_data <- text[which(str_detect(text, pattern = "shotsData")):which(str_detect(text, pattern = "x7D\\\\x5D\\\\x7D'\\)"))]

split <- unlist(str_split(match_data, "shotsData \\t= JSON.parse\\('|'\\)"))

split <- split[- c(1, length(split))]

gre <- gregexpr("\\\\x[0-9a-fA-F]{2}", split)
regm <- regmatches(split, gre)

for (i in seq_along(split)) {
  regmatches(split[i], gre[i]) <- list(sapply(as.raw(strtoi(substr(regm[[i]], 3, 4), 16L)), rawToChar))
}

df <- fromJSON(sprintf("[%s]", paste(read_lines(split), collapse=",")))
df <- df[1,1][[1]]  %>%
  bind_rows(df[1,2][[1]]) %>%
  as_tibble()



df[, c(1, 2, 4, 5, 6, 9, 11, 13, 16, 17)] <- map(df[, c(1, 2, 4, 5, 6, 9, 11, 13, 16, 17)], parse_number)
df[, "date"] <- map(df[, "date"], parse_datetime)

return(df)

}