# Creating a PassSonar plot in R using data from WhoScored


## Getting the data 

WhoScored provide an interactive match centre for a large number of football matches, giving us regular folk the ability to look at
heatmaps, shot locations, touch maps etc.

![](https://github.com/TimHoare/football/blob/master/visualisations/PassSonar/images/whoscored_match_centre.png)

If we look at the page source, we can see that the raw match event information is stored as JSON in a a JavaScript variable 
called 'matchCentreData'.

The events themselves are in an array named 'events' (shock) and with some scraping or copy and pasting, we can format this nicely
in a text editor to get a feel for the data:

![](https://github.com/TimHoare/football/blob/master/visualisations/PassSonar/images/whoscored_json.png)

I've now saved everthing in that variable (i.e. before var matchCentreEventTypeJson and the semicolon before it) to file so I can
import it into R. 

The first step is to read the file in as a string:

```r
json <- readChar("{your file name}.json",
                 nchars = file.info("{your file name}.json")$size)
```
Then turn the string into a list using the `fromJSON` function from the `jsonlite` package.

```r
library(jsonlite)

list <- fromJSON(json, flatten = TRUE) 
```
![](https://github.com/TimHoare/football/blob/master/visualisations/PassSonar/images/json_list.png)

We can now extract individual elements from the list, for example the events array (now a dataframe):

```r
library(tidyverse)

events <- list[["events"]] %>%
  janitor::clean_names() %>%
  tbl_df() # I prefer snake case
```
Next we need to clean up the `qualifiers` column, which is a list of dataframes. This is a pain to do, but I have written a function 
for this, which I won't dwell on for too long:

```r
clean_qualifiers <- function(df) {

  qualifiers <- df %>%
    select(id, qualifiers) %>% # Select only the qualifiers column and the uid for the event
    unnest() %>% # Unnest the qualifiers column
    janitor::clean_names() %>%
    distinct() # Remove any duplicates
    
  # Split qualifiers into logicals (key-pass, longball etc.) and non-logicals (length, angle etc.)
  
  logicals <- qualifiers %>%
    filter(is.na(value)) %>%
    distinct(type_display_name) %>%
    pull()
  
 # Spread the logicals
  spreaded_nlog <- qualifiers %>% 
    filter(!type_display_name %in% logicals) %>%
    select(-type_value) %>%
    spread(type_display_name, value)
 
 # Repeat for non logicals however make sure the entry is true rather than just NA
  spreaded_log <- qualifiers %>%
    filter(type_display_name %in% logicals) %>%
    select(-type_value) %>%
    mutate(value = if_else(is.na(value), TRUE, NA),
           value = ifelse(is.na(value), 243, value)) %>% # Replace value for fouls which should be 243
    spread(type_display_name, value)
  
  # Join both back onto the original df
  ndf <- df %>%
    left_join(spreaded_nlog, by = "id") %>%
    left_join(spreaded_log, by = "id") %>%
    select(-qualifiers) %>%
    janitor::clean_names()
  
}

clean_events <- clean_qualifiers(events)
```
We also need to add some player names, which we can do by making a players table and joining on `player_id`.

```r
home <- list[["home"]][["players"]] %>%
  tbl_df() %>%
  select(-contains("stats")) %>%
  janitor::clean_names() # Remove the 800 or so "stats" columns created by calling "flatten = TRUE"
  # in fromJSON earlier

# Repeat the process for the away team
away <- list[["away"]][["players"]] %>%
  tbl_df() %>%
  select(-contains("stats")) %>%
  janitor::clean_names() 

# Create a single players df
players <- bind_rows(home, away)

# Join to events 
clean_events <- clean_events %>%
  left_join(players, by = "player_id")
```

## Computing the plot data

The next stage is to manipulate the data into a format that can be used for plotting. This involves a long chain of `group_by`
and `mutate` calls. 

```r
plots_data <- clean_events %>%
  filter(type_display_name == "Pass",
  
  # Remove free kicks, corners and throw-ins
         is.na(freekick_taken), is.na(indirect_freekick_taken), is.na(corner_taken),
         is.na(throw_in)) %>%
         
  # Round each pass angle to the nearest 20 degrees (you can vary this to see what looks best)
  mutate(angle = as.numeric(angle),
         rounded_angle = round(angle * 180 / pi / 20) * 20) %>%
          group_by(name, player_id,  team_id) %>%
          
  # Count number of passes for each player
  mutate(passes = n()) %>%
  ungroup() %>%
  group_by(name, player_id, team_id, rounded_angle) %>%
  
  # Calculate proportion of passes for each rounded angle
  mutate(angle_frequency = n() / passes) %>%
  ungroup() %>%
  group_by(name, player_id, team_id) %>%
  
  # Normalise each proportion relative the the highest
  mutate(max_frequency = max(angle_frequency),
         angle_normalised = angle_frequency / max_frequency) %>%
  ungroup() %>%
  group_by(rounded_angle, name, player_id, team_id, passes) %>%
  
  # Finally, calculate the mean for each rounded angle, and the mean length, setting a maximum of 30 metres
  summarise(angle_normalised = mean(angle_normalised),
            distance = mean(as.numeric(length), na.rm = TRUE),
            distance = ifelse(distance > 30, 30, distance)) %>%
  ungroup()

```












