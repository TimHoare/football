# Creating a PassSonar plot in R using data from WhoScored

This post is based on and modified from Eliot McKinley's [tutorial](https://github.com/etmckinley/PassSonar/blob/master/StatsBomb%20PassSonars.R) using StatsBomb's [free data](https://github.com/statsbomb/open-data)

## Getting the data 

WhoScored provide an interactive match centre for a large number of football matches, giving the public the ability to look at
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
  group_by(name, player_id,  field) %>%
  
  # Count number of passes for each player
  mutate(passes = n()) %>%
  ungroup() %>%
  group_by(name, player_id, field, rounded_angle) %>%
  
  # Calculate proportion of passes for each rounded angle
  mutate(angle_frequency = n() / passes) %>%
  ungroup() %>%
  group_by(name, player_id, field) %>%
  
  # Normalise each proportion relative the the highest
  mutate(max_frequency = max(angle_frequency),
         angle_normalised = angle_frequency / max_frequency) %>%
  ungroup() %>%
  group_by(rounded_angle, name, player_id, field, passes) %>%
  
  # Finally, calculate the mean for each rounded angle, and the mean length, setting a maximum of 30 metres
  summarise(angle_normalised = mean(angle_normalised),
            distance = mean(as.numeric(length), na.rm = TRUE),
            distance = ifelse(distance > 30, 30, distance)) %>%
  ungroup()
```
The summary table should look something like this: 

![](https://github.com/TimHoare/football/blob/master/visualisations/PassSonar/images/plots_info.png)

### Formation info

The starting formation of each team is detailed in the `clean_events` dataframe with the `type_display_name` "FormationSet". 
The player ids for either the home or away side (you actually need to find the relevant team id here) and the formation itself
need to be put into separate variables

```r
  ids <- clean_events %>%
  filter(team_id == 65, type_display_name == "FormationSet") %>% # I've chosen Barcelona here
  pull(involved_players) %>% # This gets the player ids in the order we need them in
  strsplit(split = ",") %>%
  unlist()

  formation <- clean_events %>%
    filter(team_id == 65, type_display_name == "FormationSet") %>%
    pull(team_formation) # This gets the formation ID 
  ```

## Plotting

Now we can now use the data to build the plot. The way to do this is by making 11 individual sonar plots and storing them in a
list as ggplot grobs. Something I have done out of personal preference is to swap the y and fill axes, as it makes more sense
to me to have distance represented by bar length; Normally these are the other way around. However, this can easily be changed
by swapping them over in the code. 


```r
player_plots <- list()

for (i in 1:11) {
  
  player <- plots_data %>%
    filter(player_id == ids[i])
  
  plot <- ggplot(player, aes(rounded_angle, distance, fill = angle_normalised)) +
    geom_col() +
    viridis::scale_fill_viridis(name = "Normalised Angle",
                                limits = c(0, 1),
                                na.value = "#FDE725FF") +
    coord_polar(start = 0, direction = - 1) +
    labs(title = player$name[1]) +
    scale_y_continuous(limits = c(0, 30)) +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(margin = margin(b = -15), hjust = 0.5, size = 11, colour = "white"))
  
  player_plots[[i]] <- ggplotGrob(plot)
  
  if (i == 11) {
    colourbar <- ggplot(player, aes(rounded_angle, distance, fill = angle_normalised)) +
      geom_col() +
      viridis::scale_fill_viridis(name = "Pass angle frequency",
                                  limits = c(0, 1),
                                  na.value = "#FDE725FF") +
      labs(x = NULL, y = NULL) +
      theme_void() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 10, colour = "white"),
            legend.title = element_text(colour = "white"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA)) +
      guides(fill = guide_colorbar(title.position = "top"))
    colourbar <- ggplotGrob(colourbar)
  }
  
}
```
The next step is to plot these on a visulisation of a football pitch. I have a function [here](https://github.com/TimHoare/football/blob/master/functions/whoscored_pitch.R), which is a modified version of [FCrSTATS](https://github.com/FCrSTATS)' pitch plotting function, with proportions specific to WhoScored. 

Also, the formation data from earlier needs to be used to know where the plots need to be on the pitch. I have made a [csv](https://github.com/TimHoare/football/blob/master/visualisations/PassSonar/formation_positions.csv) with the relative postions
for each formation slot for each formation, which makes plotting a variety of different formations very easy.

```r
formation_positions <- read_csv("formation_positions.csv")

formation_positions <- formation_positions %>%
  filter(formationId == as.numeric(formation)) %>% # Filter for locations for the team's formation (from earlier)
  mutate(formationSlot = 1:11,
         vertical = vertical / 10 * 105, # Mutate coordinates so they match the dimensions of the whoscored_pitch function
         horizontal = (10 - horizontal) / 10 * 68) %>%
  bind_cols(ids = ids[1:11]) # Bind on the player ids
  
# Making the final plot

radar_size <- 25 

whoscored_pitch(theme = "dark") +
  coord_flip() +
  annotation_custom(grob = player_plots[[1]],
                    xmin = pull(formation_positions[1, 3]) - radar_size / 2,
                    xmax = pull(formation_positions[1, 3]) + radar_size / 2,
                    ymin = pull(formation_positions[1, 4]) - radar_size / 2,
                    ymax = pull(formation_positions[1, 4]) + radar_size / 2) +
  annotation_custom(grob = player_plots[[2]],
                    xmin = pull(formation_positions[2, 3]) - radar_size / 2,
                    xmax = pull(formation_positions[2, 3]) + radar_size / 2,
                    ymin = pull(formation_positions[2, 4]) - radar_size / 2,
                    ymax = pull(formation_positions[2, 4]) + radar_size / 2) +
  annotation_custom(grob = player_plots[[3]],
                    xmin = pull(formation_positions[3, 3]) - radar_size / 2,
                    xmax = pull(formation_positions[3, 3]) + radar_size / 2,
                    ymin = pull(formation_positions[3, 4]) - radar_size / 2,
                    ymax = pull(formation_positions[3, 4]) + radar_size / 2) +
  annotation_custom(grob = player_plots[[4]],
                    xmin = pull(formation_positions[4, 3]) - radar_size / 2,
                    xmax = pull(formation_positions[4, 3]) + radar_size / 2,
                    ymin = pull(formation_positions[4, 4]) - radar_size / 2,
                    ymax = pull(formation_positions[4, 4]) + radar_size / 2) +
  annotation_custom(grob = player_plots[[5]],
                    xmin = pull(formation_positions[5, 3]) - radar_size / 2,
                    xmax = pull(formation_positions[5, 3]) + radar_size / 2,
                    ymin = pull(formation_positions[5, 4]) - radar_size / 2,
                    ymax = pull(formation_positions[5, 4]) + radar_size / 2) +
  annotation_custom(grob = player_plots[[6]],
                    xmin = pull(formation_positions[6, 3]) - radar_size / 2,
                    xmax = pull(formation_positions[6, 3]) + radar_size / 2,
                    ymin = pull(formation_positions[6, 4]) - radar_size / 2,
                    ymax = pull(formation_positions[6, 4]) + radar_size / 2) +
  annotation_custom(grob = player_plots[[7]],
                    xmin = pull(formation_positions[7, 3]) - radar_size / 2,
                    xmax = pull(formation_positions[7, 3]) + radar_size / 2,
                    ymin = pull(formation_positions[7, 4]) - radar_size / 2,
                    ymax = pull(formation_positions[7, 4]) + radar_size / 2) +
  annotation_custom(grob = player_plots[[8]],
                    xmin = pull(formation_positions[8, 3]) - radar_size / 2,
                    xmax = pull(formation_positions[8, 3]) + radar_size / 2,
                    ymin = pull(formation_positions[8, 4]) - radar_size / 2,
                    ymax = pull(formation_positions[8, 4]) + radar_size / 2) +
  annotation_custom(grob = player_plots[[9]],
                    xmin = pull(formation_positions[9, 3]) - radar_size / 2,
                    xmax = pull(formation_positions[9, 3]) + radar_size / 2,
                    ymin = pull(formation_positions[9, 4]) - radar_size / 2,
                    ymax = pull(formation_positions[9, 4]) + radar_size / 2) +
  annotation_custom(grob = player_plots[[10]],
                    xmin = pull(formation_positions[10, 3]) - radar_size / 2,
                    xmax = pull(formation_positions[10, 3]) + radar_size / 2,
                    ymin = pull(formation_positions[10, 4]) - radar_size / 2,
                    ymax = pull(formation_positions[10, 4]) + radar_size / 2) +
  annotation_custom(grob = player_plots[[11]],
                    xmin = pull(formation_positions[11, 3]) - radar_size / 2,
                    xmax = pull(formation_positions[11, 3]) + radar_size / 2,
                    ymin = pull(formation_positions[11, 4]) - radar_size / 2,
                    ymax = pull(formation_positions[11, 4]) + radar_size / 2) +
  annotation_custom(grob = colourbar, xmin = 3, xmax = 7, ymin = 5, ymax = 20) +
  annotate("text", label='Bar length = mean pass distance', x = -2, y = 60, size = 4,
           colour = "white") +
  annotate("text", label = "Barcelona PassSonar vs Real Madrid, 6th Feb 2019", x = 110, y = 34,
           colour = "white", size = 8, fontface = "bold")
```
Here's the final plot:

![](https://github.com/TimHoare/football/blob/master/visualisations/PassSonar/images/barca_sonar.png)




