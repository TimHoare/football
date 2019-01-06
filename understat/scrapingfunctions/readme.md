Understat Scraping Functions 
=============================
These functions exctract the raw json nested in understat.com webpages and converts it into rectangular dataframes.

Usage
------
```get_league_matches``` takes a league url as a string and outputs a dataframe

```r
EPL <- get_league_matches("https://understat.com/league/EPL")
EPL
```
![](https://github.com/TimHoare/football/blob/master/understat/scrapingfunctions/images/get_league_matches_example.png)

```get_matches``` wraps around the ```get_league_matches``` function (so ```get_league_matches``` must be loaded for it to run).
It takes 3 arguments: 
* league - A string, "ENG", "ITA", "SPA", "GER", "FRA" or "RUS" corresponding for the top league for that country.
* season - An integer, Set as 2018 by default (2018/19 season). This can be set as far back as 2014 (2014/15 season). Any earlier year that will revert to 2018.
* type - A string "all", for all matches for that season; "fixtures" for the fixtures for the current season (will return an empty dataframe for a non-current season); or "results" for results only. 

```r
get_matches("ENG", 2015, "all")
get_matches("RUS", type = "fixtures")
```
![](https://github.com/TimHoare/football/blob/master/understat/scrapingfunctions/images/get_matches_example1.png)
![](https://github.com/TimHoare/football/blob/master/understat/scrapingfunctions/images/get_matches_example2.png)

```get_match_xG``` takes a match url or match id and returns the shot data for that match as a dataframe.

```r
get_match_xG(10047) # Match id for Barcelona vs Real Madrid, 28/10/2018
```
![](https://github.com/TimHoare/football/blob/master/understat/scrapingfunctions/images/get_match_xG_example.png)

```get_player_data``` takes a player url or id and returns all available shot data as a dataframe

```r
get_player_data("https://understat.com/player/2371") # Player url for Cristiano Ronaldo
```
![](https://github.com/TimHoare/football/blob/master/understat/scrapingfunctions/images/get_player_data_example.png)

```get_team_data``` takes a team's url for a single season and returns all player data for that team as a data frame

```r
get_team_data("https://understat.com/team/Paris_Saint_Germain/2018")
```
![](https://github.com/TimHoare/football/blob/master/understat/scrapingfunctions/images/get_team_data_example.png)












