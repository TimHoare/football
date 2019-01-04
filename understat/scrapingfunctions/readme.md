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
