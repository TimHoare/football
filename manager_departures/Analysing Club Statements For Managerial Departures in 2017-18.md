# Analysing Club Statements For Managerial Departures in 2017-18

The idea for this analysis is shamelessly stolen from an piece by [BBC Sport](https://www.bbc.co.uk/sport/football/46875479)

### Getting the data 

The basic information, such as manager name, club name and leaving date was obtained from www.thesackrace.com. In this analysis we will
be looking at the 67 manager sackings/departures in professional English football in 2017/18. The content from the club statements
for these departures was laboriously copy-and-pasted into .txt files then joined onto the table containing the basic info. 

![](https://github.com/TimHoare/football/blob/master/manager_departures/images/manager_dataframe.png)

Note that the club statement is missing in some cases; this is simply because it was either too challenging to find or could not 
be copy-and-pasted.

### Some basic analysis

#### Which clubs dismissed the most managers?

```r
library(tidyverse)

managers_2018 %>%
  count(club, sort = TRUE)
  ```
![](https://github.com/TimHoare/football/blob/master/manager_departures/images/most_departures.png)

We see that plenty of clubs saw more thatn one manager leave but Barnet FC are the stand out with 4 departures in one season.

#### Who were some of the longest serving managers that left their clubs?

```r
managers_2018 %>%
  mutate(days_at_club = parse_number(days_at_club) / 365) %>%
  arrange(desc(days_at_club))
  ```

![](https://github.com/TimHoare/football/blob/master/manager_departures/images/longest_serving_managers.png)

The most notable departure here is Arsène Wenger, who left Arsenal FC after nearly 22 years in charge. Exeter's manager Paul
Tisdale also left, having been at the club since 2006.

#### Who were the managers that left earliest in the season?

```r
managers_2018 %>%
  arrange(date)
  ```
![](https://github.com/TimHoare/football/blob/master/manager_departures/images/earliest_to_leave.png)

League One side Northampton Town were the first club to part with their manager in 2017/18 after Justin Edinburgh was sacked after
4 straight defeats from the start of the season. Frank de Boer was the first Premier League manager to be sacked after just 77 days 
and 5 games in charge.

### Text Analysis

For the text analysis I will mainly be using the [tidytext](https://cran.r-project.org/web/packages/tidytext/index.html) package,
which employs the tidy data principles (in this case one row per word).

```r
library(tidytext)

managers_2018_words <- managers_2018 %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, value)
  
managers_2018_words
  ```
![](https://github.com/TimHoare/football/blob/master/manager_departures/images/managers_2018_words.png)

#### What were some of the longest and shortest statements from clubs regarding a manager departure?

```r
managers_2018_words %>%
  count(name, club, id, sort = TRUE)
  ```
  ![](https://github.com/TimHoare/football/blob/master/manager_departures/images/longest_statements.png)
  
The longest statement by quite some distance is [Swansea City's](https://www.swanseacity.com/news/swansea-city-part-company-paul-clement) following the departure of Paul Clement in December 2017.

```r
managers_2018_words %>%
  filter(!is.na(word)) %>%
  count(name, club, id) %>%
  arrange(n)
  ```  
![](https://github.com/TimHoare/football/blob/master/manager_departures/images/shortest_statement.png)

The shortest statements here are usually a sentence or two, announcing the manager has left and thanking him for his efforts.

#### What are some of the most common words used?

```r
managers_2018_words %>%
  filter(!is.na(word), !word %in% c("â")) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  head(15) %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(colour = "black", fill = "blue") +
  coord_flip() +
  labs(x = NULL,
       y = "Number of occurrences of word")
```
![](https://github.com/TimHoare/football/blob/master/manager_departures/images/most_common_words.png)

Here, uniteresting words such as the, and, of, have been filtered out using the built in `stop_words` dataset from `tidytext`
Also "â" has been filtered out as this how double quotation marks have been represented. 



  

  
  









