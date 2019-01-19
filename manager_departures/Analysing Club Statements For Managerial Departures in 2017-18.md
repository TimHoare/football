# Analysing Club Statements For Managerial Departures in 2017-18

The idea for this analysis is shamelessly stolen from a piece by [BBC Sport](https://www.bbc.co.uk/sport/football/46875479)

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

Here, uninteresting words such as the, and, of, have been filtered out using the built in `stop_words` dataset from `tidytext`
Also "â" has been filtered out as this how double quotation marks have been represented. 

There are some more fairly uninteresting words at the top here: club, manager, team and football are all pretty obvious. "Paul"
is strange one, but we see that a Paul was sacked/departed 6 times last season (Tisdale, Heckingbottom (twice), Hurst, Lambert
and Clement)

### Sentiment Analysis

Sentiment analysis considers the the emotion or intention of a text to get an overall idea of how positive or negative it is.
`tidytext` has three build in sentiment lexicons, and in this case I will be using the `afinn` lexicon which assigns a score 
to each word based on how positive or negative it is, e.g. "anger" has a score of -3, while "wowww" has a score of 4.

I'm not going to spend long on this because there are a variety of issues with this approach, for example: 
* In most cases there is not nearly enough text to get an accurate representation of the sentiment. 
* Words can be interpreted in many different ways and the lexicon may not always get it right.
* Any sort of negation cannot be taken into account here as we are going on a per-word basis.

#### Which statements show the most negative sentiment

```r
managers_2018_words %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(name, club, id) %>%
  summarise(sentiment = sum(score)) %>%
  arrange(sentiment)
  ```
![](https://github.com/TimHoare/football/blob/master/manager_departures/images/negative_sentiment.png)

We see that Gary Caldwell's departure from Chesterfield is the only statement with a negative sentiment score. We can also 
look at the induvidual words and how they were assigned:

```r
managers_2018_words %>%
  inner_join(get_sentiments("afinn")) %>%
  filter(id == 64)
  ```
![](https://github.com/TimHoare/football/blob/master/manager_departures/images/gary_caldwell.png)  

Only five words (out of 110) have been assined a sentiment score, and the word "hard" for example has been assigned a negative 
score, when it has been used in a positive context: "Gary has worked very hard during his time at the club".

#### Which statements show the most positive sentiment

We can also look at the most positive statements using similar code:

```r
managers_2018_words %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(name, club, id) %>%
  summarise(sentiment = sum(score)) %>%
  arrange(desc(sentiment))
  ```
  ![](https://github.com/TimHoare/football/blob/master/manager_departures/images/positive_sentiment.png) 
  
  Top of the list is the longest statement, regarding Paul Clement's departure from Swansea City, as the club give him plenty of credit
  for his work in the previous season.
  
  ### Relationships between words
  
  
  The `tidytext` package also has the capability to look at ngrams, which tokenises adjacent words. Looking at bigrams:
  
  ```r
  manager_bigrams <- managers_2018 %>%
  unnest_tokens(bigram, value, token = "ngrams", n = 2)


manager_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !is.na(word1), !is.na(word2),
         !word1 %in% c("â", "œi"),
         !word2 %in% c("â", "œi")) %>%
  count(word1, word2, sort = TRUE)
  ```
  ![](https://github.com/TimHoare/football/blob/master/manager_departures/images/bigrams.png) 
  
  We can visualise these relationships using the `ggraph` and `igraph` packages:
  
  ```r
  library(igraph)
  library(ggraph)
  
  ngram_graph <- manager_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !is.na(word1), !is.na(word2),
         !word1 %in% c("â", "œi"),
         !word2 %in% c("â", "œi")) %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n >= 5) %>%
  graph_from_data_frame()


set.seed(2019)

ggraph(ngram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(colour = "firebrick1", size = 5, alpha = 0.75) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
  ```
![](https://github.com/TimHoare/football/blob/master/manager_departures/images/bigram_graph.png)
  
Again, we don't have a lot of information here to make a good graph, as we mostly get club names, but we do see "mutual consent"
which is often used in statements, and a larger network centred around team affairs, management etc.
