# required packages

install.packages("janeaustenr")
install.packages("SentimentAnalysis")
install.packages("tidytext")
install.packages("wordcloud")
install.packages("Rcpp")
install.packages("RColorBrewer")

# Required Libraries

library(janeaustenr)
library(stringr)
library(tidytext)
library(dplyr)
library(tidyr)
library(reshape2)
library(wordcloud)
library(SentimentAnalysis)
library(RColorBrewer)

sentiments # 


data1 <- austen_books() %>%     #inner join concept used with mutate.
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)



pos_sentimnt <- get_sentiments("bing") %>%  # bing used to implement filter
  filter(sentiment == "positive")

data1 %>%
  filter(book == "Emma") %>%            # to find +ve words like ( joy happy etc)
  semi_join(pos_sentimnt) %>%
  count(word, sort = TRUE)


bing <- get_sentiments("bing")

Ema_sentimt <- data1 %>%              
  inner_join(bing) %>%
  count(book = "Emma" , index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%            # separting in 2 columns +ve and -ve
  mutate(sentiment = positive - negative)       # calculating total sentiment

library(ggplot2)

ggplot(Ema_sentimt, aes(index, sentiment, fill = book)) +      #pos and neg scores of words
  geom_bar(stat = "identity", show.legend = TRUE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

word_counts <- data1 %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)

head(word_counts)

word_counts %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment))+
  geom_col() +
  coord_flip() +
  labs(y = "Sentiment Score for words")   #getting sentiment score of words and visualizing it


data1 %>%                            #print the word cloud
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "dark green"),
                   max.words = 100)

