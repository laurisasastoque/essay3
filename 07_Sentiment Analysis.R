
# Where do I start? Create your corpus and set up your data with R Studio -----



# This is an R script file, created by Giulia (reads like English "Julia")

# Everything written after an hashtag is a comment (normally appears in green). If you don't want to type the hash manually every time, you can type your comments normally and after you finish, with the cursor on the sentence, press ctrl+shift+c. it will turn text into a comment and vice versa.

# Everything else is R code. To execute the code, place the cursor on the corresponding line and press Ctrl+Enter (windows)


## load packages ----------

# Ok, let's start! Before you begin you will need to load some packages. These allow you to execute specific operations.
# If you have not done so already, you have to install them first: it might take a few minutes and you only have to do it once. If R asks you whether you want to install dependencies for the packages, say yes.
# 
# install.packages("tidyverse")
# install.packages("readr")
# install.packages("data.table")
# install.packages("tm")
# install.packages("tidytext")
# install.packages("syuzhet")
# install.packages("sjPlot")
# install.packages("wordcloud")
# install.packages("textdata")

# Once you have installed the packeges you can comment the installation code like this:

#   install.packages("blablabla")

# so this operation will not be execute again in the future.


library(tidyverse)
library(ggplot2)
library(readr)
library(data.table)
library(syuzhet)
library(tm)
library(tidytext)
library(sjPlot)
library(wordcloud)
library(textdata)
library(readtext)

# load required objects
load("corpus_all/metadata_all.rda")

useless_words <- readtext("useless_words.txt", encoding = "UTF-8") %>% 
  tidytext::unnest_lines(input = "text", output = "words")


## set options --------------------

# we can start setting up a few options for our project

options(stringsAsFactors = F, # do not convert to factor upon loading
        scipen = 999, # do not convert numbers to e-values
        max.print = 200, # stop printing after 200 values
        warn = -1) # as many warnings in R are useless (and annoying), you might want to disable them


theme_set(theme_sjplot2()) # set default ggplot theme to light
fs = 12 # default plot font size


## files import ----------------- 

# in this case, the files are consistently saved as author_title_year.txt, where we use one token only for the author and for the title, and the format YYYY for the year of first publication.

# It is important to be consistent! It can make your life much easier when you deal with many texts.

# this tells R to look only for txt files in the working directory. (that's why we had to change it. we will set it back to the previous WD later)

files_list <- list.files("corpus_all", full.names = T, pattern = "txt") 

corpus_source <- readtext(files_list, encoding = "UTF-8") %>%
 mutate(doc_id = stringr::str_remove(doc_id, ".txt")) %>%
 inner_join(metadata_all, by = "doc_id") %>% 
  group_by(is_male) %>% 
  sample_n(5) %>% 
  as_tibble() 


# let's have a look at out dataset now

head(corpus_source)

corpus_sentences <- corpus_source %>%
  unnest_lines(input = text, output = sentence, drop = T, to_lower = F) %>%
  group_by(doc_id) %>%
  mutate(sentence_id = seq_along(sentence)) %>%
  ungroup() %>%
  mutate(unique_sentence_id = seq_along(sentence))

corpus_tokens <- corpus_sentences %>%
  unnest_tokens(input = sentence, output = token, drop = T, to_lower = T) %>% 
  # we turn all to lowercase
  group_by(doc_id, sentence_id) %>%
  mutate(token_id = seq_along(token)) %>%
  ungroup() %>%
  mutate(unique_token_id = seq_along(token)) %>% 
  filter() %>% 
  filter(!token %in% useless_words$words)

head(corpus_tokens)



# Corpus overview --------
## plot token frequency ---------------------- 

# now we can have a first look at our corpus and see which words are most frequent in the novels

corpus_tokens %>%
  group_by(song_name, token) %>%
  anti_join(as_tibble(stopwords("es")), by = c("token" = "value")) %>% # delete stopwords
  count() %>% # summarize count per token per title
  arrange(desc(n)) %>% # highest freq on top
  group_by(song_name) %>% # 
  mutate(top = seq_along(token)) %>% # identify rank within group
  filter(top <= 15) %>% # retain top 15 frequent words
  # create barplot
  ggplot(aes(x = -top, fill = song_name)) + 
  geom_bar(aes(y = n), stat = 'identity', col = 'black') +
  # make sure words are printed either in or next to bar
  geom_text(aes(y = ifelse(n > max(n) / 2, max(n) / 50, n + max(n) / 50),
                label = token), size = fs/3, hjust = "left") +
  theme(legend.position = 'none', # get rid of legend
        text = element_text(size = fs), # determine fs
        axis.text.x = element_text(angle = 45, hjust = 1, size = fs/1.5), # rotate x text
        axis.ticks.y = element_blank(), # remove y ticks
        axis.text.y = element_blank()) + # remove y text
  labs(y = "token count", x = "", # add labels
       title = "Most frequent words throughout the songs") +
  facet_grid(. ~ song_name) + # separate plot for each title
  coord_flip() + # flip axes
  scale_fill_sjplot()

# relatively unsurprisingly, names of characters are generally the most frequent tokens. To see what other tokens are highly frequent, we can for example import a list of first and last names, so that we can exclude them from the plot.


# first_names <- read_csv("scripts/first_names.txt") %>%
#   rename(token = word)
# 
# last_names <- read_csv("scripts/last_names.txt") %>%
#   rename(token = word)
# 
# 
# # let's see then how it looks without those
# 
# corpus_tokens %>%
#   anti_join(first_names) %>%
#   anti_join(last_names) %>%
#   group_by(title, token) %>%
#   anti_join(as_tibble(stopwords()), by = c("token"="value")) %>% # delete stopwords
#   count() %>% # summarize count per  per title
#   arrange(desc(n)) %>% # highest freq on top
#   group_by(title) %>% # 
#   mutate(top = seq_along(token)) %>% # identify rank within group
#   filter(top <= 15) %>% # retain top 15 frequent words
#   # create barplot
#   ggplot(aes(x = -top, fill = title)) + 
#   geom_bar(aes(y = n), stat = 'identity', col = 'black') +
#   # make sure words are printed either in or next to bar
#   geom_text(aes(y = ifelse(n > max(n) / 2, max(n) / 50, n + max(n) / 50),
#                 label = token), size = fs/3, hjust = "left") +
#   theme(legend.position = 'none', # get rid of legend
#         text = element_text(size = fs), # determine fs
#         axis.text.x = element_text(angle = 45, hjust = 1, size = fs/1.5), # rotate x text
#         axis.ticks.y = element_blank(), # remove y ticks
#         axis.text.y = element_blank()) + # remove y text
#   labs(y = "Word count", x = "", # add labels
#        title = "Most frequent words throughout the novels") +
#   facet_grid(. ~ title) + # separate plot for each title
#   coord_flip() + # flip axes
#   scale_fill_sjplot()

# can you see any interesting pattern now?



# Sentiment analysis --------------

# "so, now, what about the sentiments?", you might ask?



## lexicons -----------

# first we need to decide which lexicons we can use for sentiment analysis

# in this example, we will use three popular lexicons, namely the AFINN, NRC and BING. For the sake of simplicity, we will simply use the versions provided by the syuzhet package.
# we can therefore match these onto our corpus directly with the function called get_sentiments, which is included in the syuzhet package. Rather than loading the sentiment lexicons, it applies it directly to the corpus.

# but before we do that, let's have a look at the Sentiment lexicons alone:

## 1) which lexicons are included in the package and how big are they?

?get_sentiment_dictionary()

head(get_sentiment_dictionary("nrc", language = "spanish"), 100)
get_sentiment_dictionary("nrc", language = "spanish") %>% nrow()



# now let's apply them all to our corpus


novels_SA <- bind_rows(

  
  # 3 NRC 
  corpus_tokens %>% 
    inner_join(get_sentiment_dictionary("nrc", language = "spanish"), by = c("token" = "word")) %>%
    group_by(song_name, sentiment) %>%
    mutate(dictionary = 'nrc')
  
) %>%
  ungroup()

# in this case, we have performed an "inner_join" function from the package tidyverse. this means that the combination of our corpus and the lexicons will only preserve the words for which a match exist, i.e. only words with a sentiment value.

# if you want to preserve the whole corpus, with NA values for "empty" matches, you can use "letf_join" instead. give it a try and see how it changes!


## let's have a look at our corpus

novels_SA %>% head()



# we can have a look at the most frequent words with a sentiment value using a wordcloud. we can look at these all together, or select one specific sentiment for the wordcloud.

## wordclouds by sentiment ---------------

# positive sentiments

novels_SA %>%
  # anti_join(stop_words, by = c("token" = "word")) %>% # delete stopwords
  filter(sentiment %in% c("positive", "anticipation", "joy", "trust")) %>% # here is where we can select what to look at
  group_by(token) %>%
  count() %>% # summarize count per token
  mutate(log_n = sqrt(n)) %>% # take root to decrease outlier impact
  with(wordcloud(token, 
                 log_n,
                 max.words = 50, 
                 colors=brewer.pal(5, "Dark2"),
                 random.order = F,
  ))

# negative sentiments

novels_SA %>%
  # anti_join(stop_words, by = c("token"="word")) %>% # delete stopwords
  filter(sentiment == "negative") %>% # here is where we can select what to look at
  group_by(token) %>%
  count() %>% # summarize count per token
  mutate(log_n = sqrt(n)) %>% # take root to decrease outlier impact
  with(wordcloud(token, 
                 log_n,
                 max.words = 50, 
                 colors=brewer.pal(5, "Dark2"),
                 random.order = F,
  ))


# it looks pretty correct, right? a lot of "love" for positive sentiments and quite some doubt and death for negative ones
# You might have noticed that "miss" is by far the biggest (and therefore more frequent) term in the "negative cloud. With some sense, we can probably understand that there is a chance this is a "mistake": "to miss" as a verb might be negative, but it is possible that "miss" would not really be a negative term in Austen's novels when referring to a young woman.
# we can see how the graph looks like without it.



# better, right?





## dictionaries comparison -----------------

# we might want to see if the different lexicons perform differently.
# the three lexicons share the negative/positive value, so let's focus on that

# novels_SA %>%
#   filter(sentiment == "negative" | sentiment == "positive") %>%
#   group_by(token, sentiment, dictionary) %>%
#   count() %>% # summarize count per token per sentiment
#   group_by(sentiment) %>%
#   arrange(sentiment, desc(n)) %>% # most frequent on top
#   mutate(top = seq_along(token)) %>% # identify rank within group
#   filter(top <= 15) %>% # keep top 15 frequent words
#   ggplot(aes(x = -top, fill = factor(sentiment))) + 
#   # create barplot
#   geom_bar(aes(y = n), stat = 'identity', col = 'black') +
#   # make sure words are printed either in or next to bar
#   geom_text(aes(y = ifelse(n > max(n) / 2, max(n) / 50, n + max(n) / 50),
#                 label = token), size = fs/3, hjust = "left") +
#   theme(legend.position = 'none', # remove legend
#         text = element_text(size = fs), # determine fs
#         axis.text.x = element_text(angle = 45, hjust = 1), # rotate x text
#         axis.ticks.y = element_blank(), # remove y ticks
#         axis.text.y = element_blank()) + # remove y text
#   labs(y = "token count", x = "", # add manual labels
#        title = "Frequency of words carrying sentiment",
#        subtitle = "Using tidytext and the AFINN, bing, and nrc sentiment dictionaries") +
#   facet_grid(sentiment ~ dictionary) + # separate plot for each sentiment
#   coord_flip()  + # flip axes
#   scale_fill_sjplot()


## NRC also has discrete emotions, we might want to focus on them separately

novels_SA %>%
  filter(dictionary == "nrc") %>%
  filter(!sentiment %in% c('negative','positive')) %>%
  group_by(token, sentiment, dictionary) %>%
  count() %>% # summarize count per token per sentiment
  group_by(sentiment) %>%
  arrange(sentiment, desc(n)) %>% # most frequent on top
  mutate(top = seq_along(token)) %>% # identify rank within group
  filter(top <= 15) %>% # keep top 15 frequent words
  ggplot(aes(x = -top, fill = factor(sentiment))) + 
  # create barplot
  geom_bar(aes(y = n), stat = 'identity', col = 'black') +
  # make sure words are printed either in or next to bar
  geom_text(aes(y = ifelse(n > max(n) / 2, max(n) / 50, n + max(n) / 50),
                label = token), size = fs/3, hjust = "left") +
  theme(legend.position = 'none', # remove legend
        text = element_text(size = fs), # determine fs
        axis.text.x = element_text(angle = 45, hjust = 1), # rotate x text
        axis.ticks.y = element_blank(), # remove y ticks
        axis.text.y = element_blank()) + # remove y text
  labs(y = "token count", x = "", # add manual labels
       title = "Frequency of words carrying sentiment",
       subtitle = "nrc sentiment dictionary") +
  facet_grid(. ~ sentiment) + # separate plot for each sentiment
  coord_flip()  + # flip axes
  scale_fill_sjplot()



# these explorations may reveal interesting patterns in the novels, as well as flows in the sentiment lexicons. A "perfect" lexicons does not exist, but more and more researchers are working to develop better and more reliable ones.




# sentiment across chapters --------

# another thing you might want to do is to have a look at how sentiment evolves acrosss a narrative.
# we can do so with some graphs:


## nrc emotions across novels (let's see three) ----------------------------

title_list <- novels_SA %>% select(song_name) %>% distinct() %>% sample_n(3)

novels_SA %>% 
  filter(dictionary == "nrc") %>%
  filter(!sentiment %in% c('negative','positive')) %>%
  right_join(title_list) %>% # we retain only the titles we selected
  group_by(sentiment, song_name, sentence_id) %>%
  count() %>% # summarize count
  # create area plot
  ggplot(aes(x = as.numeric(sentence_id), y = n)) +
  geom_area(aes(fill = sentiment), stat = 'identity') + 
  # add black smoothing line without standard error
  theme(legend.position = 'none', # remove legend
        text = element_text(size = fs)) + # change font size
  labs(x = "Chapter", y = "Emotion value", # add labels
       subtitle = "Using tidytext and the nrc sentiment dictionary") +
  # separate plots per sentiment and title and free up x-axes
  facet_grid(sentiment ~ song_name, scale = "free_x") +
  scale_fill_sjplot()


# or just negative positive


novels_SA %>% 
  filter(dictionary == "nrc") %>%
  filter(sentiment %in% c('negative','positive')) %>%
  right_join(title_list) %>% # we retain only the titles we selected
  group_by(sentiment, title, sentence_id) %>%
  count() %>% # summarize count
  # create area plot
  ggplot(aes(x = as.numeric(sentence_id), y = n)) +
  geom_area(aes(fill = sentiment), stat = 'identity') + 
  # add black smoothing line without standard error
  theme(legend.position = 'none', # remove legend
        text = element_text(size = fs)) + # change font size
  labs(x = "Chapter", y = "Emotion value", # add labels
       subtitle = "Using tidytext and the nrc sentiment dictionary") +
  # separate plots per sentiment and title and free up x-axes
  facet_grid(sentiment ~ title, scale = "free_x") +
  scale_fill_sjplot()



# this value is a binary value yes/no (positive/negative), so it does not tell us much abiout the actual dimension of the sentiment.
# other dictionaries use continuous values, such as the 'afinn' one.
# let's see the same plot for it


afinn <- get_sentiment_dictionary("afinn")

test <- novels_SA %>% 
  filter(dictionary == "afinn") %>%
  right_join(title_list) %>% # we retain only the titles we selected
  group_by(title, sentence_id, sentiment) %>%
  summarise(n = sum(value)) %>%
  arrange(desc(n)) %>%
  left_join(corpus_sentences)

novels_SA %>% 
  filter(dictionary == "afinn") %>%
  right_join(title_list) %>% # we retain only the titles we selected
  group_by(title, sentence_id, sentiment) %>%
  summarise(n = sum(value)) %>% # summarize count
  # create area plot
  ggplot(aes(x = as.numeric(sentence_id), y = n)) +
  geom_area(aes(fill = sentiment), stat = 'identity') + 
  # add black smoothing line without standard error
  theme(legend.position = 'none', # remove legend
        text = element_text(size = fs)) + # change font size
  labs(x = "Chapter", y = "Emotion value", # add labels
       subtitle = "Using tidytext and the nrc sentiment dictionary") +
  # separate plots per sentiment and title and free up x-axes
  facet_grid(sentiment ~ title, scale = "free_x") +
  scale_fill_sjplot()



# it is often easier to evaluate the sentiment over a novel by looking a bigger chunks of text at the time (such as chapters). Sometimes you might have that information in your files. 
# Because in this case we do not have chapters data in our dataset, we can arbitrarily assign "fake chapters" to the novels, to see the evolution of sentiment throughout them. (of course if you have that data already present in your dataset you do not need this)

# for the sake of simplicity, let's split the novels into 15 chapters each.

test <- novels_SA %>%
  ungroup() %>%
  group_split(title)

test2 = list()

for (i in 1:length(test)) {
  avg_ch_lenght <- nrow(test[[i]])/15
  r  <- rep(1:ceiling(nrow(test[[i]])/avg_ch_lenght),each=avg_ch_lenght)[1:nrow(test[[i]])]
  test2[[i]] <- split(test[[i]],r)
}


for (i in 1:length(test2)) {
  for (j in 1:length(test2[[i]])) {
    test2[[i]][[j]]$chapter <- paste0(j)
  }
}

test = list()

for (i in 1:length(test2)) {
  test[[i]] <- data.table::rbindlist(test2[[i]])
}

novels_SA2 <- data.table(rbindlist(test))
novels_SA2$chapter <- as.numeric(novels_SA2$chapter)

remove(test, test2, j,i,r,avg_ch_lenght)

# and let's see how it looks if we group and count by 'chapter'


novels_SA2 %>% 
  filter(dictionary == "nrc") %>%
  filter(sentiment %in% c('negative','positive')) %>%
  right_join(title_list) %>% # we retain only the titles we selected
  group_by(sentiment, title, chapter) %>%
  count() %>% # summarize count
  # create area plot
  ggplot(aes(x = as.numeric(chapter), y = n)) +
  geom_area(aes(fill = sentiment), stat = 'identity') + 
  # add black smoothing line without standard error
  theme(legend.position = 'none', # remove legend
        text = element_text(size = fs)) + # change font size
  labs(x = "Chapter", y = "Emotion value", # add labels
       subtitle = "Using tidytext and the nrc sentiment dictionary") +
  # separate plots per sentiment and title and free up x-axes
  facet_grid(sentiment ~ title, scale = "free_x") +
  scale_fill_sjplot()



# this value is a binary value yes/no (positive/negative), so it does not tell us much abiout the actual dimension of the sentiment.
# other dictionaries use continuous values, such as the 'afinn' one.
# let's see the same plot for it


afinn <- get_sentiment_dictionary("afinn")

novels_SA2 %>% 
  filter(dictionary == "afinn") %>%
  right_join(title_list) %>% # we retain only the titles we selected
  group_by(title, chapter, sentiment) %>%
  summarise(n = sum(value)) %>% # summarize count
  # create area plot
  ggplot(aes(x = chapter, y = n)) +
  geom_area(aes(fill = sentiment), stat = 'identity') + 
  # add black smoothing line without standard error
  theme(legend.position = 'none', # remove legend
        text = element_text(size = fs)) + # change font size
  labs(x = "Chapter", y = "Emotion value", # add labels
       subtitle = "Using tidytext and the nrc sentiment dictionary") +
  # separate plots per sentiment and title and free up x-axes
  facet_grid(sentiment ~ title, scale = "free_x") +
  scale_fill_sjplot()