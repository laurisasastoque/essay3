# Frequencies

# Load packages
library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(ggplot2)
library(readtext)

# set seed
set.seed(20022901)

# load required objects
load("corpus_all/metadata_all.rda")


# first we need to create a corpus.
# this time we'll be using "quanteda", a package that allows to do corpus analysis very easily

files_list <- list.files("corpus_all", full.names = T, pattern = "txt") 


corpus <- readtext(files_list, encoding = "UTF-8") %>%  
  as_tibble() %>%
  mutate(doc_id = stringr::str_remove(doc_id, ".txt")) %>% 
  left_join(metadata_all, by = "doc_id")


head(corpus)

# and to transform this into a corpus that we can use with the package "quanteda", a friendly package that allows us to analyse various aspects of a corpus

quanteda_texts <- quanteda::corpus(corpus,
                                   docid_field = "doc_id",
                                   text_field = "text",
                                   meta = list("song_name",
                                               "artist_name",
                                               "artist_country",
                                               "is_male"
                                               )
                                   )

remove(corpus)


# Tokens corpus ---------------

# quanteda mainly works with so called DFM (Document-feature matrix). These
# - Represents frequencies of features in documents in a matrix
# - Have an efficient structure, but do not have information on the position of words
# - Allow for a bag-of-words approach


## Let's create a dfm corpus --------------

## first we need to create a "token" corpus. This file is very big,
## so we recommend that you do NOT execute this code. (that's why it's green)
# 
quanteda_texts_tok <- tokens(quanteda_texts,
                             # we don't want pucntuation
                             remove_punct = T,
                             # we want to keep hyphens
                             split_hyphens = T,
                             # but no symbols
                             remove_symbols = T) %>% 
  tokens_remove(pattern = stopwords("es")) %>% 
  tokens_remove(pattern = stopwords("en"))
 
save(quanteda_texts_tok, file = "quanteda_texts_tok.RData")


# load("quanteda_texts_tok.RData")

## then we can create a dfm

quanteda_texts_dfm <- dfm(quanteda_texts_tok)

# and we can have a first look at the most frequent words, for instance with a wordcloud

textplot_wordcloud(quanteda_texts_dfm, max_words = 100)

# in a "table" form

textstat_frequency(quanteda_texts_dfm) %>%
  head(30)

# or in a plot, such as a this one

quanteda_texts_dfm %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

## it is quite evident that we have at the top of the list words that are not 
## extremely interesting (unless you are focusing on conjunctions and pronouns).
## We can thus decide to look only at words that fall into a specific range 
## of our frequency ranking.

# If you look at the table we produced before, you'll see that 
# somewhere around the 150 occurrences words seem to be getting more juicy.
# Let's see how that goes if we set that up.

dfm(quanteda_texts_tok)  %>%
  dfm_trim(max_termfreq = 150) %>% # here we say "we want a frequency max of 5000"
  textplot_wordcloud(max_words = 100)

dfm(quanteda_texts_tok)  %>%
  dfm_trim(max_termfreq = 150) %>% 
  textstat_frequency() %>%
  head(20)

dfm(quanteda_texts_tok)  %>%
  dfm_trim(max_termfreq = 150) %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()


# another thing we can do is to visualize group differences in frequency,
# for instance, we might want to see which words are the most frequently used by women vs men authors.

colnames(quanteda_texts_dfm@docvars) #this will show you which metadta we have

# by country
quanteda_texts_dfm %>% 
  textstat_frequency(n = 20, groups = artist_country) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency, color = group)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

# by gender
quanteda_texts_dfm %>% 
  textstat_frequency(n = 20, groups = is_male) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency, color = group)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()


# we can also compare word frequencies by document
textplot_wordcloud(quanteda_texts_dfm, 
                   max_words = 100, 
                   comparison = TRUE,
                   labelsize = 1, 
                   color = rev(RColorBrewer::brewer.pal(10, "Dark2")))

# check which other color palettes you can use with
RColorBrewer::display.brewer.all()


## or look at single words comparisons:

sorted_features <- topfeatures(quanteda_texts_dfm, n = nfeat(quanteda_texts_dfm))
sorted_features[c("mami", "baby", "linda", "culo")]


# Stats ---------------

## frequencies
## as we saw before, we can easily craete a table with all the information 
## about toke frequencies in our corpus

frequency_table <- textstat_frequency(quanteda_texts_dfm)

# and observe it either in the console

print(head(frequency_table, 20))

# or as a full table

# view(frequency_table)

### plots

quanteda_texts_dfm %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()


# we can also look at how a specific term is used over time

# quanteda_texts_dfm %>% 
#   textstat_frequency(groups = as.numeric(year)) %>% 
#   filter(feature == "war") %>%
#   ggplot(aes(x = group, y = frequency, label=feature)) +
#   geom_col(width = .1) +
#   # geom_point(size = 1) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
#   )


# or compare the presence of a term by author

quanteda_texts_dfm %>% 
  textstat_frequency(groups = artist_name) %>% 
  filter(feature == "bitch") %>%
  ggplot(aes(x = group, y = frequency, label = feature)) +
  geom_col(width = .1) +
  # geom_point(size = 1) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  ) 


## Concordance

# with quanteda you can also visualise concordances (KWIC = keywords in context)

kwic_test <- quanteda_texts_tok %>%
  kwic(pattern =  "ell*", window = 10)

head(kwic_test, 10)


kwic_test <- quanteda_texts_tok %>%
  kwic(pattern =  c("ella*","mujer*"), 
       window = 5)

head(kwic_test, 10)

# if you want to find multi-word expressions, separate words by white space and wrap the character vector by phrase().

kwic_test <- quanteda_texts_tok %>%
  kwic(pattern = phrase("old woman"))

head(kwic_test)

remove(kwic_test)


# LEXICAL DIVERSITY

# one thing that you might want to do, is to chech how different various documents are
# you can do that with the function "textstat_lexdiv()", which calculates various lexical diversity measures based on the number of unique types of tokens and the length of a document. It is useful, for instance, for analysing speakers’ or writers’ linguistic skills, or the complexity of ideas expressed in documents.

tstat_lexdiv <- quanteda_texts_dfm %>%
  textstat_lexdiv()


# textstat_dist() calculates similarities of documents or features for various measures. The output is compatible with R’s dist(), so hierarchical clustering can be performed without any transformation.

tstat_dist <- quanteda_texts_dfm %>% 
  textstat_dist() %>%
  as.dist()

tstat_dist %>%
  hclust() %>%
  plot(xlab = "Distance", ylab = NULL)
