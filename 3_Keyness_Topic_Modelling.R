# Keyness analysis & Topic Modelling


# call the packages
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(tidyverse)
library(readtext)
library(topicmodels)
library(ggplot2)
library(tidytext)


# load the tokenized texts
load("quanteda_texts_tok.Rdata")

# let#s see which authors we have in here

list(names(quanteda_texts_tok))

quanteda_texts_dfm <- dfm(quanteda_texts_tok)

useless_words <- readtext("useless_words.txt", encoding = "UTF-8") %>% 
  tidytext::unnest_lines(input = "text", output = "words")


# Plot “keyness” in a target and reference group
# If you want to compare the differential associations of keywords in a target and reference group, you can calculate “keyness” which is based on textstat_keyness. In this example, we compare the texts by women with those written by men.

# Create a dfm grouped by gender

gender_dfm <- tokens(quanteda_texts_tok, remove_punct = TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(stopwords("es")) %>%
  tokens_remove(useless_words$words) %>%
  tokens_group(groups = is_male) %>%
  dfm()

# Calculate keyness and determine gender as target group
result_keyness <- textstat_keyness(gender_dfm)

# Plot estimated word keyness
textplot_keyness(result_keyness) 


# obviously doing this focusing on gender is not really meaningful: typically you would look at one author vs the rest, but you would need at least two texts by a single author.

# for instance, we can try with Gaskell vs the rest

tstat_key <- quanteda_texts_dfm %>% 
  textstat_keyness(target = grepl("Young Miko", quanteda_texts_dfm@docvars$docname_))

textplot_keyness(tstat_key)


# TOPIC MODELLING

# basic topic modelling with quanteda can be fairly easy, you just need to have a dfm and run the function textmodel_lda()

library(seededlda)
library(topicmodels)

# this might take quite some time so i saved it for you, you can just load it below

tmod_lda <- textmodel_lda(quanteda_texts_dfm, k = 5)
save(tmod_lda, file = "tmod_lda.RData")

load("tmod_lda.RData")

terms(tmod_lda, 10)



# but let's change focus and try with a different corpus

clean_dfm <- tokens(quanteda_texts_tok, remove_punct = TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(stopwords("es")) %>%
  tokens_remove(useless_words$words) %>%
  dfm()

# Convert to matrix
dtm <- as.matrix(clean_dfm)

# Fit LDA model
set.seed(123)
lda <- LDA(dtm, k = 5, control = list(seed = 1234))


# Visualize the topics
topics <- tidy(lda, matrix = "beta")
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ggplot(top_terms, aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  labs(x = NULL, y = "Beta") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Extract topic distribution for each document
doc_topics <- tidy(lda, matrix = "gamma", document_names = rownames(dtm))

# Plot the distribution of topics across documents
doc_topics %>%
  ggplot(aes(x = topic, y = gamma, fill = factor(topic))) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(width = 0)
