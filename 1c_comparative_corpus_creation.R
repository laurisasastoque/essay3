# Data cleaning

# Import packages ----

library(tidyverse)
library(tidytext)
library(readtext)
library(readxl)
library(syuzhet)

# set seed
set.seed(20022901)

# load required objects
load("corpus_comparative/comparative_corpus_metadata.rda")
comparative_metadata <- comparative_songs_tibble 
rm(comparative_songs_tibble)



# Multiple .txt files ----------

# if you have more than one text, you probably won't want to repeat this operations manually several times.
# you can then proceed as follows:
# (this is just one way but there are many out there)

# run the "readtext" function from the "readtext" package, simply indicating the folder in which your texts are stored, and the format preceded by "*." (this means "all files that have this extension").

# the corpus we are using here is the ELTEC UK collection, available online.
# because 100 texts require quite a lot of processing effort, for this practice
# we have scaled it down to 5

# this code creates a random list of 5 of the files inside the folder 'corpus"

files_list <- list.files("corpus_comparative", full.names = T, pattern = "txt") 

corpus_comparative <- readtext(files_list, encoding = "UTF-8") 

head(corpus_comparative)

# if we waned to reduce the corpus after having created it we could do this
# corpus <- corpus %>%
#   sample_n(size = 10)
# head(corpus)


# Split sentences -------

# for the moment, each row contains a whole book in text form, under the variable "text"
# we might what to split that into sentences

comparative_corpus_lines <- corpus_comparative %>%
  unnest_lines(input = "text",
                   output = "sentence",
                   to_lower = F, # for the moment we do not want to convert to lower case
                   drop = T) %>%
  filter(!grepl("\\[.*\\]", sentence)) %>% # we can spare memory and drop the 'full text', 
  # keeping only the new column" sentence"
  as_tibble()



# corpus3 <- corpus %>%  
#   sample_n(size = 3) %>%
#   mutate(text = str_sub(text, 1, 1000)) # we'll look at a subset to make it less intensive

library(udpipe)

comparative_corpus_udp <- udpipe(x = corpus_comparative$text, object = "spanish") %>% 
  filter(upos != "PUNCT") # remove punctiation from tokens

save(comparative_corpus_udp, file = "corpus_objects/comparative_corpus_udp.rda")

# now, as we mentioned you might want to use the information in the doc_id to create more variables (that's how "columns" are called in R) in our corpus
# alternatively, and maybe more efficiently, you can have a separate file where you store metadata.
# Just remember to make yure that the variable "doc_id" in the corpus and in the metadata correspond,
# otherwise you won' be able to match the data to the corpus.


# Add metadata ------

comparative_corpus_lines <- comparative_corpus_lines %>%
  mutate(doc_id = str_remove(doc_id, ".txt"))

comparative_corpus_lines <- comparative_corpus_lines %>%
  left_join(comparative_metadata, by = "doc_id")


# let's see how it looks

head(comparative_corpus_lines)

# Neat, right?

# you might also want to add an identification number for the sentences, which can be useful for later analysis

comparative_corpus_lines <- comparative_corpus_lines %>%
  group_by(doc_id) %>% # your doc_id must be always unique
  mutate(sentence_id = seq_along(sentence)) %>% # this means "sequence along the column sentence"
  ungroup()


view(head(comparative_corpus_lines))


# Tokenization -------

# we might want then to split the text into tokens.
# we can easily use the unnest_tokens function from tidytext:

comparative_corpus_token <- unnest_tokens(comparative_corpus_lines,
                              input = "sentence",
                              output = "token", 
                              to_lower = F, 
                              drop = F)

# 

comparative_corpus_ngrams <- unnest_ngrams(comparative_corpus_lines,
                               input = "sentence",
                               output = "ngram",
                               n = 5,
                               to_lower = F, 
                               drop = F)

# as we did for sentences, we might want to preserve the position of the tokens inside sentences, 
# and add a token_id index

comparative_corpus_token <- corpus_token %>%
  group_by(doc_id, sentence) %>%
  mutate(token_id = seq_along(token)) %>% # this means "sequence along the column "token"
  ungroup()


# so let's see how does it look now

head(comparative_corpus_token, 10)

save(comparative_corpus_ngrams, file = "corpus_objects/comparative_corpus_ngrams.rda")
save(comparative_corpus_ngrams, file = "corpus_objects/comparative_corpus_lines.rda")
save(comparative_corpus_ngrams, file = "corpus_objects/comparative_corpus_token.rda")

# splitting into tokes can be useful if we want to match our corpus to lists of 
# labelled data (for instance, locations or sentiment lexicons).
# We'll talk about this during the SA session.

