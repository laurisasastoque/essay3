# load packages
library("mallet")
library("ggwordcloud")
library("reshape2")

# 1. Prepare the dataset

# Define variables
num_topics <- 5 # number of topics
len_split <- 10000 # length of the split texts (they will be the actual documents to work on)
n_iterations <- 200 # number of iterations

# Prepare the corpus
tm_corpus <- list()
my_texts <- character()
file_list <- list.files("corpus_all", full.names = T)

# First loop: read file and tokenize them (in the easiest way)
for (i in 1:length(file_list)) {
  
  # read text
  loaded_file <- readLines(file_list[i], warn = F)
  loaded_file <- paste(loaded_file, collapse = "\n")
  
  # tokenize
  tm_corpus[[i]] <- unlist(strsplit(loaded_file, "\\W"))
  tm_corpus[[i]] <- tm_corpus[[i]][which(tm_corpus[[i]] != "")]
  # then add correct names to the different texts in the list
  # (we can re-use the names saved in the list_files variable, by deleting the "corpus/" at the beginning)
  names(tm_corpus)[i] <- gsub(pattern = "corpus_all/|.txt", replacement = "", x = file_list[i])
  
  # print progress
  print(i)
  
}

# visualize the output
tm_corpus[[1]]

# Second loop to generate final files
counter <- 1
for (i in 1:length(tm_corpus)) {
  # work on single text
  tokenized_text <- tm_corpus[[i]]
  # get total length
  len_limit <- length(tokenized_text)
  # use total length to get the number of times you can split it
  split_dim <- trunc(len_limit/len_split)
  # then do the actual splitting
  tokenized_text_split <- split(tokenized_text, ceiling(seq_along(tokenized_text)/len_split))
  # # last part will be shorter than the set length, so better merge it with the previous one
  # tokenized_text_split[[length(tokenized_text_split)-1]] <- c(tokenized_text_split[[length(tokenized_text_split)-1]], tokenized_text_split[[length(tokenized_text_split)]])
  # tokenized_text_split <- tokenized_text_split[-length(tokenized_text_split)]
  # then collapse back the split texts into a single string 
  tokenized_text_split <- unlist(lapply(tokenized_text_split, function(x) paste(x, collapse = " ")))
  # finally we perform a loop on the split texts to incrementally save all in just one variable 
  for (n in 1:length(tokenized_text_split)){
    # put to lowercase
    my_texts[counter] <- tolower(tokenized_text_split[n])
    # assign names
    names(my_texts)[counter] <- paste(names(tm_corpus)[i], n, sep = "_")
    # increase the counter (started as 1 from outside the loop)
    counter <- counter + 1
  }
  print(i)
}

# visualize the output
my_texts[1]

# 2. Topic modeling with Mallet

library(mallet) 

# preparation of texts for topic model
text.instances <- 
  mallet.import(text.array = my_texts, 
                stoplist = as.character(stopwords::stopwords("es")),
                id.array = names(my_texts))

# define all variables (better not to change alpha and beta)
topic.model <- MalletLDA(num.topics = num_topics, alpha.sum = 1, beta = 0.1)

# load documents for topic modeling
topic.model$loadDocuments(text.instances)

# create the topic models
topic.model$setAlphaOptimization(20, 50) # this is for optimization
topic.model$train(n_iterations) # number of iterations defined at the beginning

# calculate topic per document
doc.topics <- mallet.doc.topics(topic.model, smoothed = TRUE, normalized = TRUE)

# calculate topic per words
topic.words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)

# 3. Visualize the topics

# use a loop to visualize the top words per topic in a table
top_words <- data.frame()
firstwords <- character()
for (i in 1:num_topics) {
  words.per.topic <- mallet.top.words(topic.model, word.weights = topic.words[i,], num.top.words = 20)
  words.per.topic$topic <- i
  top_words <- rbind(top_words, words.per.topic)
  firstwords[i] <- paste(words.per.topic$words[1:5], collapse = " ")
}

# visualize the table
View(top_words)

# visualize the first five words per topic
names(firstwords) <- paste("Topic", 1:length(firstwords))
firstwords

# use the doc.topics to cluster the documents

# first assign names that correspond to:
# the first five words of the topics
colnames(doc.topics) <- firstwords
# the titles of the documents
rownames(doc.topics) <- names(my_texts) # to make them look better, remove "corpus" from the names

# visualize an heatmap and save it to a file
png(filename = "heatmap.png")
heatmap(doc.topics, margins = c(25,25), cexRow = 2, cexCol = 2)
dev.off()

# 3.1 (additional step) compress the heatmap by averaging topic values per text
library(tidyverse)
library(reshape2)

# first, we need to convert the matrix into a "tidy" dataframe
doc.topics <- as.data.frame(doc.topics)
doc.topics$chapter <- rownames(doc.topics)
doc.topics.m <- melt(doc.topics, id.vars = "chapter")
View(doc.topics.m)

# then we have to extract the titles
chapters_tmp <- strsplit(doc.topics$chapter, "_")
doc.topics.m$title <- sapply(chapters_tmp, function(x) paste(x[1:3], collapse = "_"))
View(doc.topics.m)

# so we can use tidyverse functions to calculate the mean topic value
doc.topics <- doc.topics.m %>%
  group_by(title, variable) %>%
  summarise(gamma = mean(value))

# then we have to re-convert the tidy table into a matrix for visualization
doc.topics <- dcast(data = doc.topics,
                    formula = title~variable,
                    fun.aggregate = sum,
                    value.var = "gamma")

my_rownames <- doc.topics$title
doc.topics$title <- NULL
doc.topics <- as.matrix(doc.topics)
rownames(doc.topics) <- my_rownames

# visualize/save the heatmap!
png(filename = "heatmap_2.png", width = 1000, height = 1000)
heatmap(doc.topics, margins = c(25,25), cexRow = 2, cexCol = 2)
dev.off()

# 4. Wordcloud visualization
# using a specific ggplot library
library(ggwordcloud)

# prepare the plot
p1 <- ggplot(
  top_words,
  aes(label = term, size = weight)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  theme_minimal() +
  facet_wrap(~topic)

# show it
p1

# save it
ggsave(p1, filename = "Topics_wordcloud.png", scale = 2.5)