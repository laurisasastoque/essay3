# fixing mistakes
load("corpus/corpus_metadata.rda")
metadata <- songs_tibble 
rm(songs_tibble)


load("corpus_comparative/comparative_corpus_metadata.rda")
comparative_metadata <- comparative_songs_tibble 
rm(comparative_songs_tibble)

metadata <- metadata %>% 
  mutate(is_male = 0)

comparative_metadata <- comparative_metadata %>% 
  mutate(is_male = 1)

metadata_all <- rbind(metadata, comparative_metadata)

save(metadata_all, file = "corpus_all/metadata_all.rda")
