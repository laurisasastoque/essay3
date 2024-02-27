# spotify pull

# install 'spotifyr' package
# install.packages('spotifyr')

# load packages
library(spotifyr)
library(tidyverse)
library(knitr)

# authentication
Sys.setenv(SPOTIFY_CLIENT_ID = 'cf5c3ab1f67543fc84591de20e2fe5f4')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '0bf44ccc0ac64ee695cd95eab4656023')

access_token <- get_spotify_access_token()

young_miko <- get_artist_audio_features('young miko',
                                        include_groups = c("album", "single", "compilation"))

# what is the most popular song
young_miko %>% 
  arrange(-valence) %>% 
  select(track_name, valence) %>% 
  head(5) 

young_miko_metadata <- young_miko %>% 
  select(artist_name, album_release_date, album_release_year, danceability, energy, key, explicit, 
         track_name) 

save(young_miko_metadata, file = "corpus/young_miko_metadata.rda")

ggplot(young_miko_metadata, aes(explicit)) +
  geom_bar() +
  theme_minimal()

young_miko_metadata %>% 
  group_by(album_release_year) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

ggplot(young_miko_metadata %>% head(5), aes(track_name, y = danceability)) +
  geom_point()

  