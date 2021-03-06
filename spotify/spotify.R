library(tidyverse)
library(httr)
library(jsonlite)

token <- "Bearer BQB3AbLGABrotn_db1Ects9DLy9E5k0FAbCCjhPtZxQq-ZJwflxWY_ubJHgGXoqYz54bIpJtzxzfGBv7Rlo"

#Beyonce
# Get albums Ids
responsealbums <- GET("https://api.spotify.com/v1/artists/6vWDO969PvNqNYHIOW5v0m/albums/?include_groups=album&country=US", add_headers(Authorization = token))

albums_full <- content(responsealbums, as="text") %>% 
  fromJSON()

albums_info <- albums_full[['items']]

#Get Tracks Ids
dados_tracks <- data.frame()
tracks_info <- data.frame()
for (i in 1:length(albums_info$id)) {
  response_track <- GET(paste('https://api.spotify.com/v1/albums/', '/tracks/?limit=50', sep = albums_info$id[i]), add_headers(Authorization = token))
  tracks_full <- content(response_track, as="text") %>% 
    fromJSON()
  
  tracks_info <- tracks_full[['items']]
  print(tracks_info)
  dados_tracks <- rbind(dados_tracks, data.frame(id = tracks_info$id, name = tracks_info$name, stringsAsFactors = FALSE))
  
}

#ids as string
#create chuncks
list_tracks_ids <- split(dados_tracks$id, ceiling(seq_along(dados_tracks$id)/50))

#get pop of tracks
#Get Tracks popularity
dados_tracks <- data.frame()
for (i in 1:length(list_tracks_ids)) {
  url_tracks_ids <- paste(list_tracks_ids[[i]], collapse = ',')
  response_track_pop <- GET('https://api.spotify.com/v1/tracks', query = list(ids = url_tracks_ids), add_headers(Authorization = token))
  track_pop_full <- content(response_track_pop, as="text") %>% 
    fromJSON()
  
  track_pop_info <- track_pop_full[['tracks']]
  
  dados_tracks <- rbind(dados_tracks, data.frame(id = track_pop_info$id, popularity = track_pop_info$popularity, album =  track_pop_info$album$name ,stringsAsFactors = FALSE))
  
}

#get analysis tracks

dados <- data.frame()
for (i in 1:length(list_tracks_ids)) {
  url_tracks_ids <- paste(list_tracks_ids[[i]], collapse = ',')
  response_track_feature <- GET('https://api.spotify.com/v1/audio-features', query = list(ids = url_tracks_ids), add_headers(Authorization = token))
  track_feature_full <- content(response_track_feature, as="text") %>% 
    fromJSON()
  
  track_feature_info <- track_feature_full[['audio_features']]
  
  dados <- rbind(dados, track_feature_info)
  
}

dados_join <- inner_join(dados_tracks, dados)
dados_join <- inner_join(dados_join, dados_tracks)

#gráfico
ggplot(aes(x = valence, y = popularity),
       data = dados_join) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~album) 

ggplot(aes(x = time_signature, y = popularity),
       data = dados_join) +
  geom_bar()

#gráfico
ggplot(aes(x = duration_ms / 60000, y = popularity),
       data = dados_join) +
  geom_point()

describe(dados_join)       
