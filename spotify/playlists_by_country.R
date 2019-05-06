library(tidyverse)
library(httr)
library(jsonlite)

token <- "Bearer BQBGeBDqpu95wYdvHJjqZww3CKr4CTY5a65hAAvQkP3P38zaAvfncQl0G3uthprGxChhbxhxxFYXDYOsnak"

playlists_id <- c("37i9dQZEVXbMDoHDwVN2tF",
                  "37i9dQZEVXbMZAjGMynsQX",
                  "37i9dQZEVXbMXbN3EUUhlg",
                  "37i9dQZEVXbLRQDuF5jeBp",
                  "37i9dQZEVXbJPcfkRz0wJ0",
                  "37i9dQZEVXbM8SIrkERIYl",
                  "37i9dQZEVXbLnolsZ8PSNw",
                  "37i9dQZEVXbIPWwFssbupI",
                  "37i9dQZEVXbJiZcmkrIHGU",
                  "37i9dQZEVXbMH2jvi6jvjk",
                  "37i9dQZEVXbKXQ4mDTEBXq",
                  "37i9dQZEVXbMnZEatlMSiu",
                  "37i9dQZEVXbLZ52XmnySJg")

playlists_country <- c("Global",
                       "Costa Rica",
                       "Brasil",
                       "EUA",
                       "Austrália",
                       "Nova Zelândia",
                       "Reino Unido",
                       "França",
                       "Alemanha",
                       "Africa do Sul",
                       "Japão",
                       "Taiwan",
                       "Índia")

playlists <- data.frame(id = playlists_id, country = playlists_country, stringsAsFactors = FALSE)

#Get Tracks Ids
dados_tracks <- data.frame()
tracks_info <- data.frame()

for (i in 1:nrow(playlists)) {
  response_tracks <- GET( paste('https://api.spotify.com/v1/playlists/', '/tracks', sep = playlists$id[i]), add_headers(Authorization = token))
  response_tracks_parsed <- content(response_tracks, as="text") %>% 
    fromJSON()
  
  tracks_info <- response_tracks_parsed[['items']]$track
  
  dados_tracks <- rbind(dados_tracks, data.frame(id = tracks_info$id, name = tracks_info$name, playlist = playlists$country[i], stringsAsFactors = FALSE))
}

#ids as string
#create chuncks
list_tracks_ids <- split(dados_tracks$id, ceiling(seq_along(dados_tracks$id)/50))

#get analysis tracks
dados_analysis <- data.frame()
for (i in 1:length(list_tracks_ids)) {
  url_tracks_ids <- paste(list_tracks_ids[[i]], collapse = ',')
  response_track_feature <- GET('https://api.spotify.com/v1/audio-features', query = list(ids = url_tracks_ids), add_headers(Authorization = token))
  track_feature_full <- content(response_track_feature, as="text") %>% 
    fromJSON()
  
  track_feature_info <- track_feature_full[['audio_features']]
  
  dados_analysis <- rbind(dados_analysis, track_feature_info)
  
}

dados <- inner_join(dados_analysis, dados_tracks, by = "id")

dados <- dados %>% 
  distinct(name, playlist, .keep_all = T)

dados <- group_by(dados, playlist)

summarise(dados, valence = mean(valence)) %>% 
  ggplot(aes(x = playlist, y = valence)) +
  geom_col()
