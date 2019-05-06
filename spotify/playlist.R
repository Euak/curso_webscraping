#Charts
token <- "Bearer BQAYlQ6rrszMwzXhJgVswYJx9vENzsB_27eZZCRVaFC487Qezu_BNzAwoqpa3hM2dlnizYzhV1Ko8v6OBLE"

#Get Tracks Ids
dados_tracks <- data.frame()
tracks_info <- data.frame()

response_tracks <- GET('https://api.spotify.com/v1/playlists/37i9dQZEVXbMDoHDwVN2tF/tracks', add_headers(Authorization = token))
response_tracks_parsed <- content(response_tracks, as="text") %>% 
  fromJSON()

tracks_info <- response_tracks_parsed[['items']]
dados_tracks <- rbind(dados_tracks, data.frame(id = tracks_info$track$id, name = tracks_info$track$name, stringsAsFactors = FALSE))

#ids as string
#create chuncks
list_tracks_ids <- split(dados_tracks$id, ceiling(seq_along(dados_tracks$id)/50))

#get pop of tracks
#Get Tracks popularity
dados_pop <- data.frame()
for (i in 1:length(list_tracks_ids)) {
  url_tracks_ids <- paste(list_tracks_ids[[i]], collapse = ',')
  response_track_pop <- GET('https://api.spotify.com/v1/tracks', query = list(ids = url_tracks_ids), add_headers(Authorization = token))
  track_pop_full <- content(response_track_pop, as="text") %>% 
    fromJSON()
  
  track_pop_info <- track_pop_full[['tracks']]
  
  dados_pop <- rbind(dados_pop, data.frame(id = track_pop_info$id, popularity = track_pop_info$popularity, album =  track_pop_info$album$name ,stringsAsFactors = FALSE))
  
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
dados_join <- inner_join(dados_join, dados_pop)

#gráfico
ggplot(aes(x = acousticness, y = popularity),
       data = dados_join) +
  geom_point()

#gráfico
ggplot(aes(x = duration_ms / 60000, y = popularity),
       data = dados_join) +
  geom_point()

describe(dados_join)       
