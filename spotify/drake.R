#Drake
token <- "Bearer BQAYlQ6rrszMwzXhJgVswYJx9vENzsB_27eZZCRVaFC487Qezu_BNzAwoqpa3hM2dlnizYzhV1Ko8v6OBLE"

# Get albums Ids
responsealbums <- GET("https://api.spotify.com/v1/artists/3TVXtAsR1Inumwj472S9r4/albums/?include_groups=album&country=US", add_headers(Authorization = token))

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
ggplot(aes(x = tempo, y = popularity),
       data = dados_join) +
  geom_point() + 
  facet_wrap(~album)

#gráfico
ggplot(aes(x = duration_ms / 60000, y = popularity),
       data = dados_join) +
        geom_point()
  
describe(dados_join)       
