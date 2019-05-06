library(tidyverse)
library(httr)
library(jsonlite)

token <- "Bearer BQAMDVEOKUmTOc-lvJn47AwtSkRFo30rTGyA6gAGy-AxJOXFajfJCoyttk6SJoZM63D3rvdFyygaVjm11pg"

#Beyonce
# Get albums Ids
responsealbums <- GET("https://api.spotify.com/v1/artists/6vWDO969PvNqNYHIOW5v0m/albums/?include_groups=album&country=US", add_headers(Authorization = token))

albums_full <- content(responsealbums, as="text") %>% 
  fromJSON()

albums_info <- albums_full[['items']]

#Get Albuns full info
url_albums_ids <- paste(albums_info$id, collapse = ',')
response_albums <- GET('https://api.spotify.com/v1/albums', query = list(ids = url_albums_ids), add_headers(Authorization = token))
response_albums_parsed <- content(response_albums, as="text") %>% 
  fromJSON()
  
album_full_info <- response_albums_parsed[['albums']]

album_full_info$release_year <- substr(album_full_info$release_date, 1,4)

album_full_info$release_year <- as.character(album_full_info$release_year)

album_full_info$release_year

ggplot(aes(x = release_year, y = popularity, color = name),
       data = album_full_info) +
  geom_point() +
  geom_smooth()
