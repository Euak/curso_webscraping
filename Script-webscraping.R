library("tidyverse")
library("rvest")

url <- "https://www.metacritic.com/browse/tv/score/metascore/all/filtered?view=condensed&sort=desc"

dados <- data.frame()
for (i in 0:1) {
  url <- paste("https://www.metacritic.com/browse/tv/score/metascore/all/filtered?view=condensed&sort=desc&page=", sep = '',i)
  
  pagina <- read_html(url)
  
  nodes_shows <- html_nodes(pagina, xpath = "//div[@class='product_wrap']")
  
  nodes_link <- html_nodes(nodes_shows, xpath = "//div[@class='basic_stat product_title']/a") 
  nodes_metascore <- html_nodes(nodes_shows, xpath = "//div[@class='basic_stat product_score brief_metascore']/div") 
  nodes_userscore <- html_nodes(nodes_shows, xpath = "//div[@class='basic_stat condensed_stats']//span[contains(@class,'textscore')]") 
  
  titulos <- html_text(nodes_link) %>% 
    c()
  
  links <- html_attr(nodes_link, name = "href") %>%
    c()
  
  metascores <- html_text(nodes_metascore)
  
  userscores <- html_text(nodes_userscore)
  
  dados <- rbind(dados, data.frame(title = titulos, metascore = metascores, userscore = userscores, link = links, stringsAsFactors = F))
  
}

# Convertendo as notas como numéricos
dados$metascore <- as.numeric(dados$metascore)
dados$userscore <- as.numeric(dados$userscore)

# Removendo os line breaks e espaços no início e final das strings
dados$title <- trimws(dados$title)

glimpse(dados)

# Adicionando nossa métrica
dados <- dados %>% 
  mutate(combinescore = metascore * userscore)

# Adicionando coluna para o nome da série
dados <- dados %>% 
  mutate(title_series = sub(":\\sSeason.*", '', title))

# Avaliando nossa métrica
dados %>% 
  ggplot(aes(x = combinescore)) +
  geom_histogram()

dados %>% 
  group_by(title_series) %>% 
  summarise(media = mean(combinescore)) %>% 
  arrange(desc(media)) %>% 
  head(20)
