install.packages("rvest")
install.packages("dplyr")
install.packages("stringr")

library("rvest")
library("dplyr")
library("stringr")

url_base <- "https://www.al.sp.gov.br/alesp/pesquisa-proposicoes/?direction=acima&lastPage=44&currentPage=NUMPAG&act=detalhe&idDocumento=&rowsPerPage=20&currentPageDetalhe=1&tpDocumento=&method=search&text=merenda&natureId=&legislativeNumber=&legislativeYear=&natureIdMainDoc=&anoDeExercicio=&strInitialDate=&strFinalDate=&author=&supporter=&politicalPartyId=&tipoDocumento="
i <- "1"
url <- stringr::str_replace(url_base, "NUMPAG", i)
lista.tabelas <- xml2::read_html(url) %>% # read_html ira ler a url e retornar uma estrutura html
  rvest::html_table(header = T, fill = TRUE) # html_table ira procurar as tabelas na estrutura html e retornas cada uma delas estruturada em um data frame. Obs: O argumento header é para indicar que as tabelas tem nome nas colunas

print(tabela)

# Objetos variados
matriz <- matrix(c(1:6), nrow=2)
vetor.inteiros <- c(42:1)
vetor.texto <- c("a", "b", "c", "d", "e")
vetor.logico <- c(T, F, T, T, T, T, T, T, F)
texto <- "meu querido texto"
resposta <- 42

# Lista
minha.lista <- list(matriz, vetor.inteiros, vetor.texto, vetor.logico, texto, resposta)
print(minha.lista)

print(minha.lista[1:3])
class(minha.lista[1:3])
print(minha.lista[4])
class(minha.lista[4])

print(minha.lista[[4]])
class(minha.lista[[4]])

tabela <- lista.tabelas[[1]]

print(tabela[1,])

url_base <- "https://www.al.sp.gov.br/alesp/pesquisa-proposicoes/?direction=acima&lastPage=44&currentPage=NUMPAG&act=detalhe&idDocumento=&rowsPerPage=20&currentPageDetalhe=1&tpDocumento=&method=search&text=merenda&natureId=&legislativeNumber=&legislativeYear=&natureIdMainDoc=&anoDeExercicio=&strInitialDate=&strFinalDate=&author=&supporter=&politicalPartyId=&tipoDocumento="
for (i in 0:4) {
  url <- stringr::str_replace(url_base, "NUMPAG", as.character(i))
  lista.tabelas <- xml2::read_html(url) %>% rvest::html_table(header = T, fill = TRUE)
  tabela <- lista.tabelas[[1]]
  print(head(tabela))
}

# Criando 2 data frames separados
meus.dados1 <- data.frame("id" = 1:10, "Experimento" = rep(c("Tratamento"), 10))
print(meus.dados1)
meus.dados2 <- data.frame("id" = 11:20, "Experimento" = rep(c("Controle"), 10))
print(meus.dados2)

# Combinando os dois data.frames
meus.dados.completos <- dplyr::bind_rows(meus.dados1, meus.dados2)
print(meus.dados.completos)

url_base <- "https://www.al.sp.gov.br/alesp/pesquisa-proposicoes/?direction=acima&lastPage=44&currentPage=NUMPAG&act=detalhe&idDocumento=&rowsPerPage=20&currentPageDetalhe=1&tpDocumento=&method=search&text=merenda&natureId=&legislativeNumber=&legislativeYear=&natureIdMainDoc=&anoDeExercicio=&strInitialDate=&strFinalDate=&author=&supporter=&politicalPartyId=&tipoDocumento="
dados <- data.frame()
for (i in 0:4) {
  print(i)
  url <- stringr::str_replace(url_base, "NUMPAG", as.character(i))
  lista.tabelas <- xml2::read_html(url) %>% rvest::html_table(header = T, fill = TRUE)
  tabela <- lista.tabelas[[1]]
  dados <- dplyr::bind_rows(dados, tabela)
}

# Estrutura do data frame
dplyr::glimpse(dados)

# 6 primeiras observações
head(dados)

# 6 últimas observações
tail(dados)

#sugestão para limpar a tabela
dados_teste[dados_teste==""] <- NA
dados_teste <- dados_teste %>% na.omit()


rm(list = ls())

library(rvest)

url <- "http://www.al.sp.gov.br/alesp/busca/?q=merenda&page=2"

pagina <- readLines(url)

class(pagina)

pagina <- read_html(url)

class(pagina)

pagina <- xml_root(pagina)

nodes_link <- xml_find_all(pagina, "//ul[@class='lista_navegacao']/li/a")

print(nodes_link)

#atualização
url <- "https://www.al.sp.gov.br/alesp/noticias/?inicio=20&fim=40&textoCodificado=merenda&tituloCodificado=&resumoCodificado=&autorCodificado=&texto=merenda&midia=N&autoria=&data=&dataFim=&titulo=&resumo=&autor="

pagina <- read_html(url)

pagina <- xml_root(pagina)

nodes_link <- xml_find_all(pagina, "//div[@id='resultado-noticia']//h5/a")

print(nodes_link)

View(nodes_link[[1]])

conteudo_1 <- xml_text(nodes_link[[1]])

print(conteudo_1)

atributo_1 <- xml_attr(nodes_link[[1]], attr = "href")

print(atributo_1)

conteudos <- c()
atributos <- c()
for (i in 1:10){
  print(i)
  conteudo_i <- xml_text(nodes_link[[i]])
  conteudos <- c(conteudos, conteudo_i)
  
  atributo_i <- xml_attr(nodes_link[[i]], attr = "href")
  atributos <- c(atributos, atributo_i)
}

dados <- data.frame(conteudos, atributos)
head(dados)

library(tidyverse)
conteudos <- map(nodes_link, xml_text)

print(conteudos)

conteudos <- map_chr(nodes_link, xml_text)

print(conteudos)

atributos <- map_chr(nodes_link, xml_attr, attr = "href")

print(atributos)

dados <- data.frame(conteudos, atributos)
head(dados)


urlbase <- "https://www.al.sp.gov.br/alesp/noticias/?textoCodificado=merenda&inicio="

dados <- tibble()

for (i in 0:30){
  print(i)
  url <- paste(urlbase, paste(0+20, 0+40, sep = "&fim="), sep = "")
  
  pagina <- read_html(url)
  pagina <- xml_root(pagina)
  
  nodes_link <- xml_find_all(pagina, "//div[@id='resultado-noticia']//h5/a")
  
  conteudos <- map_chr(nodes_link, xml_text)
  
  atributos <- map_chr(nodes_link, xml_attr, attr = "href")
  
  dados <- bind_rows(dados, data.frame(conteudos, atributos))
  
}

# Atividade 3

library(rvest)
library(dplyr)

url <- "http://datafolha.folha.uol.com.br/eleicoes/2016/02/1744581-49-nao-votariam-em-lula.shtml"

pagina <- xml2::read_html(url)

xml2::xml_structure(pagina)

titulo <- rvest::html_node(pagina ,
                           xpath = '//h1[@class = "main_color main_title"]') %>% 
  rvest::html_text()
print(titulo)

datahora <- rvest::html_node(pagina ,
                             xpath = '//time') %>% 
  rvest::html_text()
print(datahora)

pesquisa <- rvest::html_node(pagina ,
                             xpath = '//p[@class = "stamp download"]/a') %>% 
  rvest::html_attr("href")
print(pesquisa)

texto <- rvest::html_node(pagina ,
                          xpath = '//article[@class = "news"]') %>% 
  rvest::html_text()

print(texto)

texto <- rvest::html_nodes(pagina ,
                           xpath = '//article[@class = "news"]/p') %>% 
  rvest::html_text()
print(texto)

url<- "http://datafolha.folha.uol.com.br/eleicoes/2015/11/1701573-russomanno-larga-na-frente-em-disputa-pela-prefeitura-de-sp.shtml"

pagina <- xml2::read_html(url)

xml2::xml_structure(pagina)

titulo <- rvest::html_node(pagina ,
                           xpath = '//h1[@class = "main_color main_title"]') %>% 
  rvest::html_text()
print(titulo)

datahora <- rvest::html_node(pagina ,
                             xpath = '//time') %>% 
  rvest::html_text()
print(datahora)

pesquisa <- rvest::html_node(pagina ,
                             xpath = '//p[@class = "stamp download"]/a') %>% 
  rvest::html_attr("href")
print(pesquisa)

texto <- rvest::html_node(pagina ,
                          xpath = '//article[@class = "news"]') %>% 
  rvest::html_text()

print(texto)

texto <- rvest::html_nodes(pagina ,
                           xpath = '//article[@class = "news"]/p') %>% 
  rvest::html_text()
print(texto)

url <- "http://datafolha.folha.uol.com.br/eleicoes/2016/02/1744581-49-nao-votariam-em-lula.shtml"
pagina <- read_html(url)
pesquisa <- rvest::html_nodes(pagina ,
                              xpath = '//p[@class = "stamp download"]/a') %>% 
  rvest::html_attr("href") %>% 
  str_replace_all('\\\\','/')

print(pesquisa)

getwd()
download.file(pesquisa, "pesquisa.pdf")

url <- "http://search.folha.uol.com.br/search?q=elei%C3%A7%C3%B5es&site=datafolha&sr=26&skin=datafolha&results_count=668&search_time=0%2C103&url=http%3A%2F%2Fsearch.folha.uol.com.br%2Fsearch%3Fq%3Delei%25C3%25A7%25C3%25B5es%26site%3Ddatafolha%26sr%3D51%26skin%3Ddatafolha"

pagina <- read_html(url)

link <- rvest::html_nodes(pagina, xpath = '//h2[@class = "title"]/a') %>% 
  rvest::html_attr("href")

links_datafolha <- c()
for (i in 1:21){
  print(i)
  i <- (i - 1) * 25 + 1
  url <- stringr::str_replace(url, "CONTADORLINK", as.character(i))
  pagina <- read_html(url)
  link <- rvest::html_nodes(pagina,
                            xpath = '//h2[@class = "title"]/a') %>% 
    rvest::html_attr("href")
  links_datafolha <- c(links_datafolha, link)
}

head(links_datafolha)

dados <- data.frame()
for (link in links_datafolha){
  print(link)
  pagina <- read_html(link)
  titulo <-  rvest::html_node(pagina, xpath = '//h1[@class = "main_color main_title"]') %>%
    rvest::html_text()
  datahora <- rvest::html_node(pagina, xpath = '//time') %>%
    rvest::html_text()
  texto <- rvest::html_node(pagina, xpath = "//article[@class = 'news']") %>%
    rvest::html_text()
  dados <- rbind(dados, data.frame(titulo, datahora, texto))
}

nullToNA <- function(x) {
  if (is.null(x)){
    return(NA)
  } else {
    return(x)
  }
}

texto_vetor <- rvest::html_nodes(pagina ,
                                 xpath = '//article[@class = "news"]/p') %>% 
  rvest::html_text()
texto <- c()
for (paragrafo in texto_vetor){
  texto <- paste(texto, paragrafo)
}
texto <- nullToNA(texto)
