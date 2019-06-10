

##Setando ambiente com os arquivos de apoio.
setwd("C:/Users/Eric/Desktop/Arquivos_R")
getwd()

## Incluindo as bibliotecas nescessárias

library(tidyverse)
library(data.table)
library(tidytext)
library(glue)
library(stringr)
library(stringi)
library(rvest)
library(readr)
library(ptstem)
library(wordcloud2)
library(tm)
library(tokenizers)
library(odbc)
library(tidyselect)
library(tinytex)
library(wordcloud)

#1 Carregando as stopwords

#Stopwords HTML
stopwordsPage <- read_html("C:/Users/Eric/Desktop/Arquivos_R/BrazilianStopwords.html", enconding="UTF-8")
stopwordsList <- html_nodes(stopwordsPage,'td') 

#Limpando o html, trocando br por tags
xml_find_all(stopwordsList, ".//br") %>% xml_add_sibling("p","\n")
xml_find_all(stopwordsList, ".//br") %>% xml_remove()

swstr <- html_text(stopwordsList)

#Transformar em um dicionário
sw <- unlist(str_split(swstr,'\\n')) 
glimpse(sw)


#Recuperando outro stopwords em portugues do TM

#carregando o stopwords da tm
swList2 <- stopwords('portuguese')
glimpse(swList2)

  
#Fazendo um merge nos dados

str(sw)
sw_merged <- union(sw,swList2) 
summary(sw_merged)

#Verificando Duplicidades
tibble(word = sw_merged) %>% 
  group_by(word) %>% 
  filter(n()>1)



#2 Carregando os termos de polaridade de análise de Sentimentos em uma tabela geral

an <- read.csv("ADJ_negativos.txt", header = F, sep = "\t", strip.white = F,
               stringsAsFactors = F, encoding="UTF-8")

exn <- read.csv("MWE_negativos.txt", header = F, sep = "\t", strip.white = F,
                stringsAsFactors = F, encoding="UTF-8")

vn <- read.csv("verbos_negativos.txt", header = F, sep = "\t", strip.white = F, 
               stringsAsFactors = F, encoding="UTF-8")

subn <- read.csv("subst_negativos.txt", header = F, sep = "\t", strip.white = F, 
                 stringsAsFactors = F, encoding="UTF-8")

ap <- read.csv("ADJ_positivos.txt", header = F, sep = "\t", strip.white = F, 
               stringsAsFactors = F, encoding="UTF-8")

exp <- read.csv("MWE_positivos.txt", header = F, sep = "\t", strip.white = F, 
                stringsAsFactors = F, encoding="UTF-8")

vp <- read.csv("verbos_positivos.txt", header = F, sep = "\t", strip.white = F, 
               stringsAsFactors = F, encoding="UTF-8")

sp <- read.csv("subst_positivos.txt", header = F, sep = "\t", strip.white = F, 
               stringsAsFactors = F, encoding="UTF-8")


str(an);str(exn)




#3 Carregando o lexico de sentimentos disponível no Kaggle
#from kaggle sentiment words https://www.kaggle.com/rtatman/sentiment-lexicons-for-81-languages/data

poskaggle <- read.csv("positive_words_pt.txt", header = F, sep = "\t", strip.white = F, 
                      stringsAsFactors = F, encoding="UTF-8")

negkaggle <- read.csv("negative_words_pt.txt", header = F, sep = "\t", strip.white = F, 
                      stringsAsFactors = F, encoding="UTF-8")

#verificando se está tudo certo
head(negkaggle)
head(poskaggle)


##decidi salvar esses termos em uma tabela e classificalos como tipo e polaridade que sabe 
##depois eu grave um score para o peso da polaridade
##Criando um dataframe que salvarei os termos começando pelos adjetivos negativos
dfPolaridades <- an %>% 
  mutate(word = V1, polaridade = -1, tipo='adjetivo', sentimento='negativo') %>%
  select(word,polaridade,tipo,sentimento) %>%
  arrange(word)
head(dfPolaridades)


##aqui faço um count para poder adicionar os dados corretamente
icount <-  length(exn$V1)
dfPolaridades <- bind_rows(dfPolaridades,list(word = exn$V1, polaridade=rep(-1,icount),tipo=rep('expressao',icount),sentimento=rep('negativo',icount)))
dfPolaridades %>% arrange(desc(word)) %>% head(3)




icount <-  length(vn$V1)
dfPolaridades <- bind_rows(dfPolaridades,list(word = vn$V1, polaridade=rep(-1,icount),tipo=rep('verbo',icount)
                                              ,sentimento=rep('negativo',icount)))

icount <-  length(subn$V1)
dfPolaridades <- bind_rows(dfPolaridades,list(word = subn$V1, polaridade=rep(-1,icount),tipo=rep('substantivo',icount)
                                              ,sentimento=rep('negativo',icount)))

icount <-  length(negkaggle$V1)
dfPolaridades <- bind_rows(dfPolaridades,list(word = negkaggle$V1, polaridade=rep(-1,icount),tipo=rep('noclass',icount)
                                              ,sentimento=rep('negativo',icount)))

icount <-  length(ap$V1)
dfPolaridades <- bind_rows(dfPolaridades,list(word = ap$V1, polaridade=rep(1,icount),tipo=rep('adjetivo',icount)
                                              ,sentimento=rep('positivo',icount)))

icount <-  length(exp$V1)
dfPolaridades <- bind_rows(dfPolaridades,list(word = exp$V1, polaridade=rep(1,icount),tipo=rep('expressao',icount)
                                              ,sentimento=rep('positivo',icount)))

icount <-  length(vp$V1)
dfPolaridades <- bind_rows(dfPolaridades,list(word = vp$V1, polaridade=rep(1,icount),tipo=rep('verbo',icount)
                                              ,sentimento=rep('positivo',icount)))

icount <-  length(sp$V1)
dfPolaridades <- bind_rows(dfPolaridades,list(word = sp$V1, polaridade=rep(1,icount),tipo=rep('substantivo',icount)
                                              ,sentimento=rep('positivo',icount)))

icount <-  length(poskaggle$V1)
dfPolaridades <- bind_rows(dfPolaridades,list(word = poskaggle$V1, polaridade=rep(1,icount),tipo=rep('noclass',icount)
                                              ,sentimento=rep('positivo',icount)))



#visualizando como está nosso dataframe
dfPolaridades %>% group_by(word) %>% filter(n() == 1) %>% summarize(n=n())
dfPolaridades %>% count()

#0.5 Removendo termos de sentimentos repetidos

dfPolaridadesUnique <- dfPolaridades[!duplicated(dfPolaridades$word),]
dfPolaridadesUnique %>% count()




#BASE QUE SERÁ ANALISADA

con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "XXXXXXXX", 
                      Database = "XXXXXXXX", Port = 1433)


x <- dbSendQuery(con, "SELECT TOP (10000) Message
                        FROM XXXXXXX
                 ")


result <- dbFetch(x, n = -1)

tibble(result)


#0.6 Processo de limpeza
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
#resultUtf <-  readr::parse_character(result, locale = readr::locale('pt')) # sapply(tweetxt, function(x) iconv(x, "UTF-8"))

resultUtf <- sapply(result, function(x) stri_trans_tolower(x,'pt'))
resultUtf <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ",  resultUtf);
resultUtf <- str_replace(resultUtf,"RT @[a-z,A-Z]*: ","")
resultUtf <- gsub("@\\w+", "", resultUtf)
resultUtf <- removeURL(resultUtf)
resultUtf <- str_replace_all(resultUtf,"@[a-z,A-Z]*","")  
resultUtf <- gsub("[^[:alnum:][:blank:]!?]", " ", resultUtf)
resultUtf <- gsub("[[:digit:]]", "", resultUtf)

tibble(resultUtf,10)


#0.6.1 Removendo Mensagens Repetidas
length(resultUtf)
tibble(resultUtf) %>% unique() %>% count()
resultUtfUnique <- resultUtf %>% unique() 
length(resultUtfUnique)

#0.6.2 Remoção das stopwords
resultUtfUniqueSw <- tm::removeWords(resultUtfUnique,c(sw_merged,'rt'))
tibble(resultUtfUniqueSw)

#0.7 Nuvem de palavras

ttokens <- data_frame(word= resultUtfUniqueSw) %>% unnest_tokens(word,word)
ttokens %>% count(word, sort = T) 

ttokens_filter <- ttokens %>% filter(nchar(word) > 3)
ttokens_filter %>% count(word, sort=T)


#0.8 Wordclouds
ttokens_freq <- ttokens_filter %>% count(word, sort = T) %>% select(word, freq=n) 
ttokens_freq

#wordcloud2(ttokens_freq , minSize = 2, size = 1, backgroundColor = 'black')

pal2 <- brewer.pal(8,"Dark2")

wordcloud(words =  ttokens_freq$word, freq = ttokens_freq$freq , min.freq = 50,  random.color = T, max.word = 100
          ,random.order = F, colors = pal2 ,scale=c(2,.5) 
          ,use.r.layout=FALSE,fixed.asp=TRUE)


#wordcloud2(ttokens_freq %>% filter(freq<100) , minSize = 6, size = 1, backgroundColor = 'black')


