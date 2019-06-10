#Caminho Arquivos de apoio
setwd("C:/Users/Eric/Desktop/Arquivos_R")
getwd()

#Carregando as polaridades salvas na primeira parte do artigo

polaridades_pt <- dfPolaridadesUnique #read_csv('parte2/polaridades_pt.csv')
stopwordslist <- sw_merged #read_csv('stopwordsfinal.csv')
stopwordslist





#Trabalhando com a TM Package
#Criando o corpus

library(tm)
library(gridExtra)
library(igraph)
library(quanteda)
library(tidytext)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(topicmodels)


resultencoded <- sapply(resultUtfUnique,enc2native)
df <- data.frame(text=resultencoded)
head(df)

#Criando um id para o documento, isso vai ser util na conversão
df$doc_id <- row.names(df)
head(df)

tm_corpus <- Corpus(DataframeSource(df))
inspect(tm_corpus[1:4])

#criando o dtm usando palavras que tem mais de 3 letras e uma frequencia >19
dtm <- DocumentTermMatrix(tm_corpus, control=list(wordLengths=c(4, 20),language=locale('pt')
                                                  , stopwords=stopwords('portuguese'),
                                                  bounds = list(global = c(30,500))))
dtm

#podemos ver os termos mais frequentes
findFreqTerms(dtm)

ttm_results <- t(as.matrix(dtm)) %*% as.matrix(dtm)
head(ttm_results)


#construindo um grafico da matriz acima
g <- graph.adjacency(ttm_results, weighted=T, mode = 'undirected')
g <- simplify(g)

#setando o label e degree dos vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
E(g)$color <- ifelse(E(g)$wheight > 30, "green", "red")

set.seed(50)
layout1 <- layout_on_sphere(g)
#plot(g, layout=layout1)
#tkplot(g, layout=layout.drl)
#plot(g, layout=layout.kamada.kawai)
plot(g, layout=layout.fruchterman.reingold)


#melhorando a apresentação de acordo com o peso dos termos
V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
plot(g, layout=layout1)


#Usando a package Quanteda
#Apresentarei a vocês agora uma outra maneira de criar matrizes de termo e documentos
#Note que no quanteda não precisa fazer um encode de todo o texto

#vou qualificar os dados para futuramente fazer alguma coisa com isso
twdf <- tibble(resultq = resultUtfUnique)
twdf$whois <- NA
twdf$whois[twdf$resultq %like% 'valor'] <- 'valor'
twdf$whois[twdf$resultq %like% 'fatura'] <- 'fatura'
twdf$whois[twdf$resultq %like% 'internet'] <- 'internet'
twdf$whois[twdf$resultq %like% 'fixo'] <- 'fixo'
twdf$whois[twdf$resultq %like% 'conta'] <- 'conta'
twdf$whois[twdf$resultq %like% 'total'] <- 'total'
twdf$whois[is.na(twdf$whois) ] <- 'semtag'
freq <- twdf %>% count(whois, sort = T) %>% select( whois,freq = n) 
freq

#PLOT

#hist(freq$freq)
#plot(density(freq$freq))

pie(table(twdf$whois))
barplot(table(twdf$whois))
distMatrix <- as.matrix(dist(freq$freq))
plot(density(distMatrix))



#criando o dtm
dfq <- data.frame(id=row.names(twdf),
                  text=twdf$resultq, whois = factor(twdf$whois))

myCorpus <- corpus(twdf,  text_field = 'resultq', 
                   metacorpus = list(source = "fixo")) 

myCorpus


head(textstat_readability(myCorpus,"all"),2)


#observando
summary(myCorpus,6)

#para acessar qq parte da matriz use a função texts
texts(myCorpus)[28:30]

summary(corpus_subset(myCorpus, whois == 'conta'),6)

#A função kwic (keywords in context) procura o texto e nos mostra uma forma visual da matrix
kwic(myCorpus,'fixo')

#com o quanteda tambem podemos tokenizar o texto, vamos pegar por exemplo nossos tweets originais
temptok <- tokens(resultUtfUnique)
#note o objeto token
temptok[1:5]


##O Document-feature matrix

remove(temptok)
#Agora vamos usar a principal funçao do quanteda a dfm, que transforma o corpus em um 
#documento termo matriz e ao contrario da funçao tokens ela aplica varios funções de limpeza como
#retirar pontuação, converter para minusculo, para saber mais consulte a documentação
myDfm <- dfm(myCorpus, stem = F)
myDfm

##Pode-se tamber criar o documento como na Package TM já fazendo o stem e stopwords'

stopwors2 <- c('the','r','é','c','?','!','of','rt','pra')
myDfm <- dfm(myCorpus, groups='whois', remove = c(quanteda::stopwords("portuguese"),stopwors2
                                                  ,tm::stopwords('portuguese')), 
             stem = F, remove_punct = TRUE)
#note que com a opão groups ele agrupou pela nossa classificação
myDfm

## para acessar os termos mais usados
topfeatures(myDfm, 20) 



#e finalmente chegamos na nossa já conhecida wordcloud
set.seed(100)
textplot_wordcloud(myDfm, min.freq = 15, random.order = FALSE,
                   rot.per = .6, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))




#Frequencia do texto

allfeats <- textstat_frequency(myDfm)
allfeats$feature <- with(allfeats, reorder(feature, -frequency))

ggplot(head(allfeats,20), aes(x=feature, y=frequency, fill=frequency)) + geom_bar(stat="identity") +
  xlab("Termos") + ylab("Frequência") + coord_flip() +
  theme(axis.text=element_text(size=7))


#Quais os termos mais frequentes?
col <- textstat_collocations(myCorpus , size = 2:4, min_count = 2)
head(col)

#col <- with(col, reorder(collocation, count))
ggplot(col[order(col$count, decreasing = T),][1:25,], 
       aes(x=reorder(collocation,count), y=factor(count), fill=factor(count))) + geom_bar(stat="identity") +
  xlab("Expressões") + ylab("Frequência")  + coord_flip() +
  theme(axis.text=element_text(size=7))


#Vendo as palavras relacionadas do agrupamentos que criamos
tstatkeyness <- textstat_keyness(myDfm, target = 'conta')
head(tstatkeyness,10)

#e agora plotando!
textplot_keyness(tstatkeyness)

#dfm_sort(myDfm)[, 1:20]
textstat_simil(myDfm, c('conta','total','fixo','valor','internet'))

#Plotando score wordfish(escala não supervisionada)
## wordfish
wfm <- textmodel_wordfish(myDfm)
textplot_scale1d(wfm)

#Plot estimated word positions
textplot_scale1d(wfm, margin = "features", 
                 highlighted = c('conta','total','fixo','valor','internet'))

#Plotando Xray
textplot_xray(kwic(myCorpus[1:40], "conta"), 
              kwic(myCorpus[1:40], "total"),
              kwic(myCorpus[1:40], "fixo"),
              kwic(myCorpus[1:40], "valor"),
              kwic(myCorpus[1:40], "internet"))

#Calculando a diversidade complexidade dos textos
textstat_lexdiv(myDfm, "all",drop=T) %>% arrange(desc(U))




#Exibindo colocations, fazendo um score de termos
#vamos pegar os tweets crus sem nenhum processo, apenas com encode
twraw <- readr::parse_character(resultUtfUniqueSw, locale = readr::locale('pt')) 
mytoken <- tokens(twraw, 
                  remove_numbers=T,remove_symbols=T, 
                  remove_twitter=T, remove_url=T)

head(mytoken)


mytoken <- tokens_remove(mytoken, stopwords('portuguese'))
head(textstat_collocations(mytoken,size = 5, min_count = 5))


#Criando um grafico das relações entre as tags
myrawCorpus <- corpus(twraw)
tweetdfm <- dfm(myrawCorpus, remove_punct = TRUE)
tagdfm <- dfm_select(tweetdfm, ('#*'))
toptag <- names(topfeatures(tagdfm, 10))
head(toptag)

tagfcm <- fcm(tagdfm)
head(tagfcm)

toptagfcm <- fcm_select(tagfcm, toptag)
textplot_network(toptagfcm, min_freq = 0.1, edge_alpha = 0.8, edge_size = 5)


#Criando um grafico das relações de usuários
userdfm <- dfm_select(tweetdfm, ('@*'))
topuser <- names(topfeatures(userdfm, 200))
userfcm <- fcm(userdfm)
userfcm <- fcm_select(userfcm, topuser)
textplot_network(userfcm, min_freq = 0.1, edge_color = 'blue', edge_alpha = 0.8, edge_size = 5)



#Criando um DTM com nossas polaridades como dicionario
positivas <- polaridades_pt %>% filter(sentimento == 'positivo') %>% select(word)
negativas <- polaridades_pt %>% filter(sentimento == 'negativo') %>% select(word)

dic <- dictionary(list(positivas=as.character(positivas$word), negativas=as.character(negativas$word)))
bySentimento <- dfm(myCorpus, dictionary = dic)

scorebygroup <- tidy(bySentimento %>% 
                       dfm_group(groups='whois') )
scorebygroup




scorebygroup %>%
  ggplot(aes(document, count)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ term) +
  scale_y_continuous(labels = percent_format()) +
  ylab("Frequência por polaridade") +
  aes(color = term) + scale_color_manual(values = c("red", "green"))

     
scorebygroup %>%
  ggplot(aes(term, count)) +
  geom_bar(stat = 'identity') +
  geom_smooth() +
  facet_wrap(~ document) +
  scale_y_continuous(labels = percent_format()) +
  ylab("Frequência por polaridade") +
  aes(fill= term,color = term) + scale_color_manual(values = c("red", "green"))

#Procurando por frequencia
bySentimento


CreatePercentile <- function(x) {
  x$FreqTotal <- cumsum(x$frequency)
  TotalType <- sum(x$frequency)
  WordCoverage = NULL
  for (i in 1:10) {
    WordCoverage <- rbind(WordCoverage, c(max(x$rank[x$FreqTotal <= i/10 * 
                                                       TotalType]), i/10))
  }
  Percentile <- c(WordCoverage[, 2])
  TextNum <- c(WordCoverage[, 1])
  WordCoverage <- data.frame(Percentile, TextNum)
  return(WordCoverage)
}
facebookCorpus <- corpus_subset(myCorpus, whois=='conta')
googleCorpus <- corpus_subset(myCorpus, whois=='internet')
microsoftCorpus <- corpus_subset(myCorpus, whois=='valor')
#teste <- dfm(myCorpus, dictionary = dic, groups = 'whois')
facedfm <- dfm(facebookCorpus, stem = F)
googledfm <- dfm(googleCorpus, stem = F)
micdfm <- dfm(microsoftCorpus, stem=F)
facefreq <- textstat_frequency(facedfm)
googlefreq <- textstat_frequency(googledfm)
micfreq <- textstat_frequency(micdfm)
facecover <-CreatePercentile(facefreq)
googlecover <-CreatePercentile(googlefreq)
miccover <-CreatePercentile(micfreq)


faceg <- ggplot(facecover, aes(Percentile * 100, TextNum)) + geom_point() + 
  geom_line() + xlab("Cover") + ylab("# of Text ") + ggtitle("conta")

googleg <- ggplot(googlecover, aes(Percentile * 100, TextNum)) + geom_point() + 
  geom_line() + xlab("Cover") + ylab("# of Text ") + ggtitle("valor")

micg <- ggplot(miccover, aes(Percentile * 100, TextNum)) + geom_point() + 
  geom_line() + xlab("Cover") + ylab("# of Text ") + ggtitle("internet")
grid.arrange(faceg, googleg, micg, ncol = 3)


#Frequência relativa por empresa

gf <- myCorpus %>%
  dfm(remove = stopwords("portuguese"), remove_punct = TRUE)# %>%
  #dfm_weight(type = "relfreq")


# Calculando a frequencia relativa pelas empresas de tecnologia
freq_weight <- textstat_frequency(gf, n = 10, groups = "whois")

ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_weight):1,
                     labels = freq_weight$feature) +
  labs(x = NULL, y = "Relative frequency")


#Plotando um dendograma com quantide
#data(data_corpus_SOTU, package = "quantedaData")
dendoDfm <- dfm(myCorpus, 
                stem = TRUE, groups = 'whois', remove_punct = TRUE,
                remove = stopwords("portuguese"))
trimDfm <- dfm_trim(dendoDfm, min_count = 5, min_docfreq = 3)

distMat <- textstat_dist(dfm_weight(trimDfm, "relfreq"))

myCluster <- hclust(distMat)

myCluster$labels <- docnames(trimDfm)
# plot as a dendrogram
plot(myCluster, xlab = "", sub = "", main = "Distancia euclidiana na frequencia de tokens normalizada")



#Procurando por similaridades

sim <- textstat_simil(myDfm, c("xbox"), method = "cosine", margin = "features")
lapply(as.list(sim), head, 20)

lista <- lapply(as.list(sim), head, 20)
dotchart(lista$xbox, xlab = "metodo cosine")

#Trabalhando com modelos Topicos
quantdfm <- dfm(myCorpus, 
                remove_punct = TRUE, remove_numbers = TRUE, tolower = T,  remove = stopwords("portuguese"))
quantdfm <- dfm_trim(quantdfm, min_count = 10, max_docfreq = 10, verbose = TRUE)


mylda <- LDA(convert(quantdfm, to = "topicmodels"), k = 20)
#str(mylda)
#mylda@documents

head(get_terms(mylda,6))

#Análise de Sentimentos com Tidy
#salvando o dataframe para não ter discrepancias nos comentários
#twdf %>% write_csv(path='parte2/twittersentimentaldata.csv')
twdf <- read_csv('d://Cursos/PreparacaoCarreiraCientista/R-Bigdata/Projeto1/parte2/twittersentimentaldata.csv')
twdf$id <- rownames(twdf)
tw <- twdf %>% mutate(document = id,word=tweet) %>% select(document,word,whois)
#note que a coluna document carrega a identificação de cada texto
str(tw)


tdm <- tw %>% unnest_tokens(word,word) 

#Removendo as stopwords
tdm <- tdm %>% anti_join(data.frame(word= stopwords('portuguese')))
tdm <- tdm %>% anti_join(data.frame(word= stopwors2))
head(tdm)

#tdm <- tdm  %>% group_by(document) %>% mutate(word_per_doc = n()) 
#tdm <- tdm  %>% group_by(whois) %>% mutate(word_per_whois = n()) 

library(tidyr)
sentJoin <- tdm %>%
  inner_join(polaridades_pt, by='word')

sentJoin %>%
  count(sentimento) %>%
  ggplot(aes(sentimento,n , fill = sentimento)) +
  geom_bar(stat = "identity", show.legend = FALSE)


sentJoin %>%
  count(whois, index = document, sentimento) %>%
  spread(sentimento, n, fill = 0) %>%
  mutate(score = positivo - negativo) %>%
  ggplot(aes(index, score, fill = whois)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~whois, ncol = 2, scales = "free_x")


scored <- sentJoin %>%
  count(whois,sentimento) %>%
  spread(sentimento, n, fill = 0) %>%
  mutate(score = positivo -negativo) %>%
  mutate(scoreperc = (positivo / (positivo + negativo)) * 100)
ggplot(scored, aes(whois,scoreperc , fill = whois)) +
  geom_bar(stat = "identity", show.legend = T) 

#Quais são nossos tops sentimentos?

word_counts <- sentJoin %>%
  count(word, sentimento, sort = TRUE) %>%
  ungroup()

word_counts %>%
  filter(n > 5) %>%
  mutate(n = ifelse(sentimento == "negativo", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentimento)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Contribution to sentiment")



#E finalmente nossa wordcloud de sentimentos

library(reshape2)
library(wordcloud)
sentJoin %>%
  count(word, sentimento, sort = TRUE) %>%
  acast(word ~ sentimento, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 60)




#Sentimentos mais negativos
bottom8tw <- head(sentJoin %>%
                    count(document, sentimento) %>%
                    spread(sentimento, n, fill = 0) %>%
                    mutate(score = positivo - negativo) %>%
                    arrange(score),8)['document']


twdf %>% filter(id %in% as.vector(bottom8tw$document))


#Sentimentos mais Positivos
top8 <- head(sentJoin %>%
               count(document, sentimento) %>%
               spread(sentimento, n, fill = 0) %>%
               mutate(score = positivo - negativo) %>%
               arrange(desc(score)),8)['document']
twdf %>% filter(id %in% as.vector(top8$document))


#Analise de Sentimentos usando Naive Bayes

#devtool::install_github('mananshah99/sentR')
require(sentR)

sentimentToScore <-  sample_n(data.frame(text=twdf$tweet),100)
# 2. Aplicando o metodo de classificação Naive Bayes
out <- classify.naivebayes(sentimentToScore$text)
scoredDf <- cbind(sentimentToScore,out, stringsAsFactors=F)
scoredDf$`POS/NEG` <- as.numeric(scoredDf$`POS/NEG`) 
s <-head(scoredDf %>% arrange(`POS/NEG`) %>% select(text),10) 
#mais negativas segundo o naive bayes
s[,1]


#Mais positivas segundo Naive Bayes
#MAIS POSITIVAS...
s<-head(scoredDf %>% arrange(desc(`POS/NEG`)) %>% select(text),10) 

s[,1]

#Bonus - O Pacote SentimentAnalysis
library(SentimentAnalysis)
dictionaryPortuguese <- SentimentDictionaryBinary(positivas$word, 
                                                  negativas$word)
twdf$id <- row.names(twdf)
sentiment <- analyzeSentiment(twdf$tweet,
                              language="portuguese",
                              rules=list("PtSentiment"=list(ruleSentiment, dictionaryPortuguese), 
                                         "Ratio"=list(ruleSentimentPolarity,dictionaryPortuguese),
                                         "Words"=list(ruleWordCount)))
#sentiment
plotSentiment(sentiment)


sentiment$id <- row.names(sentiment)
worstfeelings <- sentiment[order(sentiment$PtSentiment),][1:10,] 
bestfeelings <-  sentiment[order(-sentiment$PtSentiment),][1:10,] 

##Topdown 10 Ruins
twdf[twdf$id %in% worstfeelings$id,]

#top 10
twdf[twdf$id %in% bestfeelings$id,]



#Bonus: Machine Learning com RTextTools


## Usando diferentes algoritimos para  testar Classificando
library(RTextTools)
library(ptstem)

classificados <- NULL
classificados <- read_csv('d://Cursos/PreparacaoCarreiraCientista/R-Bigdata/Projeto1/parte2/twitterclassificado.csv')
#pegando somente os que eu classifiquei manualmente, afinal..
classificados <- classificados[1:70,]
classificados$class.text[classificados$class.text== -1] <- 0
classificados$whois <- NULL
#renomeando colunas para converter em corpus, assim evitando a perda de acentuação
classificados$doc_id <- row.names(classificados)
classificados <- rename(classificados, text = tweet)

classificados$text <- sapply(classificados$text, function(x) ptstem(x, algorithm = "hunspell", complete = T))

classificados$text <- sapply(classificados$text,enc2native)
corp <- VCorpus(DataframeSource(classificados), readerControl = list(language='pt'))
corp <- tm_map(corp, stripWhitespace)

doc_matrix <- DocumentTermMatrix(corp)
set.seed(1234)
container <- create_container(doc_matrix, classificados$class.text, trainSize=1:55,
                              testSize=56:70, virgin=FALSE)
#removendo nnet
allalgos <- print_algorithms()



algos <- allalgos[! allalgos == 'NNET']
models <- train_models(container, algorithms=algos)

results <- classify_models(container, models)

# mostrando resultados
analytics <- create_analytics(container, results)
summary(analytics)


getAccuracy <- function(container, nfold, algoritmos){
  c <- 0
  d <- data.frame(algoritmo=as.numeric(0), meanAccuracy= as.numeric(0), deviation=as.numeric(0))
  for(i in algoritmos){
    print(paste('processando: ', i))
    ma <-  cross_validate(container,nfold,algorithm =i)
    if(c == 0) {
      d$algoritmo <- i
      d$meanAccuracy <- ma$meanAccuracy
      d$deviation <- sd(unlist(ma[[1]]))
      c <- 1
    }else {
      d<-rbind(d, data.frame(algoritmo=i, meanAccuracy = ma$meanAccuracy, deviation=sd(unlist(ma[[1]]))))
    }
  }
  return(d)
}
d <- getAccuracy(container,10, algos)  

# que temos erros no glmnet..longer object length is not a multiple of shorter object length
d

