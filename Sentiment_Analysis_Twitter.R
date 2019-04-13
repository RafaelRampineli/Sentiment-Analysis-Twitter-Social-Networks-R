# Mini Projeto - Análise de Sentimento de Redes Sociais {Twitter} utilizando o package: Sentiment

#Este projeto é parte integrante do curso Big Data Analytics com R e Microsoft Azure da Formação Cientista de Dados. 
#O objetivo é captutar dados da rede social Twitter e realizar análise de sentimentos com os dados capturados. 
#Para que este projeto possa ser executado, diversos pacotes devem ser instalados e carregados.
setwd("C:/Users/rafae/OneDrive/FilestoStudy/Formacao_Cientista_Dados/BigDataRAzure/Cap17_Mini_Projeto_AnaliseSentimento_RedesSociais/Solucao_Mini-Projeto")

options(warn=-1)

############### CARREGANDO PACOTES NECESSÁRIOS PARA A ANÁLISE ############### 

# Carregando os Pacotes do Twitter {Caso não tenha, instalar os pacotes antes}
library(twitteR)
library(httr)

# Carregando pacotes necessários para text mining.
library(SnowballC)
library(tm)

# Carregando pacotes necéssários para realizar a nuvem de palavras
library(RColorBrewer)
library(wordcloud)

#install.packages("Rstem_0.4-1.tar.gz", sep = "", repos = NULL, type = "source")
#install.packages("sentiment_0.2.tar.gz",sep = "", repos = NULL, type = "source")
library(Rstem) # Package útil para realizar análises em textos, documentos, emails.. 
library(sentiment) # Package Rstem é pré-requisito para utilizar o pacote sentiment
library(ggplot2)

############### ETAPA 1: AUTENTICAÇÃO NO TWITTER ############### 

# Chaves de autenticação no Twitter
# Essas chaves podem ser obtidas em https://developer.twitter.com.
# ** É necessário possuir uma conta Developer do Twitter.
consumer_key <- "vHG5kfrtK6VQ8naJhqjaKn2x7"
consumer_secret <- "DVuZ1W3Ye6qzGidx1uonFWNWchowrhlv5I21FNoy2dKL5ckKlo"
acess_token <- "475184897-O53I0UByt6PQ8qcxTg4QnvVI8eBXvyqRWqqtaBBY"
acess_tokensecret <- "mlK5CCjsdWEyGTtzppdltCJmdnfOZ06rkSmPaMifEYaGP"

# Autenticação. Responda 1 quando perguntado sobre utilizar direct connection.
setup_twitter_oauth(consumer_key, consumer_secret, acess_token, acess_tokensecret)

############### ETAPA 2: ACESSANDO O TWITTER PARA COLETAR DADOS ############### 

# Coletando dados do Twitter
Dados_Tweete <- searchTwitter(searchString = "BigData", n = 100, lang = 'en')

# Visualizando os dados coletados
head(Dados_Tweete)

############### ETAPA 3: REALIZANDO O TRATAMENTO E LIMPEZA DOS DADOS ############### 

# Tratamento dos dados coletados através de text mining.

# Criando uma função para limpeza geral dos dados obtidos.
Clean_Tweets <- function(tweet){
  # Remove http links
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  # Remove retweets
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  # Remove “#Hashtag”
  tweet = gsub("#\\w+", " ", tweet)
  # Remove nomes de usuarios “@people”
  tweet = gsub("@\\w+", " ", tweet)
  # Remove pontuacão
  tweet = gsub("[[:punct:]]", " ", tweet)
  # Remove os números
  tweet = gsub("[[:digit:]]", " ", tweet)
  # Remove os caracteres especiais
  tweet = gsub('[[:cntrl:]]', " ", tweet)
  # Remove espacos desnecessários
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  # Convertendo convertendo para letra minúscula
  tweet <- sapply(tweet, function(x){
                          y = NA
                          try_error = tryCatch(tolower(x), error=function(e) e)
                          if (!inherits(try_error, "error"))
                            y = tolower(x)
                          return(y)}
                  )
  tweet <- stringi::stri_trans_general(tweet, "latin-ascii")
  tweet <- iconv(tweet, from = "UTF-8", to = "ASCII")
}

# Obtendo somente o texto do tweete realizado
tweetlist <- sapply(Dados_Tweete, function(x) x$getText())

# Chamando a função Clean_Tweets criada para realizar a limpeza dos dados.
tweetlist <- Clean_Tweets(tweetlist)
View(tweetlist)




# Criando uma wordCloud para análise de associação das palavras.
tweetcorpus <- Corpus(VectorSource(tweetlist)) # Transforma os dados textuais para serem tratados como documentos que podem ter estruturas diferentes
tweetcorpus <- tm_map(tweetcorpus, removePunctuation)
tweetcorpus <- tm_map(tweetcorpus, function(x) {removeWords(x, stopwords("english"))}) # Removendo palavras irrelevantes para montar o wordCloud "clean"

View(tweetcorpus)

############### ETAPA 4: CRIANDO UMA WORDCLOUD ############### 

pal2 <- brewer.pal(8,"Dark2") # Gerando uma paleta de 8 cores

wordcloud(tweetcorpus, 
          min.freq = 2, 
          scale = c(5,1), 
          random.color = F, 
          max.word = 60, 
          random.order = F,
          colors = pal2)

############### ETAPA 5: REALIZANDO A ANÁLISE DE SENTIMENTO DOS DADOS COLETADOS ############### 

# Remover os registros que possuem valores missing.
tweetlist = tweetlist[!is.na(tweetlist)]
names(tweetlist) = NULL

# Classificando as emoções encontradas no Twitter utilizando algoritmo Bayes.
class_emotion = classify_emotion(tweetlist, algorithm = "bayes", prior = 1.0)
emotion = class_emotion[,7] #Escolhendo somente a coluna com o resultado "best_fit" da classificação, ou seja a que teve maior relevãncia

# Substituindo classificações que não foram identificadas relevância por "Neutral"
View(class_emotion)
emotion[is.na(emotion)] = "Neutral"

# Classificando a polaridade encontrada no Twitter (Positivo ou negativo).
class_polarity = classify_polarity(tweetlist, algorithm = "bayes")
View(class_polarity)
polarity = class_polarity[,4]


############### ETAPA 6: PREPARANDO E PLOTANDO GRÁFICOS COM O RESULTADO OBTIDO ############### 

# Gerando um dataframe com o resultado encontrados (Emotion + polarity) unificado para ser possivel realizar plots.
df_feeling = data.frame(text = tweetlist, emotion = emotion,
                     polarity = polarity, stringsAsFactors = FALSE)
View(df_feeling)

# Plotando um gráfico com as emoções identificadas no Twitter
ggplot(df_feeling, aes(x = emotion)) +
  geom_bar(aes(y = ..count.., fill = emotion)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Categorias", y = "Numero de Tweets") 

# Plotando um gráfico com a polaridade dos twitters identificados (positivo/negativo)
ggplot(df_feeling, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x = "Categorias de Sentimento", y = "Numero de Tweets")




