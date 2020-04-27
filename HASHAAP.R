
api_key <- "MDTvZYKS*****"
aapi_secret <- "FuAbYGF3***0wpkKed" 
token <- "1379657388-eXa76iB*******jNSgEOnIB" 
token_secret <- "5hwmI**PEUmq0"  
library(twitteR)
library(RCurl)
library(ROAuth)
library(ggplot2)
library(SnowballC)
library(httr)
library(dplyr)
library('tm')
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)


setup_twitter_oauth(api_key, api_secret, token, token_secret)


A_tweets2 <- searchTwitter("#AAP", since="2020-01-05", until = "2020-03-17",n=5000, lang='en')

A_tweets2.df= twListToDF(A_tweets2)
head(A_tweets2.df)
A_tweets2.df$text = gsub("&amp","",A_tweets2.df$text)
A_tweets2.df$text = gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",A_tweets2.df$text)
A_tweets2.df$text = gsub("@\\w+","",A_tweets2.df$text)
A_tweets2.df$text = gsub("[[:punct:]]","",A_tweets2.df$text)
A_tweets2.df$text = gsub("[[:digit:]]","",A_tweets2.df$text)
A_tweets2.df$text = gsub("http\\w+","",A_tweets2.df$text)
A_tweets2.df$text = gsub("[\t]{2,}","",A_tweets2.df$text)
A_tweets2.df$text = gsub("^\\s+|\\s+$","",A_tweets2.df$text)

A_tweets2.df$text<-iconv(A_tweets2.df$text,"UTF-8","ASCII",sub="")

emotions<-get_nrc_sentiment(A_tweets2.df$text)
emo_bar=colSums(emotions)
emo_sum=data.frame(count=emo_bar,emotion=names(emo_bar))
emo_sum$emotion=factor(emo_sum$emotion,levels = emo_sum$emotion[order(emo_sum$count,decreasing =TRUE )])

library(plotly)

z<-plot_ly(emo_sum,x=~emotion,y=~count,type = "bar",color =~emotion)%>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion for #AAP")

z


wordcloud_tweet = c(
  paste(A_tweets2.df$text[emotions$anger > 0], collapse=" "),
  paste(A_tweets2.df$text[emotions$anticipation > 0], collapse=" "),
  paste(A_tweets2.df$text[emotions$disgust > 0], collapse=" "),
  paste(A_tweets2.df$text[emotions$fear > 0], collapse=" "),
  paste(A_tweets2.df$text[emotions$joy > 0], collapse=" "),
  paste(A_tweets2.df$text[emotions$sadness > 0], collapse=" "),
  paste(A_tweets2.df$text[emotions$surprise > 0], collapse=" "),
  paste(A_tweets2.df$text[emotions$trust > 0], collapse=" ")
)





