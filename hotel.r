#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

getwd()
setwd('C:/Users/758724/Desktop')

hotelfull<-read.csv('hotelfull.csv')

hotelneg<-hotelfull$Negative_Review
hotelpos<-hotelfull$Positive_Review

preproc<- function(dat){
  docs <- Corpus(VectorSource(dat))
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, removeWords, c("negative","book","bit","room","get","didn","stay","hotel","negat","one","night","noth")) 
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, stemDocument)
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=20, random.order=FALSE, rot.per=0.2, 
            colors=brewer.pal(8, "Dark2"))
  #print(head(d, 50))
}

low=0
high=40000
x=0
#go through list make new for each
"""

for (x in 1:13){
  preproc(hotelpos[low:high])
  low=high
  if((high+40000)>499475){
    high=499475
  }
  else{
    high=high+40000
  }
}

for (x in 1:13){
  preproc(hotelneg[low:high])
  low=high
  if((high+40000)>499475){
    high=499475
  }
  else{
    high=high+40000
  }
}
"""

list1<-c('breakfast','small','staff','bed','room','bathroom','time','work','shower','book','servic','noth','littl','like','check','good','day','bar','need','price','even','ask','poor','expens')

for (z in list1){
  hotelfull[z]=grepl(z, hotelneg)
}

list2<-c('locat','staff','good','great','help','friend','nice','clean','bed','comfort','excel','walk','love','room','posit','station','close','realli','perfect','everyth','restaur','well','view','bar','amaz','quiet','comf','area','facil','modern','food','beauti','recept','spacious','place','metro')

for (z in list2){
  hotelfull[z]=grepl(z, hotelpos)
}

hotelfull$Negative_Review=NULL
hotelfull$Positive_Review=NULL



summary(hotelfull$Room)

list3<-c('Single','Double','Triple','Quad','Queen','King','Twin','Studio','Suite')
for (z in list3){
  hotelfull[z]=grepl(z, hotelfull$Room)
}
hotelfull$Average_Score=NULL
hotelfull$Hotel_Name=NULL
hotelfull$Reviewer_Nationality=NULL
hotelfull$Review_Total_Negative_Word_Counts=NULL
hotelfull$Total_Number_of_Reviews=NULL
hotelfull$Review_Total_Positive_Word_Counts=NULL
hotelfull$Total_Number_of_Reviews_Reviewer_Has_Given=NULL
hotelfull$days_since_review=NULL
hotelfull$Room=NULL

#write.csv(hotelfull, file = "Hotel Data3.csv")

newhotel<-read.csv('Hotel Data3.csv')

summary(newhotel)

#count num of true in each
#repeat in string
fac<-newhotel[,7:70]

count=1
for (y in (fac)){
  z=names(fac)[count]
  z=paste(' ',z)
  h=length(which(y))
  newhotel1=paste(strrep(z,h),newhotel1)
  count=count+1
}




b=data.frame(newhotel1)

docs <- Corpus(VectorSource(b$newhotel1))
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
wordcloud(words = d$word, freq = d$freq,
          min.freq = 1,scale=c(3,.5),
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))




