##Install neccesary packages
install.packages('tm')
library(tm)

##CONSTANTS

#Croatian stopwords
croStop <- c("i", "ili", "sex", "kita", "kurac", "penis", "bartol", "je", "da","u","ono","sama","ja",
             "ti","on","ona","ono","mi","vi","me","se","smo","to","za","moje","ali","svo","do","od",
             "nam","uz","bi", "što", "kako", "da", "ne", "asocira", "koja", "koji", "koje", "svoju",
             "svom","volim", "volite","sve","tim","tom","toga","svoju","svojem","svoj","svake","sve",
             "stalnim","sva","vec","veće","vidi","zadimanje","zbog","znači", "sam","samo", "rgnfa",
             "radi","radimo","steknu","suojesnog","sva","svojem","svoju","trump","tom","toga","tim",
             "tako","tvrci","usput","vec","vidi","volim","volite", "piso","nosit","odabor","nase","
             našem","naših","nekoj","nekom","nekome","može","moja","kroz","koliko","jest","kao",
             "jako","dovodi","druge","cyxc","doći","čime","and","baviti","aint","zbog","tijekom",
             "mene","kojeg","biti","htjeti","koji", "ako")

#Put all answers into one vector

careerVec <- paste(data$association.career, collapse = " ")

review_source <- VectorSource(careerVec)

corpus <- Corpus(review_source)

#Set all lower case
corpus <- tm_map(corpus, content_transformer(tolower))

corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

#Remove croatian word with no specific meaning
corpus <- tm_map(corpus, removeWords, croStop)

dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)

frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)


