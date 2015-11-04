#library(devtools)
#install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")
library(Rfacebook)

#Facebook api init
fb_oauth <- fbOAuth(app_id="1533015106959098", app_secret="3b98125b767747095db2ec8180af6e75",extended_permissions = FALSE)
save(fb_oauth, file="fb_oauth")
load("fb_oauth")

page <- getPage("INA.Hrvatska", token=fb_oauth, n =200)

#Initialize comments vector
comments <- character()
for(i in 1:nrow(page)){
      post <- getPost(page[i,]$id, fb_oauth, n = 100, comments = TRUE)
      messagesLength <- (post[1]$post$comments_count)
      #Extract all messages
      if(messagesLength>0){
            for(j in 1:messagesLength){
                  comments <- c(comments, post$comments$message[j])
            }
      }
      
}


#Remove NA from vector and special characters
commentsNoNA <- comments[!is.na(comments)]
commentsNoNA <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", commentsNoNA)


#install.packages('wordcloud')
library(wordcloud)

frequency <- getMinedText(commentsNoNA)

#Draw a graph
words <- names(frequency)
wordcloud(words[1:20], scale=c(5,0.5), frequency[1:20], random.color = TRUE,colors=brewer.pal(8, "Dark2"))



##FUNCTIONS##

library(tm)
#' @name Get Mined Text
#' @author Bartol Freškura
#' @description Analysis of the survey free answer question.
#' @param data Vector containing answers in form of free text
#' @return Vector containing words sorted by frequency
getMinedText <- function(data){
      
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
                   "mene","kojeg","biti","htjeti","koji", "ako","tko","što","čiji","ciji","koja","kakav",
                   "kakve", "svakakve","svakakav","sebe","svoj","tu","one","netko","svatko", "kamo","
                   ondje","kada","zašto","donekle","mnogo","ih","gdje","kuda","zato","stoga","s","
                   sa","pa","pak","te","ni","niti","dok","god","nego","no","već","saomo","tek",
                   "čim","cim","pošto","posto","jer","budući","neka","kao","iako","premda","makar", "donald",
                   "fame","car","ćemu","bolj","gradiš","gradimo","imaš","imati","isto","jedn","jednim","
                   izrazita","gubitk","money","našem","onoga","oko","one","onog","opi","power","progr","
                   svoje", "bitch","fuck","shit","no","yes","ako","kojem","riječ","onom","itd","npr","npr.",
                   "ikakv","drugim","jednog","jednoj","nakon","mislim","svoje","ucini","karijera","karijere",
                   "karijeru","karijeri","asocira","što", "nešto", "nesto" ,"puno", "niz", "dio","onome",
                   "vas","ivan","godine","godina","dumovec","kad","buddy","ste","nas","vam","bivši",
                   "moj","bio","goran","malo","svima", "nema","zadužen","danas", "uvijek", "treba")
      
      #Put all answers into one vector
      
      careerVec <- paste(data, collapse = " ")
      
      review_source <- VectorSource(careerVec)
      
      corpus <- Corpus(review_source)
      
      #Set all lower case
      corpus <- tm_map(corpus, content_transformer(tolower))
      
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, removeNumbers)
      
      #Remove croatian word with no specific meaning
      corpus <- tm_map(corpus, removeWords, croStop)
      
      dtm <- DocumentTermMatrix(corpus)
      dtm2 <- as.matrix(dtm)
      
      frequency <- colSums(dtm2)
      frequency <- sort(frequency, decreasing=TRUE)
      frequency
      
}