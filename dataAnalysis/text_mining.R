##Install neccesary packages
#install.packages('tm')
library(tm)


#' @name Get Mined Text
#' @author Bartol Freškura
#' @description Analysis of the survey free answer question.
#' @param data Dataframe containing answers in form of free text
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
                   "karijeru","karijeri","asocira","što")
      
      #Put all answers into one vector
      
      careerVec <- paste(data$association.career, collapse = " ")
      
      review_source <- VectorSource(careerVec)
      
      corpus <- Corpus(review_source)
      
      #Set all lower case
      corpus <- tm_map(corpus, content_transformer(tolower))
      
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, removeNumbers)
      #corpus <- tm_map(corpus, stemDocument)###Plotting INA Association
      
      #Remove croatian word with no specific meaning
      corpus <- tm_map(corpus, removeWords, croStop)
      
      dtm <- DocumentTermMatrix(corpus)
      dtm2 <- as.matrix(dtm)
      
      frequency <- colSums(dtm2)
      frequency <- sort(frequency, decreasing=TRUE)
      frequency
      
}


