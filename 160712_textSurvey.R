## Create a corpus
library(tm)


x <- paste(a0$q, sep = ',', collapse = '; ')

dat <- Corpus(VectorSource(x), readerControl = list (language= 'spanish'))
dat
options(mc.cores=1)
dat <-  tm_map (dat, removeWords, stopwords("spanish"))
dat <- tm_map(dat, stripWhitespace)
dat <- tm_map(dat, removePunctuation)
dat <- tm_map(dat, PlainTextDocument)

# dtm <- DocumentTermMatrix(dat)
# inspect(dtm)
# termFreq(x)
# class(x)
 

wordcloud(dat, scale = c(3, 0.5),min.freq = 0.1, max.words = 25,
          random.order = T, random.color = T)



a0 <- dplyr::select(surv, id = 235, q = 68, place = locationName, treatment = treatmentName)
if (class(a0$q) == 'factor') a0$q <- as.character(a0$q)
x <- paste(a0$q, sep = ' ', collapse = ' ')
dat <- Corpus(VectorSource(x), readerControl = list (language= 'spanish')) # options(mc.cores=1)
dat <-  tm_map (dat, removeWords, stopwords("spanish"))
dat <- tm_map(dat, stripWhitespace)
dat <- tm_map(dat, removePunctuation)
dat <- tm_map(dat, PlainTextDocument)
wordcloud(dat, scale = c(2, 0.5),min.freq = 1,
          random.order = FALSE, random.color = FALSE, color = 'red', family = 'Helvetica' )












