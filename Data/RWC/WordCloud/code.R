
## Export wikipedia page as PDF
## https://en.wikipedia.org/wiki/Rugby_World_Cup

## Convert PDF to text
## pdftotext
## and manually remove all the references

## Cleaning
## https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a

library(tm)

docs <- Corpus(VectorSource(readLines("Rugby_World_Cup.txt")))

docs <- docs |>
  tm_map(removeNumbers) |>
  tm_map(removePunctuation) |>
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

## term matrix
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words), freq=words)

## Filter
remove <- c("-", "–", "–", "—", "retrieved", "tbd", "archived")
df <- df[!(df$word %in% remove), ]

## Word cloud

library(wordcloud)

wordcloud(words = df$word, freq = df$freq, min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

notrun <- function() {
    library(wordcloud)
    wordcloud2(data=subset(df, freq > 1), size=1.6, color='random-dark')
}

