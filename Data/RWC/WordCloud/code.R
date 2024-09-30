
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

write.csv(df, "wiki-rwc.csv", row.names=FALSE)

## Word cloud

library(wordcloud)

svg("wordcloud.svg")
par(mar=rep(0, 4))
wordcloud(words = df$word, freq = df$freq, min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.35)
dev.off()

## Trim some whitespace

svg <- readLines("wordcloud.svg")
vbline <- grep("viewBox", svg)
svg[vbline] <- gsub("0 0 504 504", "52 52 400 400", svg[vbline])
## width/height
svg[vbline] <- gsub("504", "400", svg[vbline])
writeLines(svg, "wordcloud-trim.svg")

notrun <- function() {
    library(wordcloud2)
    wordcloud2(data=subset(df, freq > 1), size=1.6, color='random-dark')
}

