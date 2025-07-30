### BASIC SENTIMENT ANALYSIS (RULE-BASED) ###

# Step 1: Install and Load Packages
install.packages("stringr")   # Run this only once
library(stringr)

# Step 2: Build a Data Frame for Sentiment Words
sentiment_words <- data.frame(
  Word = c("good", "happy", "awesome", "great", "excellent",
           "bad", "sad", "terrible", "awful", "poor"),
  Sentiment = rep(c("Positive", "Negative"), each = 5)
)

# Step 3: Input a Sample Sentence (can be replaced)
sentence <- "I feel happy and awesome today!"

# Step 4: Count Positive and Negative Words
pos_count <- sum(str_count(sentence, sentiment_words$Word[sentiment_words$Sentiment == "Positive"]))
neg_count <- sum(str_count(sentence, sentiment_words$Word[sentiment_words$Sentiment == "Negative"]))

# Step 5: Determine Sentiment
if (pos_count > neg_count) {
  sentiment <- "Positive"
} else if (neg_count > pos_count) {
  sentiment <- "Negative"
} else {
  sentiment <- "Neutral"
}

# Step 6: Show the Result
result <- data.frame(
  Sentence = sentence,
  Pos_Words = pos_count,
  Neg_Words = neg_count,
  Sentiment = sentiment
)

print(result)


### BASIC WORD CLOUD ###

# Step 1: Install Packages (Only need to run once)
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")

# Step 2: Load Libraries
library(tm)
library(wordcloud)
library(RColorBrewer)

# Step 3: Load a Text File
# This will open a file picker — select a .txt file from your desktop
text_file <- readLines(file.choose(), encoding = "UTF-8")

# Step 4: Create a Text Corpus
corpus <- Corpus(VectorSource(text_file))

# Step 5: Clean the Text
corpus <- tm_map(corpus, content_transformer(tolower))           # convert to lowercase
corpus <- tm_map(corpus, removePunctuation)                      # remove punctuation
corpus <- tm_map(corpus, removeNumbers)                          # remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("english"))      # remove common stopwords

# Step 6: Create a Term-Document Matrix
tdm <- TermDocumentMatrix(corpus)
matrix <- as.matrix(tdm)
word_freq <- sort(rowSums(matrix), decreasing = TRUE)
df <- data.frame(word = names(word_freq), freq = word_freq)

# Step 7: Generate the Word Cloud
wordcloud(words = df$word,
          freq = df$freq,
          min.freq = 1,
          max.words = 100,
          scale = c(3, .5),
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))





