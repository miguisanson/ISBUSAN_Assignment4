# --- Olympics Reddit Sentiment Analysis ---
# This script reads the scraped Reddit data, performs sentiment analysis,
# and generates summary visualizations and word clouds.

# --- Part 0: Setup ---
# Install the necessary packages if you haven't already.
# install.packages("dplyr")
# install.packages("tidytext")
# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("wordcloud")
# install.packages("RColorBrewer")

# Load the required libraries
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

# --- Part 1: Load and Prepare Data ---
# Set your working directory to where your CSV files are located.
setwd("E:/Github School Projects/ISBUSAN_Database_Assets/Reddit_CSV")

# Load the two datasets you created with the scraper script
sports_data <- read.csv("reddit_olympics_5-sports_data.csv")
general_data <- read.csv("reddit_olympics_general-discussion_data.csv")

# Combine them into a single dataframe for analysis
full_data <- bind_rows(sports_data, general_data)

# Prepare the data for analysis
# 1. Add a unique ID to every row (post or comment)
# 2. Combine the post title and the text for better context
prepared_data <- full_data %>%
  mutate(doc_id = row_number()) %>%
  mutate(full_text = paste(title, text)) %>%
  select(doc_id, search_keyword, full_text, score)

cat("Data loaded and prepared successfully.\n")

# --- Part 2: Perform Sentiment Analysis ---
# We will use the 'bing' sentiment lexicon, which categorizes words
# as either "positive" or "negative".

# Get the Bing lexicon
bing_lexicon <- get_sentiments("bing")

# Perform the sentiment analysis
sentiment_scores <- prepared_data %>%
  # Break the text into individual words (tokens)
  unnest_tokens(word, full_text) %>%
  # Join with the sentiment lexicon to tag each word
  inner_join(bing_lexicon, by = "word") %>%
  # Count the number of positive and negative words for each document
  count(doc_id, sentiment) %>%
  # Create separate columns for "positive" and "negative" counts
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  # Calculate a final sentiment score (positive - negative)
  mutate(sentiment_score = positive - negative)

# Join the sentiment scores back to our main prepared data
final_data <- prepared_data %>%
  left_join(sentiment_scores, by = "doc_id") %>%
  # If a document had no sentiment words, its score will be NA. Change this to 0.
  mutate(
    positive = ifelse(is.na(positive), 0, positive),
    negative = ifelse(is.na(negative), 0, negative),
    sentiment_score = ifelse(is.na(sentiment_score), 0, sentiment_score)
  )

cat("Sentiment analysis complete.\n")

# --- Part 3: Summarize and Visualize Results ---

# Create a summary of sentiment distribution for each sport
sentiment_summary <- final_data %>%
  # Classify each document's overall sentiment
  mutate(overall_sentiment = case_when(
    sentiment_score > 0 ~ "Positive",
    sentiment_score < 0 ~ "Negative",
    TRUE ~ "Neutral"
  )) %>%
  # Count the number of Positive/Negative/Neutral posts for each keyword
  count(search_keyword, overall_sentiment) %>%
  # Calculate percentages for better comparison
  group_by(search_keyword) %>%
  mutate(percentage = n / sum(n) * 100)

# Print the summary table to the console
print(sentiment_summary)

# Create a bar chart to visualize the sentiment comparison
sentiment_plot <- ggplot(sentiment_summary, aes(x = search_keyword, y = percentage, fill = overall_sentiment)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Sentiment Distribution Across Olympic Topics on Reddit",
    x = "Search Keyword",
    y = "Percentage of Posts/Comments",
    fill = "Overall Sentiment"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot. It will also be saved as a file.
print(sentiment_plot)

# Save the plot as a PNG file
ggsave("sentiment_distribution_plot.png", plot = sentiment_plot, width = 10, height = 6)
cat("Sentiment distribution plot saved as sentiment_distribution_plot.png\n")

# Save the final, detailed data with sentiment scores to a new CSV.
# This file is ready to be used in Tableau.
write.csv(final_data, "reddit_data_with_sentiment.csv", row.names = FALSE)
cat("Final data with sentiment scores saved to reddit_data_with_sentiment.csv\n")


# --- Part 4: Generate Word Clouds ---
# This section will create separate positive and negative word clouds for each sport.

# Get the list of unique keywords to loop through
keywords <- unique(full_data$search_keyword)

# Tokenize the full dataset once for efficiency
tokenized_data <- prepared_data %>%
  unnest_tokens(word, full_text) %>%
  # Remove common "stopwords" like 'the', 'a', 'is', etc.
  anti_join(stop_words, by = "word")

for (sport in keywords) {
  cat(paste("Generating word clouds for:", sport, "\n"))
  
  # Filter for the current sport and join with the sentiment lexicon
  sport_words <- tokenized_data %>%
    filter(search_keyword == sport) %>%
    inner_join(bing_lexicon, by = "word")
  
  # Create Positive and Negative clouds
  for (senti in c("positive", "negative")) {
    
    # Prepare the word frequency data
    word_freq <- sport_words %>%
      filter(sentiment == senti) %>%
      count(word, sort = TRUE)
    
    # Check if there are any words for this sentiment
    if (nrow(word_freq) > 0) {
      
      # Set the filename for the plot
      png(paste0(sport, "_", senti, "_word_cloud.png"), width = 800, height = 800)
      
      # Generate the word cloud
      wordcloud(
        words = word_freq$word,
        freq = word_freq$n,
        min.freq = 2,
        max.words = 100,
        random.order = FALSE,
        rot.per = 0.35,
        colors = brewer.pal(8, "Dark2")
      )
      
      # Add a title
      title(main = paste(sport, "-", toupper(senti), "WORDS"))
      
      dev.off() # Close the PNG device
    }
  }
}

cat("Word cloud generation complete.\n")
