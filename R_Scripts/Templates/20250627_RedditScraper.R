# Install required packages
install.packages("RedditExtractoR")
install.packages("dplyr")
install.packages("writexl")

# Load libraries
library(RedditExtractoR)
library(dplyr)
library(writexl)

# Set output location
setwd("C:/Users/Raphael/Desktop") #Change the location in your own computer

# Parameters
keyword <- "cats"
n_posts <- 10

# Search for threads
threads <- find_thread_urls(
  keywords = keyword,
  subreddit = "dlsu",
  sort_by = "relevance",
  period = "all"
)

threads <- head(threads, n_posts)

if (nrow(threads) == 0) {
  stop("No threads found.")
}

# Function to extract post + comments
extract_thread_data <- function(thread_url) {
  tryCatch({
    reddit_data <- as.data.frame(get_thread_content(thread_url))
    if (nrow(reddit_data) == 0) return(NULL)
    
    original_post <- reddit_data[1, ] %>%
      transmute(
        post_url = thread_url,
        title = threads.title,
        author = threads.author,
        date = threads.date,
        text = threads.text,
        score = threads.score,
        type = "post"
      )
    
    if (any(!is.na(reddit_data$comments.comment))) {
      comments <- reddit_data %>%
        filter(!is.na(comments.comment)) %>%
        transmute(
          post_url = thread_url,
          title = threads.title,
          author = comments.author,
          date = comments.date,
          text = comments.comment,
          score = comments.score,
          type = "comment"
        )
      bind_rows(original_post, comments)
    } else {
      original_post
    }
  }, error = function(e) {
    message(paste("Skipped thread due to error:", thread_url))
    return(NULL)
  })
}

# Scrape data with delay
all_data <- list()

for (i in seq_along(threads$url)) {
  url <- threads$url[i]
  cat(paste0("Processing post ", i, ": ", url, "\n"))
  thread_data <- extract_thread_data(url)
  if (!is.null(thread_data)) {
    all_data[[length(all_data) + 1]] <- thread_data
  }
  Sys.sleep(2)
}

# Combine and save
final_df <- bind_rows(all_data)
output_file <- paste0("reddit_dlsu_", keyword, "_top", n_posts, "_posts.xlsx")
write_xlsx(final_df, output_file)
cat("Done! File saved to:", output_file, "\n")
