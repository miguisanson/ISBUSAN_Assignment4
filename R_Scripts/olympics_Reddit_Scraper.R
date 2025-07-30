# --- Reddit Scraper for Olympics Project Analysis ---

# Install required packages if you haven't already
# install.packages("RedditExtractoR")
# install.packages("dplyr")

# Load the necessary libraries
library(RedditExtractoR)
library(dplyr)

# --- Configuration ---

# 1. Set the output directory for the final CSV files.
# IMPORTANT: Make sure this folder exists on your computer before running the script.
setwd("E:/Github School Projects/ISBUSAN_Database_Assets/Reddit_CSV")

# 2. Define the keywords for the two separate exports.
sports_keywords <- c("Basketball", "Swimming", "Gymnastics", "Weightlifting", "Tennis")
general_keyword <- "Olympics"
n_posts <- 100 # The number of posts to scrape for EACH keyword.
subreddit_name <- "olympics"

# --- Helper Functions ---

# This function removes special characters that can cause errors.
clean_text <- function(text) {
  # Replaces non-ASCII characters with an empty string
  iconv(text, "latin1", "ASCII", sub = "")
}

# This function extracts the main post and all its comments from a single URL.
# It's defined once here and used in both scraping sections below.
extract_thread_data <- function(thread_url, search_keyword) {
  tryCatch({
    content <- get_thread_content(thread_url)
    post_df <- content$threads
    comments_df <- content$comments
    
    if (is.null(post_df) || nrow(post_df) == 0) return(NULL)
    
    # Format the original post data, now with text cleaning
    original_post <- post_df %>%
      transmute(
        search_keyword = search_keyword,
        post_url = url,
        title = clean_text(title),
        author = clean_text(author),
        date = date,
        text = clean_text(text),
        score = score,
        type = "post"
      )
    
    # Check for and format comments, now with text cleaning
    if (!is.null(comments_df) && nrow(comments_df) > 0) {
      comments <- comments_df %>%
        transmute(
          search_keyword = search_keyword,
          post_url = thread_url,
          title = clean_text(post_df$title[1]),
          author = clean_text(author),
          date = date,
          text = clean_text(comment),
          score = score,
          type = "comment"
        )
      return(bind_rows(original_post, comments))
    } else {
      return(original_post)
    }
    
  }, error = function(e) {
    message(paste("Skipped thread due to error:", thread_url, "-", e$message))
    return(NULL)
  })
}

# --- Part 1: Scrape the 5 Specific Sports ---

cat("--- Starting Part 1: Scraping 5 Specific Sports ---\n")
all_sports_data <- list()

for (keyword in sports_keywords) {
  cat(paste0("\n--- Searching for threads with keyword: '", keyword, "' ---\n"))
  
  threads <- NULL
  retries <- 0
  max_retries <- 5
  retry_delay <- 15 # Start with a 15-second delay
  
  # NEW: This loop will keep retrying if it gets rate-limited, instead of skipping.
  while(is.null(threads) && retries < max_retries) {
    threads <- tryCatch({
      find_thread_urls(keywords = keyword, subreddit = subreddit_name, sort_by = "relevance", period = "all")
    }, error = function(e) {
      retries <<- retries + 1
      message(paste("Could not fetch threads for '", keyword, "' due to an error: ", e$message))
      message(paste("This is likely due to rate limiting. Retrying in", retry_delay, "seconds... (Attempt", retries, "of", max_retries, ")"))
      Sys.sleep(retry_delay)
      retry_delay <<- retry_delay * 2 # Double the delay for the next attempt
      return(NULL)
    })
  }
  
  # Check if the search was successful after retries
  if (is.null(threads) || !is.data.frame(threads) || nrow(threads) == 0) {
    cat(paste0("Failed to fetch threads for '", keyword, "' after ", max_retries, " attempts. Moving to next keyword.\n"))
    next
  }
  
  threads <- head(threads, n_posts)
  
  cat(paste0("Found ", nrow(threads), " threads for '", keyword, "'. Starting extraction...\n"))
  for (i in seq_along(threads$url)) {
    url <- threads$url[i]
    cat(paste0("Processing post ", i, " of ", nrow(threads), ": ", url, "\n"))
    thread_data <- extract_thread_data(url, keyword)
    if (!is.null(thread_data)) {
      all_sports_data[[length(all_sports_data) + 1]] <- thread_data
    }
    # Increased and randomized the delay to be more 'polite' to Reddit's servers
    Sys.sleep(sample(3:5, 1))
  }
}

# Combine and save the data for the 5 sports
if (length(all_sports_data) > 0) {
  final_sports_df <- bind_rows(all_sports_data)
  sports_output_file <- paste0("reddit_", subreddit_name, "_", "5-sports_data.csv")
  write.csv(final_sports_df, sports_output_file, row.names = FALSE, fileEncoding = "UTF-8")
  cat("\nPart 1 Complete! Sports data saved to:", file.path(getwd(), sports_output_file), "\n")
} else {
  cat("\nPart 1 Complete, but no data was successfully scraped for the specific sports.\n")
}

# --- Part 2: Scrape General Olympics Discussion ---

cat("\n--- Starting Part 2: Scraping General 'Olympics' Discussion ---\n")
all_general_data <- list()

threads <- NULL
retries <- 0
retry_delay <- 15

while(is.null(threads) && retries < max_retries) {
  threads <- tryCatch({
    find_thread_urls(keywords = general_keyword, subreddit = subreddit_name, sort_by = "relevance", period = "all")
  }, error = function(e) {
    retries <<- retries + 1
    message(paste("Could not fetch threads for '", general_keyword, "' due to an error: ", e$message))
    message(paste("This is likely due to rate limiting. Retrying in", retry_delay, "seconds... (Attempt", retries, "of", max_retries, ")"))
    Sys.sleep(retry_delay)
    retry_delay <<- retry_delay * 2
    return(NULL)
  })
}

if (is.null(threads) || !is.data.frame(threads) || nrow(threads) == 0) {
  cat(paste0("Failed to fetch threads for '", general_keyword, "' after ", max_retries, " attempts.\n"))
} else {
  threads <- head(threads, n_posts)
  cat(paste0("Found ", nrow(threads), " threads for '", general_keyword, "'. Starting extraction...\n"))
  for (i in seq_along(threads$url)) {
    url <- threads$url[i]
    cat(paste0("Processing post ", i, " of ", nrow(threads), ": ", url, "\n"))
    thread_data <- extract_thread_data(url, general_keyword)
    if (!is.null(thread_data)) {
      all_general_data[[length(all_general_data) + 1]] <- thread_data
    }
    Sys.sleep(sample(3:5, 1))
  }
}

# Combine and save the data for the general discussion
if (length(all_general_data) > 0) {
  final_general_df <- bind_rows(all_general_data)
  general_output_file <- paste0("reddit_", subreddit_name, "_", "general-discussion_data.csv")
  write.csv(final_general_df, general_output_file, row.names = FALSE, fileEncoding = "UTF-8")
  cat("\nPart 2 Complete! General discussion data saved to:", file.path(getwd(), general_output_file), "\n")
} else {
  cat("\nPart 2 Complete, but no data was successfully scraped for the general keyword.\n")
}

cat("\n--- All scraping finished! ---\n")
