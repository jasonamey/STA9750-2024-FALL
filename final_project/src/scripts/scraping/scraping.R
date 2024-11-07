install.packages("rvest")
install.packages("tidytext")

library(rvest)
library(tidytext)
library(dplyr)
library(stringr)


WD <- getwd()
DATA_DIRECTORY <- paste(WD, "final_project/data/ny_post/bike_lane", sep="/")


file_path <- paste(DATA_DIRECTORY, "bike_lane_1.html", sep="/")

page <- read_html(file_path)


text_content <- page |> 
  html_nodes(".co_searchResult_list") |> 
  html_text(trim = TRUE)


stop_words <- c("is", "a", "to","it", "and", "at", "in", "bike", "lane", "city", "traffic", "on", "postscript", "the", "from", "this", "that","has", "of", "are", "for", "into", "its")
pattern <- paste0("\\b(", paste(stop_words, collapse = "|"), ")\\b")

cleaned_text <- gsub(pattern, "", text_content, ignore.case = TRUE)
cleaned_text <- gsub("New York Post.*?Word Count: \\d+", "", cleaned_text)
cleaned_text <- gsub("[[:punct:]]", "", cleaned_text)
cleaned_text <- gsub("\\s+", " ", cleaned_text) 

text_df <- tibble(line = 1, text = cleaned_text)

word_counts <- text_df |>
  unnest_tokens(word, text) |>
  count(word, sort = TRUE)


bigram_counts <- text_df |>
  unnest_tokens(bigram, text, token = "ngrams", n = 3) %>%
  count(bigram, sort = TRUE)

print(bigram_counts, n = 100)