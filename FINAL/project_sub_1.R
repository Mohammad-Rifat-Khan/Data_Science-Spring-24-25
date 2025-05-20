library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(tm)
library(textstem)
library(textclean)

url <- "https://www.thedailystar.net/news/investigative-stories?page="

page_urls <- paste0(url, 0:9)

get_article_links <- function(page_url) {
  read_html(page_url) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    unique() %>%
    grep("^/news/bangladesh/news/", ., value = TRUE)
}

extract_data <- function(link) {
  article_url <- paste0("https://www.thedailystar.net", link)
  article_page <- read_html(article_url)
  
  date <- article_page %>%
    html_node(".date.text-14.lh-20.color-iron") %>%
    html_text(trim = TRUE)
  
  author <- article_page %>%
    html_node(".byline.fw-600.text-16.e-mb-4 a") %>%
    html_text(trim = TRUE)
  
  title <- article_page %>%
    html_node("h1.article-title") %>%
    html_text(trim = TRUE)
  
  content <- article_page %>%
    html_nodes(".rtejustify") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = " ")
  
  data.frame(Title = title, Date = date, Author = author, Content = content, stringsAsFactors = FALSE)
}

all_links <- page_urls %>%
  map(get_article_links) %>%
  unlist()

data_list <- all_links %>%
  map_dfr(extract_data)

write.csv(data_list, "E:\\Academics\\DATA SCIENCE\\FINAL\\Project\\final.csv", row.names = FALSE)

data_list$Date <- as.Date(str_extract(data_list$Date, "\\b[A-Za-z]+ \\d{1,2}, \\d{4}\\b"), format = "%b %d, %Y")

clean_text <- function(text) {
  text <- replace_contraction(text)
  text <- tolower(text)
  text <- gsub("[^a-zA-Z\\s]", " ", text)
  text <- str_squish(text)
  tokens <- unlist(strsplit(text, "\\s+"))
  tokens <- tokens[!tokens %in% stopwords("en")]
  tokens <- stem_words(tokens)
  tokens <- lemmatize_words(tokens)
  paste(tokens, collapse = " ")
}

data_list$Title <- sapply(data_list$Title, clean_text)
data_list$Content <- sapply(data_list$Content, clean_text)

write.csv(data_list, "E:\\Academics\\DATA SCIENCE\\FINAL\\Project\\cleaned.csv", row.names = FALSE)

print("Data scraping and preprocessing completed successfully!")
