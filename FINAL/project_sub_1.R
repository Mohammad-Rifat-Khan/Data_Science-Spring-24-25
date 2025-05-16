library(rvest)

# Install and load the 'tm' package for stop word removal
install.packages('tm')
library(tm)

# Install and load the 'SnowballC' package for stemming
install.packages('SnowballC')
library(SnowballC)

# Install and load the 'hunspell' package for spell checking
install.packages('hunspell')
library(hunspell)

# Specify the URL to scrape
url <- "https://edition.cnn.com/2025/05/13/asia/bangladesh-sheikh-hasina-awami-party-intl-hnk"

# Read the webpage HTML
webpage <- read_html(url)

# Extract text from elements with class "inline-placeholder"
title <- html_nodes(webpage, ".inline-placeholder") %>% html_text()

# Preview the first few extracted titles
print(head(title))

# Store the titles in corpus
corpus <- title

# Handling Contractions: Expand contractions in the text
contractions <- c("can't" = "cannot", "won't" = "will not", "don't" = "do not",
                  "isn't" = "is not", "aren't" = "are not", "wasn't" = "was not",
                  "weren't" = "were not", "hasn't" = "has not", "haven't" = "have not",
                  "hadn't" = "had not", "doesn't" = "does not", "didn't" = "did not",
                  "i'm" = "i am", "you're" = "you are", "he's" = "he is",
                  "she's" = "she is", "it's" = "it is", "we're" = "we are",
                  "they're" = "they are", "i've" = "i have", "you've" = "you have",
                  "we've" = "we have", "they've" = "they have", "i'd" = "i would",
                  "you'd" = "you would", "he'd" = "he would", "she'd" = "she would",
                  "we'd" = "we would", "they'd" = "they would", "i'll" = "i will",
                  "you'll" = "you will", "he'll" = "he will", "she'll" = "she will",
                  "we'll" = "we will", "they'll" = "they will")
for (contr in names(contractions)) {
  corpus <- gsub(paste0("\\b", contr, "\\b"), contractions[contr], corpus, ignore.case = TRUE)
}

# Preview the text after handling contractions
print(head(corpus))

# Clean the corpus
corpus <- gsub("<.*?>", "", corpus)  # Remove HTML tags
corpus <- tolower(corpus)            # Convert text to lowercase
corpus <- gsub("[^a-z ]", "", corpus) # Remove non-letter characters (punctuation, numbers, special characters)
corpus <- gsub("\\s+", " ", corpus)   # Replace multiple spaces with a single space
corpus <- trimws(corpus)              # Trim leading and trailing whitespace

# Preview the cleaned text
print(head(corpus))

# Tokenization: Split the cleaned text into words (tokens)
tokens <- strsplit(corpus, " ")

# Preview the first few tokenized entries
print(head(tokens))

# Spell Checking: Correct spelling errors in the tokens
correct_spelling <- function(token_list) {
  corrected <- lapply(token_list, function(words) {
    # Check spelling for each word
    misspelled <- hunspell_check(words)
    corrected_words <- words
    # For misspelled words, replace with the first suggestion (if available)
    for (i in seq_along(words)) {
      if (!misspelled[i]) {
        suggestions <- hunspell_suggest(words[i])[[1]]
        if (length(suggestions) > 0) {
          corrected_words[i] <- suggestions[1]  # Use the first suggestion
        }
      }
    }
    return(corrected_words)
  })
  return(corrected)
}

# Apply spell checking to the tokens
corrected_tokens <- correct_spelling(tokens)

# Preview the first few corrected tokenized entries
print(head(corrected_tokens))

# Stop Words Removal
stop_words <- stopwords("en")  # Get a list of English stop words
filtered_tokens <- lapply(corrected_tokens, function(x) x[!x %in% stop_words])  # Remove stop words

# Preview the first few filtered tokenized entries
print(head(filtered_tokens))

# Stemming: Reduce words to their base form
stemmed_tokens <- lapply(filtered_tokens, function(x) wordStem(x, language = "english"))

# Preview the first few stemmed tokenized entries
print(head(stemmed_tokens))
