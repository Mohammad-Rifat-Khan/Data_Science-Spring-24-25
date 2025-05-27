##Topic Modelling
#Loading necessary libraries for topic modelling:
# Data Wrangling
library(tidyverse)

# Text Processing
library(tm)
#library(corpus) #Not available
library(tidytext)
library(textclean)
library(lubridate)
library(hunspell)
library(SnowballC)
library(textmineR)
library(scales)
library(dplyr)
library(textstem)

# Visualization
library(ggwordcloud)

# Modeling and Evaluation
library(randomForest)
library(e1071)
library(yardstick)



options(scipen = 999)

#Import Data:
file_path <- "W:\\Saron AIUB Related Documents\\8th Semester (17 Credits)\\Introduction To Data Science\\Final Term Assignment\\final_scrapped.csv"
investigative_news <- data.table::fread(file_path, header = T, encoding = "Latin-1")

glimpse(investigative_news)

# Data Pre Processing:
# 3.1 Text Cleansing:
investigative_clean <- investigative_news %>% 
  mutate(text_clean = Content %>% 
           replace_non_ascii() %>% 
           replace_html(symbol = F) %>% # remove html tag
           str_replace_all("[0-9]", " ") %>% 
           str_replace_all("[-|]", " ") %>% # replace "-" with space
           tolower() %>% #lowercase
           # str_remove_all("coronavirus|covid 19|covid|canadian|canadians") %>%  # remove common words
           replace_symbol() %>%
           replace_contraction() %>% 
           replace_word_elongation() %>%  # lengthen shortened word
           str_replace_all("[[:punct:]]", " ") %>% # remove punctuation
           str_replace_all(" dr ", " doctor ") %>% 
           make_plural() %>%
           str_replace_all(" s ", " ") %>%  
           str_squish() %>% # remove double whitespace
           str_trim() # remove whitespace at the start and end of the text
  )




# 3.2 Remaining number of words on each document:
document_length <- sapply(strsplit(investigative_clean$text, " "), length)

document_length %>% 
  summary()


# 3.3 Filtering Documents with less than 100 words:
investigative_clean <- investigative_clean %>% 
  slice(which(document_length > 100))

dim(investigative_clean)

#Document-Term Matrix: Tokenizing the text
# 4.1 Custom Stemming Function:
stem_hunspell <- function(term) {
  # look up the term in the dictionary
  stems <- hunspell_stem(term)[[1]]
  
  if (length(stems) == 0) { # if there are no stems, use the original term
    stem <- term
  } else { # if there are multiple stems, use the last one
    stem <- stems[[length(stems)]]
  }
  return(stem)
}


#4.2  Tokenization:
news_term <- investigative_clean %>%
  unnest_tokens(output = "word", input = text_clean) %>%
  #4.3 Removing Stop Words:
  anti_join(stop_words, by = "word") %>%
  #4.4 Removing Missing Words:
  drop_na(word) %>%
  #4.5 Counting Word Occurrences:
  count(V1, word)




# 4.6 Transforming the data into document-term matrix (DTM):
dtm_news <- news_term %>% 
  cast_dtm(document = V1, term = word, value = n)

inspect(dtm_news)
# 4.7 Removing rare word:
word_freq <- findFreqTerms(dtm_news, 
                           lowfreq = 5, 
                           highfreq = nrow(dtm_news)*0.9
)

dtm_news <- dtm_news[ , word_freq]
dtm_news



#Topic Modelling with LDA:
#5.1 Fitting the model:(pre trained)

dtm_lda <- Matrix::Matrix(as.matrix(dtm_news), sparse = T)

set.seed(123)
lda_news <- FitLdaModel(dtm = dtm_lda, 
                        k = 6, 
                        iterations = 5000,
                        burnin = 4000, 
                        calc_coherence = T
)


#5.2 Posterior per-document-per-topic probabilities:
lda_news$theta %>% 
  head() %>% 
  as.data.frame() %>% 
  set_names(paste("Topic", 1:6)) %>% 
  rownames_to_column("document")  
#5.3 top terms for each topic:
GetTopTerms(lda_news$phi, 10) %>% 
  as.data.frame()





#Exploration:
#6.1 Word-Topic Probabilities:
news_word_topic <- GetTopTerms(lda_news$phi, 30) %>% 
  as.data.frame() %>% 
  set_names(paste("Topic", 1:6))

news_word_topic
#6.2 Visualization:
news_word_topic %>% 
  rownames_to_column("id") %>%
  mutate(id = as.numeric(id)) %>% 
  pivot_longer(-id, names_to = "topic", values_to = "term") %>% 
  ggplot(aes(label = term, size = rev(id), color = topic, alpha = rev(id))) +
  geom_text_wordcloud(seed = 123) +
  facet_wrap(~topic, scales = "free") +
  scale_alpha_continuous(range = c(0.4, 1)) +
  scale_color_manual(values = c( "dodgerblue4", "firebrick4", "darkgreen", "purple","orange","cyan" )) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "firebrick"),
        strip.text.x = element_text(colour = "white"))

