# ----- Preparation of working dataset -----
# set a working folder within the tutorials
setwd("H:/code/r/tutorials/text_analysis")

# If required, install libraries used
if (FALSE) {
  install.packages("quanteda", dependencies = TRUE)
  install.packages("readtext")
  install.packages("devtools")
  devtools::install_github("quanteda/quanteda.corpora") # this installation requires admin privileges
}

# Loading the required directories
require(quanteda)
require(readtext)

# Loading data
yoo_2010_txt <- readtext(file = "1_data/2010-yoo_etal-ISR.txt", text_field="texts")

# ----- Formation of corpus -----
# Creation of the corpus file from text
yoo_2010 <- corpus(yoo_2010_txt)
ndoc(yoo_2010)
summary(yoo_2010)

# Creation of reshaped files
# Paragraphs
yoo_2010_para <- corpus_reshape(x = yoo_2010, to = 'paragraphs')
ndoc(yoo_2010_para)
summary(yoo_2010_para, 10)
# Sentences
yoo_2010_sent <- corpus_reshape(x = yoo_2010, to = 'sentences')
ndoc(yoo_2010_sent)
summary(yoo_2010_sent, 10)

# ----- Tokenisation -----
# Separation of files by tokens
yoo_2010_tkns <- tokens(x = yoo_2010_para,
                        remove_punct = TRUE,
                        remove_symbols = TRUE, 
                        remove_hyphens = TRUE)
head(yoo_2010_tkns[[1]],50)

# Removing stopwords (words without statistical meaning) from tokens
yoo_2010_tkns_clean <- tokens_select(x = yoo_2010_tkns,
                                     pattern = stopwords('english'),
                                     selection='remove')
head(yoo_2010_tkns_clean[[1]],50)

# Creation of compound tokens
yoo_2010_tkns_cmpnd <- tokens_compound(x = yoo_2010_tkns_clean,
                                       pattern = phrase(c('Digital Innovation')), 
                                       concatenator = "_") # this is optional default is "_"
head(yoo_2010_tkns_cmpnd[[1]],50)

# Creation of n-gram tokens
yoo_2010_tkns_ngram <- tokens_ngrams(x = yoo_2010_tkns_clean,
                                     n=2)
head(yoo_2010_tkns_ngram[[1]],50)

# creation of compound n-gram tokens
yoo_2010_tkns_cgram <- tokens_ngrams(x = yoo_2010_tkns_clean,
                       n=2:3)
head(yoo_2010_tkns_cgram[[1]],50)
tail(yoo_2010_tkns_cgram[[1]],50)

# ----- Document-Feature Matrix DFM -----
# Creates a matrix paragraph vs feature (word)
yoo_2010_dfm <- dfm(yoo_2010_tkns_clean)
yoo_2010_dfm

# Trim all the features (words) that appear less than 5 times
yoo_2010_dfm_trim <- dfm_trim(x = yoo_2010_dfm, min_termfreq = 3)
yoo_2010_dfm_trim

# This example allows to do all the preparation in just one pipeline
yoo_2010_dfm_real <- corpus(yoo_2010) %>%
  dfm(remove = stopwords('english'), remove_punct = TRUE) %>%
  dfm_weight( scheme = "prop")

# ----- Simple analysis -----
# Top features (words) by frequency in the dfm
topfeatures(yoo_2010_dfm, 25)
# NOTE: This ranking does not consideres the unit of analysis, but all the apperances in the DFM

# top features (words) by proportion of the dfm
yoo_2010_dfm_prop <- dfm_weight(x = yoo_2010_dfm, scheme = "prop")
# NOTE: The proportion is calculated by the frequency per unit of analysis (paragraph, sentence, document)
topfeatures(yoo_2010_dfm_prop, 25)

# 
yoo_2010_dfm_freq <- textstat_frequency(x = yoo_2010_dfm, n = 20)
head(x = yoo_2010_dfm_freq, n = 20)

# ----- Simple plotting -----
library(ggplot2)

# Plotting by global frequency
x11()
ggplot(data = yoo_2010_dfm_freq,
       mapping =  aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y= "Frequency") +
  theme_minimal()

# Plotting by unit of analysis frequency
x11()
ggplot(data = yoo_2010_dfm_freq,
       mapping =  aes(x = reorder(feature, docfreq), y = docfreq)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Document Frequency") +
  theme_minimal()

# Separation per groups
## this part is missing##
if (FALSE) {
  # Separations per groups (subsetting)
  freq_weight <- textstat_frequency(dfm_weight_pres, n = 10, groups = "President")
  head(freq_weight,30)
  # Plotting subset
  x11()
  ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
    geom_point() +
    facet_wrap(~ group, scales = "free") +
    coord_flip() +
    scale_x_continuous(breaks = nrow(freq_weight):1,
                       labels = freq_weight$feature) +
    labs(x = NULL, y = "Relative frequency")
}

# ----- Wordclouds creation -----
# Creation of wordclouds based on the DFM
set.seed(69)
x11()
textplot_wordcloud(x = yoo_2010_dfm)
# Adding colours to the wordclouds
x11()
textplot_wordcloud(x = yoo_2010_dfm,
                   color = c('red', 'pink', 'green', 'purple', 'orange', 'blue'))

# Wordcloud based in a subset
if (FALSE) {
  # Wordcloud creation from a specific subset
  obama_dfm <-
    dfm(corpus_subset(data_corpus_inaugural, President == "Obama"),
        remove = stopwords("english"), remove_punct = TRUE) %>%
    dfm_trim(min_count = 3)
  set.seed(10)
  x11()
  textplot_wordcloud(obama_dfm)
  # Worldcloud comparison betweeen three subsets
  first3_dfm <- corpus_subset(data_corpus_inaugural,
                              President %in% c("Washington", "Adams", "Jefferson")) %>%
    dfm(groups = "President", remove = stopwords("english"), remove_punct = TRUE) %>%
    dfm_trim(min_count = 10, verbose = FALSE)
  set.seed(123)
  x11()
  textplot_wordcloud(first3_dfm, comparison = TRUE)
  # Worldcloud comparison between two subsets
  obama_trump_dfm <-
    dfm(corpus_subset(data_corpus_inaugural, President %in% c("Obama", "Trump")),
        remove = stopwords("english"), remove_punct = TRUE, groups = "President") %>%
    dfm_trim(min_count = 3)
  set.seed(123)
  x11()
  textplot_wordcloud(obama_trump_dfm, comparison= TRUE, max_words = 300,
                     color = c("blue", "red"))
}

# ----- Keyword in Context -----
# Using a global expresion
yoo_2010_kwic_glob <- kwic(x = yoo_2010,
                           pattern =  "digital*",
                           window = 5,
                           valuetype = "glob")
head(yoo_2010_kwic_glob)
# Using regular expresion
yoo_2010_kwic_rege <- kwic(x = yoo_2010,
                           pattern =  "digit",
                           window = 5,
                           valuetype = "regex")
head(yoo_2010_kwic_rege)

# Using regular expresion
yoo_2010_kwic_fixe <- kwic(x = yoo_2010,
                           pattern =  "digital",
                           window = 5,
                           valuetype = "fixed")
head(yoo_2010_kwic_fixe)
