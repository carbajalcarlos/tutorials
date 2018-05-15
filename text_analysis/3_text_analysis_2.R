# ===== SETUP =====
# Loading required libraries
require(quanteda, quietly = TRUE)
require(readtext, quietly = TRUE)

# Loading required data
full_inaug_corp <- corpus(data_corpus_inaugural)
ndoc(full_inaug_corp)
summary(full_inaug_corp)

# Creation of tokens
toks <- tokens(full_inaug_corp, remove_punct=TRUE)
toks <- tokens_select(toks, stopwords('english'), selection='remove')

dfm1 <- dfm(toks)

# ===== Text analysis =====