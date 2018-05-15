# ----- SETUP -----
# Required libraries
require(quanteda, quietly = TRUE)
require(readtext, quietly = TRUE)
require(ggplot2, quietly = TRUE)

# Loading required datasets
full_inaug_corp <- corpus(data_corpus_inaugural)
ndoc(full_inaug_corp)
summary(full_inaug_corp)

# Creation of tokens
toks <- tokens(full_inaug_corp, remove_punct=TRUE)
toks <- tokens_select(toks, stopwords('english'), selection='remove')
dfm1 <- dfm(toks)

# ----- Text analysis -----
### Lexical diversity
lexdiv <- textstat_lexdiv(dfm1) 
head(lexdiv, 5)
tail(lexdiv, 5)

x11()
ggplot(data=lexdiv, aes(x=document, y=TTR)) + 
  geom_point() + coord_flip() + 
  labs(x="President",y="Type-Token Ratio")

### Keyness
period <- ifelse(docvars(data_corpus_inaugural, "Year") < 1945, "pre-war", "post-war")
dfm2 <- dfm(toks, groups = period)
head(dfm2)
# This analysis organizes the words by chi2
result <- textstat_keyness(dfm2)
head(result, 10)
tail(result, 10)

x11()
textplot_keyness(result)

# This analysis organizes the words by odds ratios
result1 <-  textstat_keyness(dfm2, measure="exact")

# This is optional to eliminate 0 and infinite values
result1 <- subset(x = result1, subset = or != 0 & is.infinite(or) == FALSE)

head(result1, 10)
tail(result1, 10)

### Document similarity
presDfm <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980),
               remove = stopwords("english"), 
               stem = TRUE, 
               remove_punct = TRUE)
simil1 <- textstat_simil(presDfm, margin="documents") 
simil1

### Locations of keywords in Documents
data_corpus_inaugural_subset <- corpus_subset(data_corpus_inaugural, Year > 1980)
x11()
textplot_xray(kwic(data_corpus_inaugural_subset, "american"),
              kwic(data_corpus_inaugural_subset, "people"))

x11()
textplot_xray(kwic(data_corpus_inaugural_subset, "freedom"),
              kwic(data_corpus_inaugural_subset, "rights"))

### Sentiment analysis
lengths(data_dictionary_LSD2015)

lsd_dfm <- dfm_lookup(dfm1, data_dictionary_LSD2015)
head(lsd_dfm)
tail(lsd_dfm)

# simple plot (positive/negative frequency)
lsd <- as.data.frame(lsd_dfm)
lsd

x11() 
ggplot(data=lsd, aes(x=document, y=negative)) +
  geom_point() + coord_flip() +
  labs(x="President",y="Negative Words")

x11()
ggplot(data=lsd, aes(x=document, y=positive)) +
  geom_point() + coord_flip() +
  labs(x="President",y="Positive Words")

# Spetial plots (proportion negative to positive)
lsd$overall <- (lsd$positive-lsd$negative)/lsd$tokn_count
x11()
ggplot(data=lsd, aes(x=document, y=overall)) +
  geom_point() + coord_flip() +
  labs(x="President",y="Overall Sentiment")

# Special plots (proportion of negative over the whole tokens)
lsd$tokn_count <- 0
for (i in 1:nrow(lsd)) {  lsd$tokn_count[i] <- length(toks[[i]])  }
x11()
ggplot(data=lsd, aes(x=document, y=negative/tokn_count)) +
  geom_point() + coord_flip() +
  labs(x="President",y="Negativity")
x11()
ggplot(data=lsd, aes(x=document, y=positive/tokn_count)) +
  geom_point() + coord_flip() +
  labs(x="President",y="Positivity")
