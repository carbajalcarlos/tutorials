# set a working folder within the tutorials
setwd("D:/code/r/tutorials/text_analysis")

# Installing required packages
install.packages("quanteda", dependencies = TRUE)
install.packages("readtext")
install.packages("devtools")
devtools::install_github("quanteda/quanteda.corpora")

# Loading required packages
require("quanteda")
require("readtext")

data_dir <- system.file("extdata/", package = "readtext")
inaug_data <- readtext(paste0(data_dir, "/csv/inaugCorpus.csv"), text_field="texts")
View(inaug_data)

udhr_data <- readtext(paste0(data_dir, "/txt/UDHR/*"))
View(udhr_data)

inaug_corp <- corpus(inaug_data)
ndoc(inaug_corp)
summary(inaug_corp)

full_inaug_corp <- corpus(data_corpus_inaugural)
ndoc(full_inaug_corp)
summary(full_inaug_corp)

recent_corp <- corpus_subset(full_inaug_corp, Year >= 1992)
summary(recent_corp)

sent_inaug_corp <- corpus_reshape(full_inaug_corp, 'sentences')
ndoc(sent_inaug_corp)
summary(sent_inaug_corp, 10)

para_inaug_corp <- corpus_reshape(full_inaug_corp, 'paragraphs') 
ndoc(para_inaug_corp)
summary(para_inaug_corp, 10)

toks <- tokens(inaug_corp, remove_punct=TRUE)
head(toks[[1]],50)

toks1 <- tokens_select(toks, stopwords('english'), selection='remove')
head(toks1[[1]],50) 



data <- read.table("test.txt", sep="\t")
