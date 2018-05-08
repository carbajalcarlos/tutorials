# set a working folder within the tutorials
setwd("D:/code/r/tutorials/text_analysis")

# Installing required packages
install.packages("quanteda", dependencies = TRUE)
install.packages("readtext")
install.packages("devtools")
devtools::install_github("quanteda/quanteda.corpora")

# Loading required packages
require(quanteda)
require(readtext)

data_dir <- system.file("extdata/", package = "readtext")
inaug_data <- readtext(paste0(data_dir, "/csv/inaugCorpus.csv"), text_field="texts")
View(inaug_data)

udhr_data <- readtext(paste0(data_dir, "/txt/UDHR/*"))
fix(udhr_data)