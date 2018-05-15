Test2 <- read.table(file = "D:/code/r/tutorials/text_analysis/Test2.txt", stringsAsFactors = FALSE,
                    sep = "\t", header = TRUE)

test <- corpus(x = Test2, text_field = "X")
