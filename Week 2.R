install.packages("tidyverse")
install.packages("tokenizers")

library(tidyverse)
library(tokenizers)

text <- paste("Now, I understand that because it's an election season",
              "expectations for what we will achieve this year are low.",
              "But, Mister Speaker, I appreciate the constructive approach",
              "that you and other leaders took at the end of last year",
              "to pass a budget and make tax cuts permanent for working",
              "families. So I hope we can work together this year on some",
              "bipartisan priorities like criminal justice reform and",
              "helping people who are battling prescription drug abuse",
              "and heroin abuse. So, who knows, we might surprise the",
              "cynics again")

words <- tokenize_words(text)
words

length(words)

length(words[[1]])

tab <- table(words[[1]])
tab <- data_frame(word = names(tab), count = as.numeric(tab))
tab

arrange(tab, desc(count))

sentences <- tokenize_sentences(text)
sentences
