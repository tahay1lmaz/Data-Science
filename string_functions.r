library(tidyverse)

# Take 100 random sentences and split them into words (without punctuation)
words1 <- sample(sentences,100)
words2 <-str_split(words1,boundary("word")) %>%
  simplify()
words2 <- unique(words2)
words2

# Q1 : Find words which are starting with “a” and ending with “e”.
# Answer 1
words2[str_detect(words2,"^a.+e$")]

# Q2 : Calculate the number of words which have more than 3 vowels.
# Answer 2
sum(str_count(words2,"[aeiou]")>3)
words2[str_count(words2,"[aeiou]")>3]

# Q3 : List the five longest word in your data
# Answer 3
new_data <- tibble(words2, longest=(str_length(words2)))
long <- arrange(new_data,desc(longest))
head(long,5)

# Q4 : Try to find word(s) which contain any of these words: age, any, day, exp, her, pro, the.
# Answer 4
find_words<-c("age","any","day","exp","her","pro","the")
find_words2<-str_c(find_words, collapse = "|")
str_subset(words2,find_words2)
