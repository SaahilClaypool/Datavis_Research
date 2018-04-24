# CS 573 - Data Vis Research - Bayesian lab analysis
library('ggplot2')
library('rstudioapi')
library('jsonlite')
# setting working directory
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())
# importing data
raw_data <- fromJSON("data-sofar.json")
raw2 <- fromJSON("turkData.json")
raw3 <- fromJSON("uniqueTurk.json")
# check for NA entries
colnames(raw_data)[colSums(is.na(raw_data)) > 0]
colnames(raw2)[colSums(is.na(raw2)) > 0]
colnames(raw3)[colSums(is.na(raw3)) > 0]
# duplicates were removed in raw3
resultsDF <- raw3
# determine if participants were CORRECT or NOT in their guesses
attach(resultsDF)
resultsDF$guessACorrect = as.factor(
  guessA == ceiling(questionN * data$has_condition * data$positive_condition) +
            ceiling((questionN - questionN * data$has_condition) * data$positive_no_condition)  
)
resultsDF$guessBCorrect = as.factor(
  guessB == ceiling(questionN * data$has_condition * data$positive_condition)
)
# basic plot
ggplot(data = resultsDF, aes(x = visType, fill = factor(guessACorrect, levels = c("TRUE","FALSE")))) +
  geom_bar(position = 'fill', width = 0.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  # scale_y_continuous(breaks = c(5,10,15,20,25), labels = scales::identity_pal()) +
  scale_fill_manual("Guess A was correct?", values = c("TRUE" = "green3", "FALSE" = "red2")) +
  labs(fill = "Guess A was correct?", title = "Guess A statistical analysis")
ggplot(data = resultsDF, 
  aes(x = visType, fill = factor(guessBCorrect, levels = c("TRUE","FALSE")))) +
  geom_bar(position = 'fill', width = 0.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  #scale_y_continuous(breaks = c(5,10,15,20,25), labels = scales::identity_pal()) +
  scale_fill_manual("Guess B was correct?", values = c("TRUE" = "green3", "FALSE" = "red2")) +
  labs(fill = "Guess B was correct?", title = "Guess B statistical analysis")
