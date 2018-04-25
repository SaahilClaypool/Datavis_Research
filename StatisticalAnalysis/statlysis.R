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
# demographic data statistics
demoDF <- resultsDF[c("age","gender","experience","education")]
demoDF$gender <- as.factor(demoDF$gender)
demoDF$experience <- as.factor(demoDF$experience)
demoDF$education <- as.factor(demoDF$education)
summary(demoDF)
# more specific demographics
skilledDemo <- demoDF[demoDF$experience == "High",]
mediumDemo <- demoDF[demoDF$experience == "Medium",]
lowDemo <- demoDF[demoDF$experience == "Low",]
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
# chi square test of independence
statDF <- resultsDF[c("visType","guessACorrect","guessBCorrect")]
chi <- chisq.test(statDF$visType, statDF$guessACorrect, correct = FALSE)
chisq.test(statDF$visType, statDF$guessBCorrect, correct = FALSE)
chisq.test(statDF$guessACorrect, statDF$guessBCorrect, correct = FALSE)
# pairwise chi square
pair1DF <- statDF[statDF$visType != "interactive",]
pair2DF <- statDF[statDF$visType != "static_interactive",]
chisq.test(pair1DF$visType, pair1DF$guessACorrect, correct = FALSE)
chisq.test(pair1DF$visType, pair1DF$guessBCorrect, correct = FALSE)
chisq.test(pair2DF$visType, pair2DF$guessACorrect, correct = FALSE)
chisq.test(pair2DF$visType, pair2DF$guessBCorrect, correct = FALSE)
# getting accuracy statistics
accuracyDF <- data.frame(matrix(nrow = 0, ncol = 2))
x <- c("visType", "accuracy")
colnames(accuracyDF) <- x
accuracyDF[nrow(accuracyDF) + 1,] <- list("interactive",
                                          length(which(resultsDF$visType=='interactive' & resultsDF$guessBCorrect==TRUE)) / length(which(resultsDF$visType=='interactive')))
accuracyDF[nrow(accuracyDF) + 1,] <- list("static_interactive",
                                          length(which(resultsDF$visType=='static_interactive' & resultsDF$guessBCorrect==TRUE)) / length(which(resultsDF$visType=='static_interactive')))
accuracyDF[nrow(accuracyDF) + 1,] <- list("text",
                                          length(which(resultsDF$visType=='text' & resultsDF$guessBCorrect==TRUE)) / length(which(resultsDF$visType=='text')))
ggplot(data = accuracyDF, aes(x = visType, y = accuracy)) +
  geom_bar(stat = 'identity', width = 0.3, fill = 'lightskyblue', color = 'darkblue') +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) +
  labs(title = "Accuracy of Guess B statistics plot") +
  theme_bw()
# splitting accuracy by Statistics Experience Demographics
splitAccuracyDF <- data.frame(matrix(nrow = 0, ncol = 3))
x <- c("visType", "accuracy", "experience")
colnames(splitAccuracyDF) <- x
splitAccuracyDF[nrow(splitAccuracyDF) + 1,] <- list("interactive",
                                                    length(which(resultsDF$visType=='interactive' &
                                                                   resultsDF$guessBCorrect==TRUE & 
                                                                   resultsDF$experience=='High')) / length(which(resultsDF$visType=='interactive' &
                                                                                                                   resultsDF$experience=='High')),
                                                    "High")
splitAccuracyDF[nrow(splitAccuracyDF) + 1,] <- list("interactive",
                                                    length(which(resultsDF$visType=='interactive' &
                                                                   resultsDF$guessBCorrect==TRUE & 
                                                                   resultsDF$experience=='Medium')) / length(which(resultsDF$visType=='interactive' &
                                                                                                                     resultsDF$experience=='Medium')),
                                                    "Medium")
splitAccuracyDF[nrow(splitAccuracyDF) + 1,] <- list("interactive",
                                                    length(which(resultsDF$visType=='interactive' &
                                                                   resultsDF$guessBCorrect==TRUE & 
                                                                   resultsDF$experience=='Low')) / length(which(resultsDF$visType=='interactive' &
                                                                                                                  resultsDF$experience=='Low')),
                                                    "Low")
splitAccuracyDF[nrow(splitAccuracyDF) + 1,] <- list("static_interactive",
                                                    length(which(resultsDF$visType=='static_interactive' &
                                                                   resultsDF$guessBCorrect==TRUE & 
                                                                   resultsDF$experience=='High')) / length(which(resultsDF$visType=='static_interactive' &
                                                                                                                   resultsDF$experience=='High')),
                                                    "High")
splitAccuracyDF[nrow(splitAccuracyDF) + 1,] <- list("static_interactive",
                                                    length(which(resultsDF$visType=='static_interactive' &
                                                                   resultsDF$guessBCorrect==TRUE & 
                                                                   resultsDF$experience=='Medium')) / length(which(resultsDF$visType=='static_interactive' &
                                                                                                                     resultsDF$experience=='Medium')),
                                                    "Medium")
splitAccuracyDF[nrow(splitAccuracyDF) + 1,] <- list("static_interactive",
                                                    length(which(resultsDF$visType=='static_interactive' &
                                                                   resultsDF$guessBCorrect==TRUE & 
                                                                   resultsDF$experience=='Low')) / length(which(resultsDF$visType=='static_interactive' &
                                                                                                                  resultsDF$experience=='Low')),
                                                    "Low")
splitAccuracyDF[nrow(splitAccuracyDF) + 1,] <- list("text",
                                                    length(which(resultsDF$visType=='text' &
                                                                   resultsDF$guessBCorrect==TRUE & 
                                                                   resultsDF$experience=='High')) / length(which(resultsDF$visType=='text' &
                                                                                                                   resultsDF$experience=='High')),
                                                    "High")
splitAccuracyDF[nrow(splitAccuracyDF) + 1,] <- list("text",
                                                    length(which(resultsDF$visType=='text' &
                                                                   resultsDF$guessBCorrect==TRUE & 
                                                                   resultsDF$experience=='Medium')) / length(which(resultsDF$visType=='text' &
                                                                                                                     resultsDF$experience=='Medium')),
                                                    "Medium")
splitAccuracyDF[nrow(splitAccuracyDF) + 1,] <- list("text",
                                                    length(which(resultsDF$visType=='text' &
                                                                   resultsDF$guessBCorrect==TRUE & 
                                                                   resultsDF$experience=='Low')) / length(which(resultsDF$visType=='text' &
                                                                                                                  resultsDF$experience=='Low')),
                                                    "Low")
ggplot(data = splitAccuracyDF, 
       aes(x = visType, y = accuracy, fill = experience)) + 
  geom_bar(stat = 'identity', width = 0.4, position = position_dodge(), color = 'darkblue') +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Accuracy of Guess B (by Statistics Experience)")
