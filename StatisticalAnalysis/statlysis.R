# CS 573 - Data Vis Research - Bayesian lab analysis
library('ggplot2')
library('rstudioapi')
library('jsonlite')
# setting working directory
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())

# importing data
raw_data <- data.frame(fromJSON("data-sofar.json", flatten = TRUE), check.rows = FALSE)
raw3 <- data.frame(fromJSON("uniqueTurk.json", flatten = TRUE), check.rows = FALSE)
raw_data$postId = NA #the data-sofar did not have the postId at the time
allRaw <- rbind(raw3, raw_data)
# fixedAllRaw is the RAW DF
fixedAllRaw <- allRaw
fixedAllRaw$visType <- as.factor(fixedAllRaw$visType)
fixedAllRaw$gender <- as.factor(fixedAllRaw$gender)
fixedAllRaw$experience <- as.factor(fixedAllRaw$experience)
fixedAllRaw$education <- as.factor(fixedAllRaw$education)
nrow(fixedAllRaw)
summary(fixedAllRaw)

# check for NA entries
colnames(raw_data)[colSums(is.na(raw_data)) > 0]
colnames(raw2)[colSums(is.na(raw2)) > 0]
colnames(raw3)[colSums(is.na(raw3)) > 0]
# duplicates were removed in raw3
resultsDF <- raw3
resultsDF <- fixedAllRaw
# calculating Log-base-2 error values
attach(resultsDF)
resultsDF$logErrorGuessA = log(abs(guessA - 
                                      (ceiling(questionN * data.has_condition * data.positive_condition) +
                                      ceiling((questionN - questionN * data.has_condition) * data.positive_no_condition))) + 1/8,2)
resultsDF$logErrorGuessB = log(abs(guessB - 
                                      (ceiling(questionN * data.has_condition * data.positive_condition))) + 1/8,2)
# setting -3 value to 0 (these values mean the guess was CORRECT)
resultsDF$logErrorGuessA[resultsDF$logErrorGuessA == -3] = 0
resultsDF$logErrorGuessB[resultsDF$logErrorGuessB == -3] = 0
# chisquare test for log error values
chisq.test(resultsDF$visType, resultsDF$logErrorGuessA, correct = FALSE)
chisq.test(resultsDF$visType, resultsDF$logErrorGuessB, correct = FALSE)
# logError plots
png('logError-guessA.png', units="in", width=5, height=4, res=300)
ggplot(resultsDF, aes(x = visType, y = logErrorGuessA)) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 3) +
  stat_summary(fun.data = "mean_cl_boot", color = "red", size = 1, shape = 18, geom = "pointrange") +
  labs(title = "Guess A log-2 error plot") +
  theme_bw()
dev.off()
png('logError-guessB.png', units="in", width=5, height=4, res=300)
ggplot(resultsDF, aes(x = visType, y = logErrorGuessB)) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 3) +
  stat_summary(fun.data = "mean_cl_boot", color = "red", size = 1, shape = 18, geom = "pointrange") +
  labs(title = "Guess B log-2 error plot") +
  theme_bw()
dev.off()
# determine if participants were CORRECT or NOT in their guesses
resultsDF$guessACorrect = as.factor(
  guessA == ceiling(questionN * data.has_condition * data.positive_condition) +
            ceiling((questionN - questionN * data.has_condition) * data.positive_no_condition)
)
resultsDF$guessBCorrect = as.factor(
  guessB == ceiling(questionN * data.has_condition * data.positive_condition)
)

# indepedent two-sample t-test
TwoGroup.ST <- resultsDF[resultsDF$visType != "interactive",]
TwoGroup.IT <- resultsDF[resultsDF$visType != "static_interactive",]
TwoGroup.IS <- resultsDF[resultsDF$visType != "text",]
t.test(logErrorGuessA ~ visType, data = TwoGroup.ST)
t.test(logErrorGuessB ~ visType, data = TwoGroup.ST)
t.test(logErrorGuessA ~ visType, data = TwoGroup.IT)
t.test(logErrorGuessB ~ visType, data = TwoGroup.IT)
t.test(logErrorGuessA ~ visType, data = TwoGroup.IS)
t.test(logErrorGuessB ~ visType, data = TwoGroup.IS)

# the separated correct guesses dataframes
bothCorrectDF <- resultsDF[resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE,]
bothCorrectDF$allCorrect <- TRUE
notCorrectDF <- resultsDF[!(resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE),]
notCorrectDF$allCorrect <- FALSE
correctOrNotDF <- rbind(bothCorrectDF, notCorrectDF)
chisq.test(correctOrNotDF$visType, correctOrNotDF$allCorrect, correct = FALSE)
# pairwise
TwoGroup.ST2 <- correctOrNotDF[resultsDF$visType != "interactive",]
TwoGroup.IT2 <- correctOrNotDF[resultsDF$visType != "static_interactive",]
TwoGroup.IS2 <- correctOrNotDF[resultsDF$visType != "text",]
chisq.test(TwoGroup.ST2$visType, TwoGroup.ST2$allCorrect, correct = FALSE)
chisq.test(TwoGroup.IT2$visType, TwoGroup.IT2$allCorrect, correct = FALSE)
chisq.test(TwoGroup.IS2$visType, TwoGroup.IS2$allCorrect, correct = FALSE)
guessACorrectDF <- resultsDF[resultsDF$guessACorrect==TRUE,]
guessBCorrectDF <- resultsDF[resultsDF$guessBCorrect==TRUE,]

#######################################################################################
# the following below is for ALL 104 participants

# demographic data statistics
demoDF <- resultsDF[c("age","gender","experience","education")]
summary(demoDF)
# more specific demographics
skilledDemo <- demoDF[demoDF$experience == "High",]
mediumDemo <- demoDF[demoDF$experience == "Medium",]
lowDemo <- demoDF[demoDF$experience == "Low",]
# basic plot
ggplot(data = resultsDF, aes(x = visType, fill = factor(guessACorrect, levels = c("FALSE","TRUE")))) +
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
                                          length(which(resultsDF$visType=='interactive' & resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE)) / length(which(resultsDF$visType=='interactive')))
accuracyDF[nrow(accuracyDF) + 1,] <- list("static_interactive",
                                          length(which(resultsDF$visType=='static_interactive' & resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE)) / length(which(resultsDF$visType=='static_interactive')))
accuracyDF[nrow(accuracyDF) + 1,] <- list("text",
                                          length(which(resultsDF$visType=='text' & resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE)) / length(which(resultsDF$visType=='text')))
png('Accuracy.png', units="in", width=5, height=4, res=300)
#insert ggplot code
ggplot(data = accuracyDF, aes(x = visType, y = accuracy)) +
  geom_bar(stat = 'identity', width = 0.3, fill = 'lightskyblue', color = 'darkblue') +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) +
  labs(title = "Accuracy statistics plot") +
  theme_bw()
dev.off()
# splitting accuracy by Statistics Experience Demographics
splitAccuracyDF <- data.frame(matrix(nrow = 0, ncol = 3))
x <- c("visType", "accuracy", "experience")
colnames(splitAccuracyDF) <- x
splitAccuracyDF[nrow(splitAccuracyDF) + 1,] <- list("interactive",
                                                    length(which(resultsDF$visType=='interactive' &
                                                                   resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE & 
                                                                   resultsDF$experience=='High')) / length(which(resultsDF$visType=='interactive' &
                                                                                                                   resultsDF$experience=='High')),
                                                    "High")
splitAccuracyDF[nrow(splitAccuracyDF) + 1,] <- list("interactive",
                                                    length(which(resultsDF$visType=='interactive' &
                                                                   resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE & 
                                                                   resultsDF$experience=='Medium')) / length(which(resultsDF$visType=='interactive' &
                                                                                                                     resultsDF$experience=='Medium')),
                                                    "Medium")
splitAccuracyDF[nrow(splitAccuracyDF) + 1,] <- list("interactive",
                                                    length(which(resultsDF$visType=='interactive' &
                                                                   resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE & 
                                                                   resultsDF$experience=='Low')) / length(which(resultsDF$visType=='interactive' &
                                                                                                                  resultsDF$experience=='Low')),
                                                    "Low")
splitAccuracyDF[nrow(splitAccuracyDF) + 1,] <- list("static_interactive",
                                                    length(which(resultsDF$visType=='static_interactive' &
                                                                   resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE & 
                                                                   resultsDF$experience=='High')) / length(which(resultsDF$visType=='static_interactive' &
                                                                                                                   resultsDF$experience=='High')),
                                                    "High")
splitAccuracyDF[nrow(splitAccuracyDF) + 1,] <- list("static_interactive",
                                                    length(which(resultsDF$visType=='static_interactive' &
                                                                   resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE & 
                                                                   resultsDF$experience=='Medium')) / length(which(resultsDF$visType=='static_interactive' &
                                                                                                                     resultsDF$experience=='Medium')),
                                                    "Medium")
splitAccuracyDF[nrow(splitAccuracyDF) + 1,] <- list("static_interactive",
                                                    length(which(resultsDF$visType=='static_interactive' &
                                                                   resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE & 
                                                                   resultsDF$experience=='Low')) / length(which(resultsDF$visType=='static_interactive' &
                                                                                                                  resultsDF$experience=='Low')),
                                                    "Low")
splitAccuracyDF[nrow(splitAccuracyDF) + 1,] <- list("text",
                                                    length(which(resultsDF$visType=='text' &
                                                                   resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE & 
                                                                   resultsDF$experience=='High')) / length(which(resultsDF$visType=='text' &
                                                                                                                   resultsDF$experience=='High')),
                                                    "High")
splitAccuracyDF[nrow(splitAccuracyDF) + 1,] <- list("text",
                                                    length(which(resultsDF$visType=='text' &
                                                                   resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE & 
                                                                   resultsDF$experience=='Medium')) / length(which(resultsDF$visType=='text' &
                                                                                                                     resultsDF$experience=='Medium')),
                                                    "Medium")
splitAccuracyDF[nrow(splitAccuracyDF) + 1,] <- list("text",
                                                    length(which(resultsDF$visType=='text' &
                                                                   resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE & 
                                                                   resultsDF$experience=='Low')) / length(which(resultsDF$visType=='text' &
                                                                                                                  resultsDF$experience=='Low')),
                                                    "Low")
png('splitAccuracy-statExp.png', units="in", width=5, height=4, res=300)
ggplot(data = splitAccuracyDF, 
       aes(x = visType, y = accuracy, fill = experience)) + 
  geom_bar(stat = 'identity', width = 0.4, position = position_dodge(), color = 'darkblue') +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Accuracy (by Statistics Experience)") +
  theme_bw()
dev.off()
# splitting accuracy by education
splitEduAccuracyDF <- data.frame(matrix(nrow = 0, ncol = 3))
x <- c("visType", "accuracy", "education")
colnames(splitEduAccuracyDF) <- x
splitEduAccuracyDF[nrow(splitEduAccuracyDF) + 1,] <- list("interactive",
                                                      length(which(resultsDF$visType=='interactive' &
                                                                     resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE  & 
                                                                     resultsDF$education=='High School')) / length(which(resultsDF$visType=='interactive' &
                                                                                                                                resultsDF$education=='High School')),
                                                      "High School")
splitEduAccuracyDF[nrow(splitEduAccuracyDF) + 1,] <- list("interactive",
                                                      length(which(resultsDF$visType=='interactive' &
                                                                     resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE  & 
                                                                     resultsDF$education=='2 Year College')) / length(which(resultsDF$visType=='interactive' &
                                                                                                                                   resultsDF$education=='2 Year College')),
                                                      "2 Year College")
splitEduAccuracyDF[nrow(splitEduAccuracyDF) + 1,] <- list("interactive",
                                                      length(which(resultsDF$visType=='interactive' &
                                                                     resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE  & 
                                                                     resultsDF$education=='4 Year College')) / length(which(resultsDF$visType=='interactive' &
                                                                                                                                   resultsDF$education=='4 Year College')),
                                                      "4 Year College")
splitEduAccuracyDF[nrow(splitEduAccuracyDF) + 1,] <- list("interactive",
                                                      length(which(resultsDF$visType=='interactive' &
                                                                     resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE  & 
                                                                     resultsDF$education=='Masters')) / length(which(resultsDF$visType=='interactive' &
                                                                                                                            resultsDF$education=='Masters')),
                                                      "Masters")
splitEduAccuracyDF[nrow(splitEduAccuracyDF) + 1,] <- list("interactive",
                                                      length(which(resultsDF$visType=='interactive' &
                                                                     resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE  & 
                                                                     resultsDF$education=='MD')) / length(which(resultsDF$visType=='interactive' &
                                                                                                                       resultsDF$education=='MD')),
                                                      "MD")
splitEduAccuracyDF[nrow(splitEduAccuracyDF) + 1,] <- list("interactive",
                                                      length(which(resultsDF$visType=='interactive' &
                                                                     resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE  & 
                                                                     resultsDF$education=='PhD')) / length(which(resultsDF$visType=='interactive' &
                                                                                                                        resultsDF$education=='PhD')),
                                                      "PhD")
splitEduAccuracyDF[nrow(splitEduAccuracyDF) + 1,] <- list("static_interactive",
                                                      length(which(resultsDF$visType=='static_interactive' &
                                                                     resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE  & 
                                                                     resultsDF$education=='High School')) / length(which(resultsDF$visType=='static_interactive' &
                                                                                                                                resultsDF$education=='High School')),
                                                      "High School")
splitEduAccuracyDF[nrow(splitEduAccuracyDF) + 1,] <- list("static_interactive",
                                                      length(which(resultsDF$visType=='static_interactive' &
                                                                     resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE  & 
                                                                     resultsDF$education=='2 Year College')) / length(which(resultsDF$visType=='static_interactive' &
                                                                                                                                   resultsDF$education=='2 Year College')),
                                                      "2 Year College")
splitEduAccuracyDF[nrow(splitEduAccuracyDF) + 1,] <- list("static_interactive",
                                                      length(which(resultsDF$visType=='static_interactive' &
                                                                     resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE  & 
                                                                     resultsDF$education=='4 Year College')) / length(which(resultsDF$visType=='static_interactive' &
                                                                                                                                   resultsDF$education=='4 Year College')),
                                                      "4 Year College")
splitEduAccuracyDF[nrow(splitEduAccuracyDF) + 1,] <- list("static_interactive",
                                                      length(which(resultsDF$visType=='static_interactive' &
                                                                     resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE  & 
                                                                     resultsDF$education=='Masters')) / length(which(resultsDF$visType=='static_interactive' &
                                                                                                                            resultsDF$education=='Masters')),
                                                      "Masters")
splitEduAccuracyDF[nrow(splitEduAccuracyDF) + 1,] <- list("static_interactive",
                                                      length(which(resultsDF$visType=='static_interactive' &
                                                                     resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE  & 
                                                                     resultsDF$education=='MD')) / length(which(resultsDF$visType=='static_interactive' &
                                                                                                                       resultsDF$education=='MD')),
                                                      "MD")
splitEduAccuracyDF[nrow(splitEduAccuracyDF) + 1,] <- list("static_interactive",
                                                      length(which(resultsDF$visType=='static_interactive' &
                                                                     resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE  & 
                                                                     resultsDF$education=='PhD')) / length(which(resultsDF$visType=='static_interactive' &
                                                                                                                        resultsDF$education=='PhD')),
                                                      "PhD")
splitEduAccuracyDF[nrow(splitEduAccuracyDF) + 1,] <- list("text",
                                                      length(which(resultsDF$visType=='text' &
                                                                     resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE  & 
                                                                     resultsDF$education=='High School')) / length(which(resultsDF$visType=='text' &
                                                                                                                                resultsDF$education=='High School')),
                                                      "High School")
splitEduAccuracyDF[nrow(splitEduAccuracyDF) + 1,] <- list("text",
                                                      length(which(resultsDF$visType=='text' &
                                                                     resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE  & 
                                                                     resultsDF$education=='2 Year College')) / length(which(resultsDF$visType=='text' &
                                                                                                                                   resultsDF$education=='2 Year College')),
                                                      "2 Year College")
splitEduAccuracyDF[nrow(splitEduAccuracyDF) + 1,] <- list("text",
                                                      length(which(resultsDF$visType=='text' &
                                                                     resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE  & 
                                                                     resultsDF$education=='4 Year College')) / length(which(resultsDF$visType=='text' &
                                                                                                                                   resultsDF$education=='4 Year College')),
                                                      "4 Year College")
splitEduAccuracyDF[nrow(splitEduAccuracyDF) + 1,] <- list("text",
                                                      length(which(resultsDF$visType=='text' &
                                                                     resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE  & 
                                                                     resultsDF$education=='Masters')) / length(which(resultsDF$visType=='text' &
                                                                                                                            resultsDF$education=='Masters')),
                                                      "Masters")
splitEduAccuracyDF[nrow(splitEduAccuracyDF) + 1,] <- list("text",
                                                      length(which(resultsDF$visType=='text' &
                                                                     resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE  & 
                                                                     resultsDF$education=='MD')) / length(which(resultsDF$visType=='text' &
                                                                                                                       resultsDF$education=='MD')),
                                                      "MD")
splitEduAccuracyDF[nrow(splitEduAccuracyDF) + 1,] <- list("text",
                                                      length(which(resultsDF$visType=='text' &
                                                                     resultsDF$guessACorrect==TRUE & resultsDF$guessBCorrect==TRUE  & 
                                                                     resultsDF$education=='PhD')) / length(which(resultsDF$visType=='text' &
                                                                                                                        resultsDF$education=='PhD')),
                                                      "PhD")

ggplot(data = splitEduAccuracyDF, 
       aes(x = visType, y = accuracy, fill = education)) + 
  geom_bar(stat = 'identity', width = 0.4, position = position_dodge(), color = 'darkblue') +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Both guesses correct accuracy (by Education)")


#######################################################################################
# the following is for the subset of respondents who got both guesses correctly
# also misc work
#######################################################################################
# demographic data statistics
demo2DF <- correctOrNotDF[c("age","gender","experience","education")]
demo2DF$gender <- as.factor(demo2DF$gender)
demo2DF$experience <- as.factor(demo2DF$experience)
demo2DF$education <- as.factor(demo2DF$education)
summary(demo2DF)
# more specific demographics
skilled2Demo <- demo2DF[demo2DF$experience == "High",]
medium2Demo <- demo2DF[demo2DF$experience == "Medium",]
low2Demo <- demo2DF[demo2DF$experience == "Low",]
# basic plot
ggplot(data = correctOrNotDF, aes(x = visType, fill = factor(guessACorrect, levels = c("FALSE","TRUE")))) +
  geom_bar(position = 'fill', width = 0.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  # scale_y_continuous(breaks = c(5,10,15,20,25), labels = scales::identity_pal()) +
  scale_fill_manual("Both guesses were correct?", values = c("TRUE" = "green3", "FALSE" = "red2")) +
  labs(title = "Both guesses were correct Y/N plot")
# chi square test of independence
stat2DF <- correctOrNotDF[c("visType","guessACorrect","guessBCorrect")]
chisq.test(stat2DF$visType, stat2DF$guessBCorrect, correct = FALSE)
chisq.test(stat2DF$guessACorrect, stat2DF$guessBCorrect, correct = FALSE)
# pairwise chi square
staticTextPairDF <- stat2DF[stat2DF$visType != "interactive",]
interactTextPairDF <- stat2DF[stat2DF$visType != "static_interactive",]
staticInteractivePairDF <- stat2DF[stat2DF$visType != "text",]
chisq.test(staticTextPairDF$visType, staticTextPairDF$guessACorrect, correct = FALSE)
chisq.test(interactTextPairDF$visType, interactTextPairDF$guessACorrect, correct = FALSE)
chisq.test(staticInteractivePairDF$visType, staticInteractivePairDF$guessACorrect, correct = FALSE)
# getting accuracy statistics
accuracy2DF <- data.frame(matrix(nrow = 0, ncol = 2))
x <- c("visType", "accuracy")
colnames(accuracy2DF) <- x
accuracy2DF[nrow(accuracy2DF) + 1,] <- list("interactive",
                                          length(which(correctOrNotDF$visType=='interactive' & correctOrNotDF$guessBCorrect==TRUE)) / length(which(correctOrNotDF$visType=='interactive')))
accuracy2DF[nrow(accuracy2DF) + 1,] <- list("static_interactive",
                                          length(which(correctOrNotDF$visType=='static_interactive' & correctOrNotDF$guessBCorrect==TRUE)) / length(which(correctOrNotDF$visType=='static_interactive')))
accuracy2DF[nrow(accuracy2DF) + 1,] <- list("text",
                                          length(which(correctOrNotDF$visType=='text' & correctOrNotDF$guessBCorrect==TRUE)) / length(which(correctOrNotDF$visType=='text')))
ggplot(data = accuracy2DF, aes(x = visType, y = accuracy)) +
  geom_bar(stat = 'identity', width = 0.3, fill = 'lightskyblue', color = 'darkblue') +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) +
  labs(title = "Both guesses correct accuracy statistics plot") +
  theme_bw()
# splitting accuracy by Statistics Experience Demographics
splitAccuracy2DF <- data.frame(matrix(nrow = 0, ncol = 3))
x <- c("visType", "accuracy", "experience")
colnames(splitAccuracy2DF) <- x
splitAccuracy2DF[nrow(splitAccuracy2DF) + 1,] <- list("interactive",
                                                    length(which(correctOrNotDF$visType=='interactive' &
                                                                   correctOrNotDF$guessBCorrect==TRUE & 
                                                                   correctOrNotDF$experience=='High')) / length(which(correctOrNotDF$visType=='interactive' &
                                                                                                                   correctOrNotDF$experience=='High')),
                                                    "High")
splitAccuracy2DF[nrow(splitAccuracy2DF) + 1,] <- list("interactive",
                                                    length(which(correctOrNotDF$visType=='interactive' &
                                                                   correctOrNotDF$guessBCorrect==TRUE & 
                                                                   correctOrNotDF$experience=='Medium')) / length(which(correctOrNotDF$visType=='interactive' &
                                                                                                                     correctOrNotDF$experience=='Medium')),
                                                    "Medium")
splitAccuracy2DF[nrow(splitAccuracy2DF) + 1,] <- list("interactive",
                                                    length(which(correctOrNotDF$visType=='interactive' &
                                                                   correctOrNotDF$guessBCorrect==TRUE & 
                                                                   correctOrNotDF$experience=='Low')) / length(which(correctOrNotDF$visType=='interactive' &
                                                                                                                  correctOrNotDF$experience=='Low')),
                                                    "Low")
splitAccuracy2DF[nrow(splitAccuracy2DF) + 1,] <- list("static_interactive",
                                                    length(which(correctOrNotDF$visType=='static_interactive' &
                                                                   correctOrNotDF$guessBCorrect==TRUE & 
                                                                   correctOrNotDF$experience=='High')) / length(which(correctOrNotDF$visType=='static_interactive' &
                                                                                                                   correctOrNotDF$experience=='High')),
                                                    "High")
splitAccuracy2DF[nrow(splitAccuracy2DF) + 1,] <- list("static_interactive",
                                                    length(which(correctOrNotDF$visType=='static_interactive' &
                                                                   correctOrNotDF$guessBCorrect==TRUE & 
                                                                   correctOrNotDF$experience=='Medium')) / length(which(correctOrNotDF$visType=='static_interactive' &
                                                                                                                     correctOrNotDF$experience=='Medium')),
                                                    "Medium")
splitAccuracy2DF[nrow(splitAccuracy2DF) + 1,] <- list("static_interactive",
                                                    length(which(correctOrNotDF$visType=='static_interactive' &
                                                                   correctOrNotDF$guessBCorrect==TRUE & 
                                                                   correctOrNotDF$experience=='Low')) / length(which(correctOrNotDF$visType=='static_interactive' &
                                                                                                                  correctOrNotDF$experience=='Low')),
                                                    "Low")
splitAccuracy2DF[nrow(splitAccuracy2DF) + 1,] <- list("text",
                                                    length(which(correctOrNotDF$visType=='text' &
                                                                   correctOrNotDF$guessBCorrect==TRUE & 
                                                                   correctOrNotDF$experience=='High')) / length(which(correctOrNotDF$visType=='text' &
                                                                                                                   correctOrNotDF$experience=='High')),
                                                    "High")
splitAccuracy2DF[nrow(splitAccuracy2DF) + 1,] <- list("text",
                                                    length(which(correctOrNotDF$visType=='text' &
                                                                   correctOrNotDF$guessBCorrect==TRUE & 
                                                                   correctOrNotDF$experience=='Medium')) / length(which(correctOrNotDF$visType=='text' &
                                                                                                                     correctOrNotDF$experience=='Medium')),
                                                    "Medium")
splitAccuracy2DF[nrow(splitAccuracy2DF) + 1,] <- list("text",
                                                    length(which(correctOrNotDF$visType=='text' &
                                                                   correctOrNotDF$guessBCorrect==TRUE & 
                                                                   correctOrNotDF$experience=='Low')) / length(which(correctOrNotDF$visType=='text' &
                                                                                                                  correctOrNotDF$experience=='Low')),
                                                    "Low")
ggplot(data = splitAccuracy2DF, 
       aes(x = visType, y = accuracy, fill = experience)) + 
  geom_bar(stat = 'identity', width = 0.4, position = position_dodge(), color = 'darkblue') +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Both guesses correct accuracy (by Statistics Experience)")
# splitting accuracy by education
splitAccuracy3DF <- data.frame(matrix(nrow = 0, ncol = 3))
x <- c("visType", "accuracy", "education")
colnames(splitAccuracy3DF) <- x
splitAccuracy3DF[nrow(splitAccuracy3DF) + 1,] <- list("interactive",
                                                      length(which(correctOrNotDF$visType=='interactive' &
                                                                     correctOrNotDF$guessBCorrect==TRUE & 
                                                                     correctOrNotDF$education=='High School')) / length(which(correctOrNotDF$visType=='interactive' &
                                                                                                                              correctOrNotDF$education=='High School')),
                                                      "High School")
splitAccuracy3DF[nrow(splitAccuracy3DF) + 1,] <- list("interactive",
                                                      length(which(correctOrNotDF$visType=='interactive' &
                                                                     correctOrNotDF$guessBCorrect==TRUE & 
                                                                     correctOrNotDF$education=='2 Year College')) / length(which(correctOrNotDF$visType=='interactive' &
                                                                                                                            correctOrNotDF$education=='2 Year College')),
                                                      "2 Year College")
splitAccuracy3DF[nrow(splitAccuracy3DF) + 1,] <- list("interactive",
                                                      length(which(correctOrNotDF$visType=='interactive' &
                                                                     correctOrNotDF$guessBCorrect==TRUE & 
                                                                     correctOrNotDF$education=='4 Year College')) / length(which(correctOrNotDF$visType=='interactive' &
                                                                                                                         correctOrNotDF$education=='4 Year College')),
                                                      "4 Year College")
splitAccuracy3DF[nrow(splitAccuracy3DF) + 1,] <- list("interactive",
                                                      length(which(correctOrNotDF$visType=='interactive' &
                                                                     correctOrNotDF$guessBCorrect==TRUE & 
                                                                     correctOrNotDF$education=='Masters')) / length(which(correctOrNotDF$visType=='interactive' &
                                                                                                                                   correctOrNotDF$education=='Masters')),
                                                      "Masters")
splitAccuracy3DF[nrow(splitAccuracy3DF) + 1,] <- list("interactive",
                                                      length(which(correctOrNotDF$visType=='interactive' &
                                                                     correctOrNotDF$guessBCorrect==TRUE & 
                                                                     correctOrNotDF$education=='MD')) / length(which(correctOrNotDF$visType=='interactive' &
                                                                                                                                   correctOrNotDF$education=='MD')),
                                                      "MD")
splitAccuracy3DF[nrow(splitAccuracy3DF) + 1,] <- list("interactive",
                                                      length(which(correctOrNotDF$visType=='interactive' &
                                                                     correctOrNotDF$guessBCorrect==TRUE & 
                                                                     correctOrNotDF$education=='PhD')) / length(which(correctOrNotDF$visType=='interactive' &
                                                                                                                       correctOrNotDF$education=='PhD')),
                                                      "PhD")
splitAccuracy3DF[nrow(splitAccuracy3DF) + 1,] <- list("static_interactive",
                                                      length(which(correctOrNotDF$visType=='static_interactive' &
                                                                     correctOrNotDF$guessBCorrect==TRUE & 
                                                                     correctOrNotDF$education=='High School')) / length(which(correctOrNotDF$visType=='static_interactive' &
                                                                                                                                correctOrNotDF$education=='High School')),
                                                      "High School")
splitAccuracy3DF[nrow(splitAccuracy3DF) + 1,] <- list("static_interactive",
                                                      length(which(correctOrNotDF$visType=='static_interactive' &
                                                                     correctOrNotDF$guessBCorrect==TRUE & 
                                                                     correctOrNotDF$education=='2 Year College')) / length(which(correctOrNotDF$visType=='static_interactive' &
                                                                                                                                   correctOrNotDF$education=='2 Year College')),
                                                      "2 Year College")
splitAccuracy3DF[nrow(splitAccuracy3DF) + 1,] <- list("static_interactive",
                                                      length(which(correctOrNotDF$visType=='static_interactive' &
                                                                     correctOrNotDF$guessBCorrect==TRUE & 
                                                                     correctOrNotDF$education=='4 Year College')) / length(which(correctOrNotDF$visType=='static_interactive' &
                                                                                                                                   correctOrNotDF$education=='4 Year College')),
                                                      "4 Year College")
splitAccuracy3DF[nrow(splitAccuracy3DF) + 1,] <- list("static_interactive",
                                                      length(which(correctOrNotDF$visType=='static_interactive' &
                                                                     correctOrNotDF$guessBCorrect==TRUE & 
                                                                     correctOrNotDF$education=='Masters')) / length(which(correctOrNotDF$visType=='static_interactive' &
                                                                                                                            correctOrNotDF$education=='Masters')),
                                                      "Masters")
splitAccuracy3DF[nrow(splitAccuracy3DF) + 1,] <- list("static_interactive",
                                                      length(which(correctOrNotDF$visType=='static_interactive' &
                                                                     correctOrNotDF$guessBCorrect==TRUE & 
                                                                     correctOrNotDF$education=='MD')) / length(which(correctOrNotDF$visType=='static_interactive' &
                                                                                                                       correctOrNotDF$education=='MD')),
                                                      "MD")
splitAccuracy3DF[nrow(splitAccuracy3DF) + 1,] <- list("static_interactive",
                                                      length(which(correctOrNotDF$visType=='static_interactive' &
                                                                     correctOrNotDF$guessBCorrect==TRUE & 
                                                                     correctOrNotDF$education=='PhD')) / length(which(correctOrNotDF$visType=='static_interactive' &
                                                                                                                        correctOrNotDF$education=='PhD')),
                                                      "PhD")
splitAccuracy3DF[nrow(splitAccuracy3DF) + 1,] <- list("text",
                                                      length(which(correctOrNotDF$visType=='text' &
                                                                     correctOrNotDF$guessBCorrect==TRUE & 
                                                                     correctOrNotDF$education=='High School')) / length(which(correctOrNotDF$visType=='text' &
                                                                                                                                correctOrNotDF$education=='High School')),
                                                      "High School")
splitAccuracy3DF[nrow(splitAccuracy3DF) + 1,] <- list("text",
                                                      length(which(correctOrNotDF$visType=='text' &
                                                                     correctOrNotDF$guessBCorrect==TRUE & 
                                                                     correctOrNotDF$education=='2 Year College')) / length(which(correctOrNotDF$visType=='text' &
                                                                                                                                   correctOrNotDF$education=='2 Year College')),
                                                      "2 Year College")
splitAccuracy3DF[nrow(splitAccuracy3DF) + 1,] <- list("text",
                                                      length(which(correctOrNotDF$visType=='text' &
                                                                     correctOrNotDF$guessBCorrect==TRUE & 
                                                                     correctOrNotDF$education=='4 Year College')) / length(which(correctOrNotDF$visType=='text' &
                                                                                                                                   correctOrNotDF$education=='4 Year College')),
                                                      "4 Year College")
splitAccuracy3DF[nrow(splitAccuracy3DF) + 1,] <- list("text",
                                                      length(which(correctOrNotDF$visType=='text' &
                                                                     correctOrNotDF$guessBCorrect==TRUE & 
                                                                     correctOrNotDF$education=='Masters')) / length(which(correctOrNotDF$visType=='text' &
                                                                                                                            correctOrNotDF$education=='Masters')),
                                                      "Masters")
splitAccuracy3DF[nrow(splitAccuracy3DF) + 1,] <- list("text",
                                                      length(which(correctOrNotDF$visType=='text' &
                                                                     correctOrNotDF$guessBCorrect==TRUE & 
                                                                     correctOrNotDF$education=='MD')) / length(which(correctOrNotDF$visType=='text' &
                                                                                                                       correctOrNotDF$education=='MD')),
                                                      "MD")
splitAccuracy3DF[nrow(splitAccuracy3DF) + 1,] <- list("text",
                                                      length(which(correctOrNotDF$visType=='text' &
                                                                     correctOrNotDF$guessBCorrect==TRUE & 
                                                                     correctOrNotDF$education=='PhD')) / length(which(correctOrNotDF$visType=='text' &
                                                                                                                        correctOrNotDF$education=='PhD')),
                                                      "PhD")
png('splitAccuracy-edu.png', units="in", width=5, height=4, res=300)
ggplot(data = splitAccuracy3DF, 
       aes(x = visType, y = accuracy, fill = education)) + 
  geom_bar(stat = 'identity', width = 0.5, position = position_dodge(), color = 'darkblue') +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Both guesses correct accuracy (by Education)")
dev.off()
