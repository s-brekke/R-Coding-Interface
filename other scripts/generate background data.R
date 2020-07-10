# Create data set for coding project 

# Add URLS
# blankdata <- read.csv("~/Dropbox/Dokumenter/Coding project/systemdata/data_table_blank.csv", stringsAsFactors=FALSE)
# colnames(blankdata) <- tolower(colnames(blankdata))
# 
# colnames(blankdata) <- gsub("link.to.corpus", "url", colnames(blankdata))
# blankdata$url
# blankdata$case.c
documents <- lapply(Cases$documents[match(unique(Decisions$case), Cases$case)], as.data.frame)
documents <- documents[which(unlist(lapply(documents, ncol)) == 7)]
documents <- lapply(documents, function(y) y[which(!is.na(y$ecli)),])

ecli <- unlist(lapply(documents, function(y) as.character(y[,1])))
url <- unlist(lapply(documents, function(y) as.character(y[,6])))

hyperlinks <- data.frame(ecli=ecli,
                         url=url)
save(hyperlinks, file="hyperlinks.rda")
# library(splitstackshape)
# Outcome data set
# outcomes <- read.csv("~/Dropbox/Dokumenter/Coding project/outcomes.csv", sep=";")
# outcomes$outcome <- gsub("(.*?)\\s?=\\s?", "\\1;", outcomes$outcome)
# 
# outcomes <- cSplit(outcomes, "outcome", ";")
# outcomes$outcome_2 <- gsub("(^.*?)\\s", "\\1;", outcomes$outcome_2)
# outcomes <- cSplit(outcomes, "outcome_2", ";")
# outcomes <- outcomes[which(!grepl("[[:lower:]]", outcomes$outcome_1)),]
# colnames(outcomes) <- c("variable", "value", "interpretation")
# outcomes$description <- outcomes$interpretation
# write.csv(outcomes, file="outcomes.csv")
# names_codes <- read.csv("~/Dropbox/Dokumenter/Coding project/names_codes.csv", sep=";")
# 
# outcomes$variable_name <- tolower(names_codes$VARIABLE[match(outcomes$variable, names_codes$LABEL)])
# outcomes <- outcomes[,c(1, 5, 2:4)]
# write.csv(outcomes, file="outcomes.csv")