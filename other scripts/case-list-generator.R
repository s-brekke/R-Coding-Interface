# Select cases ####
library(CJEU)

subject <- "Free movement of goods"
coders <- c("irene", "lucia", "eun_hye")

overlap <- 100

eclis <- Judgments$ecli[which((grepl(subject, Judgments$subject_matter) | grepl(subject, Judgments$subject_matter_subcategory)) & !Judgments$duplicate)]
eclis <- eclis[grep(":C:", eclis)]

eclis_fresh <- eclis

load("systemdata/data_master.rda")
eclis_fresh <- eclis_fresh[which(!eclis_fresh %in% data_master$ecli)]

load("pilotcases.rda")
eclis_fresh <- eclis_fresh[which(!eclis_fresh %in% pilotcases$ecli)]
delegated_cases <- pilotcases


n_cases <- c(4,3,3)
n_cases <- round(((length(eclis_fresh)+100)/(4+3+3))*n_cases)

for(n in 1:length(coders)){
  tocode <- sample(eclis_fresh, n_cases[n])
  
  eclis_fresh <- eclis_fresh[which(!eclis_fresh %in% delegated_cases$ecli)]
  potential_crosstests <- delegated_cases[which(!delegated_cases$ecli %in% delegated_cases$ecli[which(duplicated(delegated_cases$ecli))]),]
  
  if(n > 1){
    crosscode <- round(overlap/(length(coders)-1))
    tocode <- c(sample(eclis_fresh, (as.numeric(n_cases[n])-crosscode)),
                sample(potential_crosstests$ecli[which(potential_crosstests$user!=coders[n])], crosscode))
  } else {
    tocode <- sample(eclis_fresh, n_cases[n])
  }
  
  delegated_cases <- rbind(delegated_cases,
        data.frame(ecli=cjeuSortCases(tocode),
             user=coders[n]))
}
save(delegated_cases, file="systemdata/delegated_cases.rda")
