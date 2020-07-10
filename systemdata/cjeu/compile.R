Decisions <- Decisions[,c("case", "celex", "ecli", "court", "type", "subject_matter", "date_document", "date_lodged",
"subject_matter_subcategory", "preliminary_ruling", "direct_action", "applicant_actor", "defendant_actor")] 

save(Decisions, file="Decisions.rda")

Cases <- Cases[,c("case", "celex", "court", "status", "lodged_date", "title", "joined_cases", "appeal_of")]
save(Cases, file="Cases.rda")
