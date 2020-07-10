# SQL testing 

con <- dbConnect(SQLite(), "sql/cjeu.db")

rs <- dbSendStatement(con, paste("update goods set ", variable, " = '", value, "' where ecli = '", ecli, "'", sep=""))
dbHasCompleted(rs)

dbGetQuery(con, "SELECT FREED FROM goods WHERE ecli_alt = 'ECLI_EU_C_1968_22' and coded_by = 'urska'")




newrow <- outputdata[0,]
newrow[1, "ecli"] <- "test"
newrow[1, "ecli_alt"] <- gsub("\\W", "_", newrow$ecli)
newrow[1, "coded_by"] <- as.character(input$user)
paste(colnames(newrow), collapse=",")
paste(colnames(newrow), collapse=",")

dbSendStatement(con, "INSERT INTO goods (ecli, ecli_alt)
VALUES ('hello world', 'hello_world')")


# Insert new row if missing
ecli <- ecli()
if(nrow(dbGetQuery(con, "SELECT * FROM goods WHERE ecli = ? AND coded_by = ?", list(ecli, as.character(input$user)))) == 0){
  dbExecute(con, 'INSERT INTO goods (ecli, coded_by) VALUES (?, ?);', params=list(ecli, as.character(input$user)))
}


value <- eval(parse(text=paste("paste(input$", r, ", collapse=\",\")", sep="")))
not_mandatory <- c("TYPEDEF", "TYPEDEFOP", "DEFEX2")
if(r %in% not_mandatory & value == "0"){
  value <- "-888"
}
dbExecute(con, paste("UPDATE goods SET", r, "= ? WHERE ecli = ? AND coded_by = ?"), list(value, ecli, as.character(input$user)))

dbGetQuery(con, paste("SELECT", r, "FROM goods WHERE ecli = ? AND coded_by = ?"), list(ecli, as.character(input$user)))



connect2Sql <- function(){
  dbConnect(MariaDB(), host = "mysql16.unoeuro.com", 
            dbname = "courtdata_se_db_cjeu", 
            username = "courtdata_se", 
            password = "Bondeska")

}
