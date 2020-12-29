#                 _____          _ _                 _       _             __               
#                / ____|        | (_)               (_)     | |           / _|              
#               | |     ___   __| |_ _ __   __ _     _ _ __ | |_ ___ _ __| |_ __ _  ___ ___ 
#               | |    / _ \ / _` | | '_ \ / _` |   | | '_ \| __/ _ \ '__|  _/ _` |/ __/ _ \
#               | |___| (_) | (_| | | | | | (_| |   | | | | | ||  __/ |  | || (_| | (_|  __/
#                \_____\___/ \__,_|_|_| |_|\__, |   |_|_| |_|\__\___|_|  |_| \__,_|\___\___|
#                                           __/ |                                         
#                        EUI, 2020         |___/          Written by Stein Arne Brekke
#

library(shiny)
library(DBI)

# # Load config files
# # Read config files if excel
if("readxl" %in% rownames(installed.packages())){
  library(readxl)
  if("delegation.xlsx" %in% list.files("systemdata")){
    delegated_cases <- read_excel(file.path("systemdata","delegation.xlsx"))
    write.csv(delegated_cases, file.path("systemdata", "delegation.csv"), row.names = FALSE)
  }
  if("variable_list.xlsx" %in% list.files("systemdata")){
    variable_list <- read_excel(file.path("systemdata","variable_list.xlsx"))
    write.csv(variable_list, file.path("systemdata", "variable_list.csv"), row.names = FALSE)
  }
  if("users.xlsx" %in% list.files("systemdata")){
    users <- read_excel(file.path("systemdata","users.xlsx"))
    write.csv(users, file.path("systemdata", "users.csv"), row.names = FALSE)
  }
  if("config.xlsx" %in% list.files("systemdata")){
    config <- read_excel(file.path("systemdata","config.xlsx"))
    write.csv(config, file.path("systemdata", "config.csv"), row.names = FALSE)
  }
}

# To avoid error before functions load 
user <- function(){return(NA)}
ID <- function(){return(NA)}


# Todo: Make this dependant on the files existing, bug test
variable_list <- read.csv(file.path("systemdata", "variable_list.csv"),  stringsAsFactors=FALSE)
users <- read.csv(file.path("systemdata", "users.csv"), stringsAsFactors = FALSE); rownames(users) <- users$username
delegated_cases <- read.csv(file.path("systemdata", "delegation.csv"), stringsAsFactors = FALSE) 
delegated_cases <- delegated_cases[which(!is.na(delegated_cases$ID)),]
config <- read.csv(file.path("systemdata", "config.csv"), stringsAsFactors = FALSE)


rownames(config) <- config$setting
config <- as.data.frame(t(config))[2,]

sql_type <- tolower(config$sql_type)
data_base_name <- as.character(config$data_base_name)
data_set_name <- as.character(config$data_set_name)
showguide <- as.logical(config$showguide)

startcols <- c("ID", "coded_by", "date_updated", "completed")

variable_list <- variable_list[which(!grepl("invisible value", variable_list$interpretation)),]

columns <- unique(variable_list$variable)
text_columns <- variable_list$variable[which(paste(variable_list$text) == "yes")]

if(sql_type == "mysql"){
  library(RMariaDB)
  
  username <- as.character(config$username)
  host <- as.character(config$host)
  password <- as.character(config$password)
  
  # mariaDB or MySQL: Connect to server ####
  dbname <- data_base_name
  con <- dbConnect(MariaDB(), host = host,
                   dbname = dbname,
                   username = username,
                   password = password)
  outputdata <- dbGetQuery(con, paste0("SELECT * FROM ", data_set_name))
} else {
  library(RSQLite)
  con <- dbConnect(SQLite(), paste0(data_base_name, ".db"))
  if(!data_set_name %in% unlist(dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type = 'table'"))){
    cols <- c(startcols, columns)
    table <- as.data.frame(t(matrix(cols)))[0,]
    colnames(table) <- cols
    dbWriteTable(con, data_set_name, table)
  }
  outputdata <- dbGetQuery(con, paste0("SELECT * FROM ", data_set_name))
}

# Add new variable(s) 
if(length(columns[which(!columns %in% colnames(outputdata))]) > 0){
  for(c in columns[which(!columns %in% colnames(outputdata))]){
    dbExecute(con, paste0("ALTER TABLE ", data_set_name, " ADD COLUMN ",
                          paste0("`", c, "`", "TEXT")))
  }
  outputdata <- dbGetQuery(con, paste0("SELECT * FROM ", data_set_name))
}

# Text variables:
if(length(text_columns[which(!paste0(text_columns, "_TEXT") %in% colnames(outputdata))]) > 0){
  for(c in text_columns[which(!paste0(text_columns, "_TEXT") %in% colnames(outputdata))]){
    dbExecute(con, paste0("ALTER TABLE ", data_set_name, " ADD COLUMN ",
                          paste0("`", paste0(c, "_TEXT"), "`", " TEXT")
                          # # Paragraph and check box variables: (optional)
                          # paste0("`", paste0(c, "_p1"), "`", " SMALLINT(4), "),
                          # paste0("`", paste0(c, "_p2"), "`", " SMALLINT(4), "),
                          # paste0("`", paste0(c, "_OP"), "`", " BOOL")
                          ))
  }
  outputdata <- dbGetQuery(con, paste0("SELECT * FROM ", data_set_name))
}


variable_list <- variable_list[which(!grepl("invisible value", variable_list$interpretation)),]



# variables <- unique(variable_list$variable)
# 
# justground <- variable_list[0,]
# justground[1,"variable"] <- "JUST_GROUND.IF.OTHER"
# justground[1,"variable_name"] <- "Justification of grounds if other"
# justground[1,"description"] <- "Justification of grounds (if other)"
# 
# justground[2,"variable"] <- "NOTE"
# justground[2,"variable_name"] <- "Note on coding (if needed)"
# justground[2,"description"] <- "Anything to add after coding?"
# justground$value <- "text"
# 
# variable_list <- rbind(variable_list, justground)

fleeting_data_line <- outputdata[1,] 

last_write <- Sys.time()

# # CJEU data
# load(file.path("systemdata", "cjeu", "hyperlinks.rda"))
# # if(!"CJEU" %in% rownames(installed.packages())){
# load(file.path("systemdata", "cjeu", "Decisions.rda"))
# load(file.path("systemdata", "cjeu", "Cases.rda"))
# # }
# Decisions$title <- Cases$title[match(Decisions$case, Cases$case)] # Shouldn't be needed for much longer 
# data <- Decisions

IDs_to_code <- delegated_cases$ID
users_who_code <- delegated_cases$user



paragraph_survey <- function(n,
                             p1_val = NULL,
                             p2_val = NULL,
                             op_val = NULL){
  tags$div(
    tags$div(numericInput(paste(radios[n], "_p1", sep=""),
                          "Key paragraphs:", "",
                          value = p1_val,
                          width="120"),
             style="display:inline-block"),
    
    tags$div("to", style="display:inline-block"),
    
    tags$div(numericInput(paste(radios[n], "_p2", sep=""),
                          "",
                          value=p2_val,
                          width="120"),
             style="display:inline-block"),
    tags$div(checkboxInput(paste(radios[n], "_OP", sep=""), "in operative part",
                           value = op_val), style="display:inline-block"),
  )
}

radio_survey <- function(n,
                         select = "NA",
                         p1_val = NULL,
                         p2_val = NULL,
                         op_val = FALSE,
                         text_val=NULL){
  tags$div(
    if(is.na(op_val)){
      op_val <- FALSE
      NULL
    },
    if(!is.na(variable_list$headline[which(variable_list$variable == vars[n])][1])){
      tags$a(tags$h3(variable_list$headline[which(variable_list$variable == vars[n])][1]), 
             name=gsub("\\W", "_", variable_list$headline[which(variable_list$variable == vars[n])][1]),
             style="all:unset")
    },
    
    if(showguide & !is.na(variable_list$guide[which(variable_list$variable== vars[n])][1])){
      tags$p(
        tags$b(paste("Code guide for ", variable_list$variable_name[which(variable_list$variable== vars[n])][1], ": ", sep="")),  tags$br(),
        tags$i(variable_list$guide[which(variable_list$variable== vars[n])][1])
      )
    },
    
    if(TRUE %in% grepl("pre-filled", variable_list$interpretation[which(variable_list$variable == vars[n])])){
      NA_name <- NULL
      NA_value <- NULL
    } else {
      NA_name <- "Not coded"
      NA_value <- "NA"
      NULL
    },
    if(variable_list$value[which(variable_list$variable == vars[n])[1]] == "text"){
      if(grepl("short", variable_list$interpretation[which(variable_list$variable == vars[n])[1]])){
        textInput(vars[n], 
                  variable_list$variable_name[which(variable_list$variable== vars[n])][1],
                  placeholder = variable_list$description[which(variable_list$variable== vars[n])][1])
      } else {
        textAreaInput(vars[n], 
                      variable_list$variable_name[which(variable_list$variable== vars[n])][1],
                      placeholder = variable_list$description[which(variable_list$variable== vars[n])][1])
      }
    } else {
      
      if(grepl("multichoice", variable_list$interpretation[which(variable_list$variable == vars[n])[1]])){
        checkboxGroupInput(vars[n],
                           variable_list$variable_name[which(variable_list$variable == vars[n])][1],
                           selected=unlist(strsplit(select, ",")),
                           choiceNames = c(variable_list$description[which(variable_list$variable == vars[n])]),
                           choiceValues = c(variable_list$value[which(variable_list$variable == vars[n])]))
      } else {
        radioButtons(vars[n],
                     variable_list$variable_name[which(variable_list$variable == vars[n])][1],
                     selected=select,
                     choiceNames = c(variable_list$description[which(variable_list$variable == vars[n])], NA_name),
                     choiceValues = c(variable_list$value[which(variable_list$variable == vars[n])], NA_value))
      }
    },
    
    if(paste(vars[n], "_p1", sep="") %in% colnames(outputdata)){
      paragraph_survey(n, p1_val, p2_val, op_val)
    },
    if(paste(vars[n], "_TEXT", sep="") %in% colnames(outputdata)){
      textInput(paste(vars[n], "_TEXT", sep=""),
                "Comment:",
                value=text_val,
                placeholder = fillertext)
    },
    tags$hr()
  )
}

update_all <- function(session,
                       ID,
                       user){
  
  message("is.na(ID)")
  message("is.na(user)")
  if(!is.na(ID) & !is.na(user)){
    
    updateTextInput(session, "ID", value=ID)
    
    
    # Insert new row if missing
    if(nrow(dbGetQuery(con, paste0("SELECT * FROM ", data_set_name, " WHERE ID = '", ID, "' AND coded_by = '", user, "'"))) == 0){
      dbExecute(con, paste0("INSERT INTO ", data_set_name, " (ID, coded_by) VALUES (?,?)"),
                list(ID, user))
    }
    outputdata <- dbGetQuery(con, paste0("SELECT * FROM ",data_set_name, " WHERE `ID` = '", ID,  "' AND `coded_by` = '", as.character(user), "'"))
    
    
    for(t in text_variables){
      message(t)
      isolate({
        updateTextAreaInput(session, 
                            t,
                            value=outputdata[, t])
      })
    }
    for(radio in radios){
      message(radio)
      if(TRUE %in% grepl("pre-filled", variable_list$interpretation[which(variable_list$variable == radio)])){
        default_channel <- variable_list$value[which(variable_list$variable == radio & grepl("pre-filled", variable_list$interpretation))]
      } else {
        default_channel <- "NA"
      }
      channel <- paste(outputdata[,radio])
      
      if(grepl("multichoice", variable_list$interpretation[which(variable_list$variable == radio)[1]])){
        updateCheckboxGroupInput(session,
                                 radio,
                                 selected = unlist(strsplit(channel, ",")))
      } else {
        updateRadioButtons(session,
                           radio,
                           selected = channel)
      }
      
      if(paste(radio, "_OP", sep="") %in% colnames(outputdata)){
        operative_part <- ifelse(!ID %in% outputdata$ID,
                                 "FALSE",
                                 paste(outputdata[,paste(radio, "_OP", sep="")]) == TRUE)
        updateCheckboxInput(session,
                            paste(radio, "_OP", sep=""),
                            value=as.logical(operative_part))
      }
      
      isolate({
        # message(radio)
        
        if(paste0(radio, "_TEXT") %in% colnames(outputdata)){
          setvalue <- ifelse(is.na(outputdata[, paste(radio, "_TEXT", sep="")]),
                             "",
                             outputdata[, paste(radio, "_TEXT", sep="")])
          # message("t-", radio, ": ", setvalue)
          updateTextInput(session,
                          paste(radio, "_TEXT", sep=""),
                          value=isolate(setvalue))
        }
        
        if(paste0(radio, "_p1") %in% colnames(outputdata)){
          setvalue <- ifelse(is.na(outputdata[, paste(radio, "_p1", sep="")]),
                             "",
                             outputdata[, paste(radio, "_p1", sep="")])
          # message("p1-", radio, ": ", setvalue)
          updateNumericInput(session,
                             paste(radio, "_p1", sep=""),
                             value=isolate(setvalue))
          
          setvalue <- ifelse(is.na(outputdata[, paste(radio, "_p2", sep="")]),
                             "",
                             outputdata[, paste(radio, "_p2", sep="")])
          # message("p2-", radio, ": ", setvalue)
          updateNumericInput(session,
                             paste(radio, "_p2", sep=""),
                             value=isolate(setvalue))
          
          setvalue <- ifelse(is.na(outputdata[, paste(radio, "OP", sep="_")]),
                             "",
                             outputdata[, paste(radio, "OP", sep="_")])
          # message("p2-", radio, ": ", setvalue)
          updateCheckboxInput(session,
                              paste(radio, "_OP", sep=""),
                              value=isolate(setvalue))
        }
      })
    }
  }
  return(Sys.time())
}


fillertext <- "Paste relevant text snippet"
vars <- unique(variable_list$variable)
radios <- unique(variable_list$variable[which(variable_list$value != "text")])
text_variables <-  c(unique(variable_list$variable[which(variable_list$value == "text")]))

heads <- unique(na.omit(variable_list$headline))
headlinelinks <- paste("<a href=\"#", gsub("\\W", "_", c(heads, "End")), "\">",
                       gsub("Variables related to ([[:lower:]])", "\\U\\1", c(heads, "End"), perl=TRUE),
                       "</a><br />", sep="", collapse="")


clicktime <- Sys.time() # This is used to make sure nothing is saved faster than the software can keep up
starttime <- Sys.time() # Calculate time used
user_new <- NA



# First user interface:
ui <- fluidPage(
  # Application title
  tags$a(name="top"),
  titlePanel("Coding interface"),
  
  # Sidebar with case info
  sidebarLayout(
    sidebarPanel(
      
      textInput("ID", 
                "ID number:",
                c(outputdata$ID[which(outputdata$date_coded == max(c(na.omit(outputdata$date_coded[which(outputdata$coded_by == paste(user_new))])),0) &
                                          outputdata$coded_by == paste(user_new))], "")[1],
                placeholder = "Please sign in before starting"),
      
      tags$div(actionButton("previouscase", "< previous", width="40%"),
               
               # actionButton("randomcase", "?", width="15%"),
               actionButton("update", "^", width="15%"),
               
               actionButton("nextcase", "next >", width="40%"),
               style="display:inline-block; width:100%"),
      
      
      tags$h3(textOutput("title")),
      
      # # CJEU metadata
      # tags$i(uiOutput("url")), 
      # 
      # tags$div(tags$b("Case:"),
      #          style="display:inline-block"),
      # tags$div(textOutput("casenumber"),
      #          style="display:inline-block"),
      # tags$br(),
      # tags$div(tags$b("ECLI:"),
      #          style="display:inline-block"),
      # tags$div(textOutput("IDnumber"),
      #          style="display:inline-block"),
      # tags$br(),
      # 
      # tags$div(tags$b("CELEX:"),
      #          style="display:inline-block"),
      # tags$div(textOutput("celex"),
      #          style="display:inline-block"),
      # tags$br(),
      # 
      # tags$i(uiOutput("docinfo")), 
      # 
      # tags$b("Subject matter:"),
      # textOutput("subject_matter"),
      # 
      # tags$b("Date of lodging:"),
      # textOutput("lodged"),
      # 
      # tags$b("Date of document:"),
      # textOutput("published"),
      
      tags$b("Last updated:"),
      textOutput("last_update"),
      
      
      selectInput("user", "User:", c(users$username)),
      textOutput("username"),
      actionButton("signin", "Sign in"),
      tags$hr(),
      div(HTML(headlinelinks)),
      tags$hr(), # ONLY ONLINE
      
      # checkboxInput("notFMP", "This case is not free movement of persons"),
      # checkboxInput("secondary", HTML("This case is not <i>primarily</i> free movement of persons, and should not be coded as such")),
      actionButton("confirm", "Confirm"),
      tags$hr(),
      # actionButton("submit", "Save"),
      # actionButton("statistics", "User statistics"),
      downloadButton("downloadData", "Download data")
      
      # tags$p("For online users:"), # ONLY ONLINE
      # actionButton("submit", "Save"),
      # downloadButton("downloadData", "Download user data"), # ONLY ONLINE
      # downloadButton("downloadCSV", "Download in CSV format"), # ONLY ONLINE
      # 
    ),
    
    
    mainPanel(
      tags$div(uiOutput("completed")), 
      
      radio_survey(1),
      radio_survey(2),
      radio_survey(3),
      radio_survey(4),
      radio_survey(5),
      radio_survey(6),
      
      tags$hr(),
      
      # actionButton("submit2", "Save"), # saving is fully automated
      actionButton("timer", "Show timer"),
      tags$br(),
      a("Click here to return to the top", href="#top", name="End"),
      
    )
  )
)


# Now for the server side:
server <- function(input, output, session) {
  
  # # Collect "ID". Universal variables have to be defined as reactive functions.
  # # If CJEU document:
  # ID <- reactive(Decisions$ID[which(Decisions$ID == gsub("^\\W*|\\W*$", "", input$ID) |
  #                                         Decisions$celex == gsub("^\\W*|\\W*$", "", input$ID) |
  #                                         Decisions$case == gsub("^(\\d+/)", "C-\\1", gsub("^\\W*|\\W*$", "", input$ID)) &
  #                                         Decisions$type == "Judgment"
  # )][1])
  # # Else:
  ID <- reactive(input$ID)
  
  user <- eventReactive(input$signin, {
    input$user
  })
  
  observeEvent(input$confirm, {
    if(!is.na(ID())){
      # if(input$notFMP){
      #   x <- dbExecute(con, paste0("UPDATE policy_areas SET `FMP` = 'no', `date_of_coding` = ? WHERE `ID` = ?"),
      #                  list(as.character(Sys.Date()), ID()))
      #   dbExecute(con, "DELETE FROM ", data_set_name, " WHERE `ID` = ?", list(ID()))
      #   showNotification(paste("Noted! Please press 'Next case' to continue. :)"),
      #                    duration=10)
      # }
      # if(input$secondary){
      #   x <- dbExecute(con, paste0("UPDATE policy_areas SET `FMP` = 'secondary', `date_of_coding` = ? WHERE `ID` = ?"),
      #                  list(as.character(Sys.Date()), ID()))
      #   dbExecute(con, "DELETE FROM ", data_set_name, " WHERE `ID` = ?", list(ID()))
      #   showNotification(paste("Noted! Please press 'Next case' to continue. :)"),
      #                    duration=10)
      # }
      # if(!input$notFMP & !input$secondary){
        
        columns <- NULL
        values <- NULL
        
        for(r in colnames(outputdata)[5:ncol(outputdata)]){
          if(!is.null(eval(parse(text=paste("input$", r, sep=""))))){
            value <- eval(parse(text=paste("paste(input$", r, ", collapse=\",\")", sep="")))
          } else {
            value <- NA
          }
          
          values <- c(values, value)
        }
        values[which(values == "NA")] <- NA
        columns <- colnames(outputdata)
        values <- c(ID(), user(), as.character(Sys.Date()), as.numeric(!(TRUE %in% is.na(values))), values)
        values[grep("_OP", columns)] <- as.numeric(values[grep("_OP", columns)])
        
        # Update SQL
        
        dbExecute(con, paste0("UPDATE ", data_set_name, " SET ", paste0("`", columns, "` = ? ", collapse = ", "), " WHERE `ID` = ? AND `coded_by` = ?"),
                  as.list(c(values, ID(), user())))
        
        dbExecute(con, paste0(" DELETE FROM ", data_set_name, " WHERE `coded_by` = ? AND `date_updated` IS NULL"),
                  list(user()))
        showNotification(paste("Saved!"),
                         duration=10)
        
        
      # }
    }
  })
  
  # On sign in: Load data
  observeEvent(input$signin,{
    user_new <<- user()
    clicktime <<- Sys.time()
    
    # Reconnect to server in case of time out
    if(last_write < (Sys.time())){
      # More than 200 seconds (3 min) since last save
      # Reconnect to server in case of time out
      if(sql_type == "mysql"){
        try(dbDisconnect(con), silent=TRUE)
        con <<- dbConnect(MariaDB(), host = host,
                          dbname = dbname,
                          username = username,
                          password = password)
        
        message("Reconnected!")
      }
    }
    
    dbExecute(con, paste0(" DELETE FROM ", data_set_name, " WHERE `coded_by` = ? AND `date_updated` IS NULL"),
              list(user()))
    
    x <- which(users$username == user())
    
    load_data <- dbGetQuery(con, paste0("SELECT ID, date_updated FROM ", data_set_name, " WHERE coded_by = '", as.character(user()) , "'"))
    if(length(na.omit(load_data$date_updated)) > 0){
      newest <- load_data$ID[which(load_data$date_updated == max(load_data$date_updated, na.rm = TRUE))][1]
    } else {
      # newest <- sample(IDs_to_code[which(!IDs_to_code %in% dbGetQuery(con, "SELECT `ID` FROM ", data_set_name, "")$ID)], 1)
      newest <- IDs_to_code[which(users_who_code == user())][1]
    }
    # if(is.na(newest)){
    #   newest <- sample(IDs_to_code[which(!IDs_to_code %in% dbGetQuery(con, "SELECT `ID` FROM ", data_set_name, "")$ID)], 1)
    # }
    # updateTextInput(session, "ID", value="loading...")
    loadID <- newest
    message("New ID: ", newest)
    # if(nrow(load_data) == 0){
    #   loadID <- sample(IDs_to_code[which(!IDs_to_code %in% dbGetQuery(con, "SELECT `ID` FROM ", data_set_name, "")$ID)], 1)
    # }
    isolate(
      clicktime <<- update_all(session, ID=loadID, user=user())
    )
    message("loaded ", input$ID)
    
    output$username <- renderText({
      name <- isolate(users$name[which(users$username == user())])
      
      if(format(Sys.time(), "%T") < "05:00:00" | format(Sys.time(), "%T") >  "21:00:00"){
        paste("Good evening, ", name,"!", sep="")
      } else {
        if(format(Sys.time(), "%T") <  "07:30:00"){
          paste("Good morning, ", name,"!", sep="")
        } else {
          paste("Signed in as ", name, sep="") 
        }
      }
    })
    
  })
  
  # # CJEU descriptive variables ####
  # # URL and type
  # output$url <- renderUI({
  #   url <- NA
  #   if(ID() %in% hyperlinks$ID){ 
  #     url <- hyperlinks$url[which(hyperlinks$ID == ID())][1]
  #   }
  #   if(!grepl("doclang=en", paste(url), ignore.case = TRUE)){
  #     url <- paste("http://curia.europa.eu/juris/liste.jsf?critereEcli=", ID(), sep="")
  #   }
  #   hyperlink <- a(paste(unique(unlist(data$type[which(data$ID==ID())])), collapse=", "),
  #                  href=url,
  #                  target="blank")
  #   tagList(hyperlink)
  # })
  # 
  # output$docinfo <- renderUI({
  #   url <- NA
  #   # if(ID() %in% blankdata$ID){ # Create list of URLs somewhere else - master data, rather 
  #   #   url <- blankdata$url[which(blankdata$ID == ID())]
  #   # } 
  #   if(!is.na(ID())){
  #     url <- paste("https://eur-lex.europa.eu/legal-content/EN/ALL/?uri=ID:", ID(), sep="")
  #     hyperlink1 <- a("Document information; ",
  #                     href=url,
  #                     target="blank")
  #     url <- paste("https://eur-lex.europa.eu/legal-content/EN/SUM/?uri=ID:", ID(), sep="")
  #     hyperlink2 <- a(" Summary",
  #                     href=url,
  #                     target="blank")
  #     tagList(hyperlink1, hyperlink2)
  #   }
  # })
  # 
  # output$subject_matter <- renderText({
  #   paste(unique(unlist(data$subject_matter[which(data$ID==ID())])), collapse=", ")
  # })
  # 
  # output$IDnumber <- renderText({
  #   ID()
  # })
  # output$casenumber <- renderText({
  #   Decisions$case[which(Decisions$ID == ID())]
  # })
  # 
  # output$celex <- renderText({
  #   Decisions$celex[which(Decisions$ID == ID())]
  # })
  # 
  # 
  output$title <- renderText({
    
    
    # if(paste(user()) == "Sign in" | is.na(ID())){
    #   updateTextInput(session, "ID", value="Please sign in before starting")
    # } 
    # else {
    #   clicktime <<- update_all(session, ID = ID(), user=user())
    # }
    
    # updateTextInput(session, "firstpar_feedback", value=outputdata$firstpar[])
    # updateTextInput(session, "note", value=outputdata$note[])
    if(is.na(ID())){
      "No case loaded"
    } else {
      ID()
      # data$title[which(data$ID==ID())]
    }
  })
  
  output$completed <- renderUI({
    message("Updating ", Sys.time())
    if(!exists("lastmissings")){
      lastmissings <- 0
      lastID <- "missing"
    }
    # message(2)
    if(!exists("lastID2")){
      lastID2 <- "missing"
    }
    # message(3)
    if(ID() != ""){
    outputdata <- dbGetQuery(con, paste0("SELECT * FROM ", data_set_name, " WHERE `ID` = '", ID(),  "' AND `coded_by` = '", user(), "'"))
    } else {
      outputdata <- as.data.frame(NA)
    }
    fleeting_data_line <<- outputdata #this solution is UGLY - but helps load optional fields
    
    # message("ID: ", ID())
    # message("last: ", lastID2)
    # message(user())
    
    message("hi")
    
    case_complete <- NULL
    for(radio in vars){
      case_complete <- c(case_complete, !TRUE %in% eval(parse(text=paste("c(input$", radio, "[1] == \"NA\", is.null(input$", radio, "))",  sep=""))))
      # message(radio, " - ", case_complete[length(case_complete)])
    }
    if(is.na(ID()) | is.na(user()) | paste(lastID2) != paste(ID())){
      
      if(paste(outputdata$completed[1]) == "1"){
        case_complete <- rep(TRUE, length(vars))
      } else {
        case_complete <- rep(FALSE, length(vars))
      }
      message("Not checking for completed case")
      
      if(paste(ID()) != input$ID & !is.na(user())){
        isolate(
          clicktime <<- update_all(session, ID=ID(), user=user())
        )
      }
    }
    
    if(!is.na(ID()) & !is.na(user()) & paste(lastID2) == paste(ID())){
      
      if(paste(ID()) != input$ID & !is.na(user())){
        isolate(
          clicktime <<- update_all(session, ID=ID(), user=user())
        )
      }
      # if(nrow(dbGetQuery(con, paste0("SELECT * FROM ", data_set_name, " WHERE ID = ? AND coded_by = ?")), list(ID(), user())) == 0){
      #   dbExecute(con, paste0("INSERT INTO ", data_set_name, " (ID, coded_by) VALUES (?,?)"),
      #             list(ID(), user()))
      # }
      
      # Save on the go:
      colnames <- colnames(outputdata)[5:ncol(outputdata)]
      message("1")
      in_input <- lapply(colnames, function(y) 
        eval(parse(text=paste0("input$", y)))
      )
      colnames <- colnames[which(!unlist(lapply(in_input, is.null)))]
      
      in_input <- as.character(unlist(lapply(in_input[which(!unlist(lapply(in_input, is.null)))], function(y) paste(y, collapse = ","))))
      
      in_output  <- outputdata[,colnames]
      message("2")
      # in_input <- dbEscapeStrings(con, in_input)
      # in_input <- in_input
      
      x <- which(paste(in_output[colnames]) != paste(in_input))
      message("3")
      message(clicktime)
      if(length(x) > 0 & Sys.time()-1 > clicktime){
        message("4")
        if(TRUE %in% c(in_input[x] == "NA" & grepl("\\d", paste(in_output[x])))){
          showNotification(paste("WARNING: Attempting to overwrite previously recorded data with NA values.
        Automatic saving is disabled. 
        _____________________________ 
        This might have been caused by entering a new case number manually without pressing '↻'. 
        If this is the case, open this case again by pressing '←' and then '→'.
        _____________________________
        Please reload the page to continue coding.
        _____________________________
        Press the 'Confirm' button to override if you are intentionally coding NA value."),
                           duration=15, type="warning")
        } else {
          
          if(last_write < (Sys.time()-200)){
            # More than 200 seconds (3 min) since last save
            # Reconnect to server in case of time out
            if(sql_type == "mysql"){
              try(dbDisconnect(con), silent=TRUE)
              con <<- dbConnect(MariaDB(), host = host,
                                dbname = dbname,
                                username = username,
                                password = password)
              
              message("Reconnected!")
            }
          }
          
          # add completed 
          variables <- c(colnames[x], "date_updated")
          values <- c(in_input[x], as.character(Sys.time()))
          values[grep("_OP", variables)] <- as.numeric(as.logical(values[grep("_OP", variables)]))
          
          # message("user: ", user())
          dbExecute(con, paste0("UPDATE ", data_set_name, " SET ", paste0("`", variables, "` = ?", collapse = ", "), " WHERE `ID` = ? AND `coded_by` = ?"),
                    as.list(c(values, ID(), user())))
          
          
          returned <- "no connection"
          try(returned <- dbGetQuery(con, paste0("SELECT `", variables[1], "` FROM ", data_set_name, " WHERE `ID` = ? AND `coded_by` = ?"),
                                     list(ID(), user())),
              silent=TRUE)
          
          if(returned  != values[1]){
            showNotification(paste("An error appears to have ocurred when saving the data. If the problem persists, please try reloading the page. If that doesn't help, please send me (Stein Arne) an email about it. :)"),
                             duration=10)
          } else {
            last_write <<- Sys.time()
          }
        }
      }
      message("5")
      # message(paste(colnames[which(paste(in_output[colnames]) != paste(in_input))], collapse = ", "))
    }
    lastID2 <<- ID()
    
    # isolate({
    #   updateActionButton(session, "submit", label="Save")
    #   updateActionButton(session, "submit2", label="Save")
    # })
    message("6")
    if(all(case_complete)){
      if(paste(outputdata$completed) != "1"){
        message("Complete!")
        dbExecute(con, paste0("UPDATE ", data_set_name, " SET `completed` = 1 WHERE `ID` = ? AND `coded_by` = ?"),
                  list(ID(), user()))
      }
      div(
        p("Case completed!", style="color:green; text-align:center; font-size:30px;"),
        p("No missing values found in this case", style="color:green; text-align:center; font-size:15px; font-style:italic;"))
    } else {
      message("7")
      if(paste(outputdata$completed[1]) == "1"){
        dbExecute(con, paste0("UPDATE ", data_set_name, " SET `completed` = 0 WHERE `ID` = ? AND `coded_by` = ?"),
                  list(ID(), user()))
      }
      message("7.1: ", length(which(!case_complete)) < 6)
      if(length(which(!case_complete)) < 6){
        
        lastmissings <<- length(which(!case_complete))
        lastID <<- ID()
        div(
          p("Almost done!", style="color:orange; text-align:center; font-size:30px;"),
          p(paste("Still missing:", paste(variable_list$variable_name[match(vars[which(!case_complete)], variable_list$variable)], collapse=", ")),
            style="color:orange; text-align:center; font-size:15px; font-style:italic;"))
      } else {
        if(is.na(ID())){
          div(
            p("Welcome!", style="color:black; text-align:center; font-size:30px;"),
            p("Please sign in and open a case before you start.", style="color:black; text-align:center; font-size:15px; font-style:italic;"))
        } else {
          # if(input$Not_applicable){
          #   
          #   lastmissings <<- length(which(!case_complete))
          #   lastID <<- ID()
          #   div(
          #     p("Case completed!", style="color:green; text-align:center; font-size:30px;"),
          #     p("Not applicable", style="color:green; text-align:center; font-size:15px; font-style:italic;"))
          # } else {
          #   # div(
          #   #   p("Work in progress!", style="color:red; text-align:center; font-size:30px;"),
          #   #   p("I am completely changing how data is stored, please wait. Sorry about the inconvenience.", style="color:red; text-align:center; font-size:15px; font-style:italic;"))
          #   ""
          # }
          message(7.2)
          ""
        }
      }
    }
  })
  
  output$last_update <- renderText({
    message(data_set_name)
    
    paste0(ifelse(ID()=="", "Not available", dbGetQuery(con, paste0("SELECT `date_updated` FROM ", data_set_name, " WHERE `ID` = '", ID(), "' AND `coded_by` = '", user(), "'"))))
  })
  
  output$published <- renderText({
    format(data$date_document[which(data$ID==ID())], "%x")
  })
  output$lodged <- renderText({
    format(data$date_lodged[which(data$ID==ID())], "%x")
  })
  
  output$downloadData <- downloadHandler(
    filename = paste0(data_set_name, Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(dbGetQuery(con, paste0("SELECT * FROM ", data_set_name)), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$nextcase,
               {
                 newID <- NA
                 IDs <- IDs_to_code[which(users_who_code == user())]
                 message("newID")
                 if(is.na(ID())){
                   newID <- IDs[1]
                 } else {
                   if(ID() %in% IDs){
                     if(which(IDs == ID()) == length(IDs)){
                       newID <- ID()
                     } else {
                       newID <- IDs[which(IDs == ID())+1]
                     }
                   } else {
                     IDs <- dbGetQuery(con, paste0("select `ID`, `completed` from ", data_set_name, " where `coded_by` = ?"),
                                                           list(user()))$ID
                     newID <- IDs[which(IDs == ID())+1]
                     if(is.na(newID)){
                       newID <- sample(IDs_to_code[which(users_who_code == user())][1])
                     }
                   }
                 }
                 message(newID)
                 # # # IF RANDOM SELECTION
                 # # Not really done - just assigned!
                 # done <- dbGetQuery(con, paste0("select `ID`, `completed` from ", data_set_name, " where `coded_by` = ?"),
                 #                    list(user()))
                 # if("" %in% done$ID){
                 #   dbExecute(con, paste0(" DELETE FROM ", data_set_name, " WHERE `ID` = ''"))
                 #   done <- done[which(done$ID != ""),]
                 # }
                 # 
                 # if(!is.na(ID())){
                 #   if(ID() %in% done$ID){
                 #     if(which(done$ID == ID()) != nrow(done)){
                 #       newID <- done$ID[which(done$ID == ID())+1]
                 #     }
                 #   }
                 # }
                 # if(is.na(newID)){
                 #   # List IDs that are yet to be coded
                 #   IDs <- IDs_to_code[which(!IDs_to_code %in% dbGetQuery(con, "SELECT `ID` FROM ", data_set_name, "")$ID)]
                 #   if(length(IDs) == 0){
                 #     IDs <- IDs_to_code[which(!IDs_to_code %in% dbGetQuery(con, "SELECT `ID` FROM ", data_set_name, " where `completed` = 1")$ID)]
                 #   }
                 #   
                 #   # Pick one if the user has more cases left to code
                 #   if(nrow(done) < users$n_cases[which(users$username == user())]){
                 #     newID <- sample(IDs, 1)
                 #   } else {
                 #     if(all(done$completed == 1)){
                 #       showNotification(paste("You have completed all your", 
                 #                              users$n_cases[which(users$username == user())],
                 #                              "assigned cases!"), duration=NULL, type="warning")
                 #       newID <- sample(IDs, 1)
                 #     } else {
                 #       newID <- sample(done$ID[which(done$completed == 0)], 1)
                 #     }
                 #   }
                 # }
                 # # message("To ",newID)
                 # 
                 # 
                 # # isolate({
                 # #   updateTextInput(session, "ID", value=newID)
                 # # })
                 
                 clicktime <<- update_all(session, ID = newID, user=user())
               })
  
  observeEvent(input$previouscase,
               {
                 newID <- ID()
                 IDs <- IDs_to_code[which(users_who_code == user())]
                 
                 if(is.na(ID())){
                   newID <- IDs[1]
                 } else {
                   if(ID() %in% IDs){
                     if(which(IDs == ID()) == 1){
                       newID <- ID()
                     } else {
                       newID <- IDs[which(IDs == ID())-1]
                     }
                   } else {
                     IDs <- dbGetQuery(con, paste0("select `ID`, `completed` from ", data_set_name, " where `coded_by` = ?"),
                                         list(user()))$ID
                     newID <- IDs[which(IDs == ID())-1]
                     if(length(newID) == 0){
                       newID <- sample(IDs_to_code[which(users_who_code == user())][1])
                     }
                   }
                 }
                 
                 # # # From random pool:
                 # # Not really done - just assigned!
                 # done <- dbGetQuery(con, paste0("select `ID`, `completed` from ", data_set_name, " where `coded_by` = ?"),
                 #                    list(user()))
                 # if("" %in% done$ID){
                 #   dbExecute(con, paste0(" DELETE FROM ", data_set_name, " WHERE `ID` = ''"))
                 #   done <- done[which(done$ID != ""),]
                 # }
                 # 
                 # if(!is.na(ID())){
                 #   if(ID() %in% done$ID){
                 #     if(which(done$ID == ID()) != 1){
                 #       newID <- done$ID[which(done$ID == ID())-1]
                 #     }
                 #   }
                 # }
                 # 
                 # # isolate({
                 # #   updateTextInput(session, "ID", value=newID)
                 # # })
                 
                 clicktime <<- update_all(session, ID = newID, user=user())
               })
  
  
  observeEvent(input$update,{
    
    
    if(!is.na(ID()) & !is.na(user())){
      clicktime <<- update_all(session, ID = ID(), user = user())
    }
    
    clicktime <<- Sys.time()
  })
  observeEvent(input$statistics,
               {
                 if(user() != "Sign in"){
                   coded <- dbGetQuery(con, paste0("SELECT ",
                                                   paste("`", c("ID", "date_coded_text", unique(variable_list$variable)),"`", collapse=",", sep=""),
                                                   " FROM ", data_set_name, " WHERE coded_by = '", user() , "'"))
                   
                   
                   coded$date_coded_text[which(is.na(coded$date_coded_text))] <- "Unknown"
                   
                   coded_TF <- apply(coded, 2, function(y) paste(y) != "NA")
                   
                   n_cases <- paste0(length(which(apply(coded_TF, 1, sum)==ncol(coded_TF))),
                                     " of ",
                                     length(IDs_to_code[which(users_who_code == user())]),
                                     " cases coded (", 
                                     round(length(which(apply(coded_TF, 1, sum)==ncol(coded_TF)))/length(IDs_to_code[which(users_who_code == user())])*100),
                                     "%)")
                   
                   percentages <- round((apply(coded_TF, 1, sum)-4)/(ncol(coded_TF)-4)*100)
                   if(length(which(percentages < 100 & percentages > 30)) > 0){
                     table <- data.frame(ID=coded$ID[which(percentages < 100 & percentages > 30)],
                                         completion=paste0(percentages[which(percentages < 100 & percentages > 30)], "%"),
                                         `date`= gsub(" .*$", "", coded$date_coded_text[which(percentages < 100 & percentages > 30)]),
                                         `time`= gsub("^.* ", "", coded$date_coded_text[which(percentages < 100 & percentages > 30)])
                     )
                   } else {
                     table <- data.frame(ID="No cases appear to be partially completed",
                                         completion="")
                   }
                   
                   showModal(modalDialog(
                     div(n_cases,
                         hr(),
                         h4("Partially completed cases:"),
                         renderTable(table)
                     ),
                     title = paste("User statistics for", users$name[which(users$username == user())]),
                     easyClose = TRUE
                   ))
                 }
                 else {
                   coded <- dbGetQuery(con, paste0("SELECT ",
                                                   paste("`", c("ID", "date_updated", "coded_by", unique(variable_list$variable)),"`", collapse=",", sep=""),
                                                   " FROM ", data_set_name))
                   
                   
                   coded_TF <- apply(coded, 2, function(y) paste(y) != "NA")
                   table <- data.frame(table(users_who_code))
                   colnames(table) <- c("user", "assigned")
                   completed <- table(coded$coded_by[which(apply(coded_TF, 1, sum)==ncol(coded_TF))])
                   table$completed <- 0
                   table$completed[match(names(completed), table$user)] <- round(completed)
                   table$completed <- as.integer(table$completed)
                   table$progress <- paste0(round(table$completed/table$assigned*100), "%")
                   
                   showModal(modalDialog(
                     # div(h4("Partially completed cases:"),
                     renderTable(table),
                     # ),
                     title = paste("User statistics"),
                     easyClose = TRUE
                   ))
                 }
               })
  
  observeEvent(input$timer,
               {
                 showNotification(paste("It has been", round(difftime(Sys.time(), starttime, units="hours"),2),
                                        "hours since you started working"),
                                  duration=10)
               })
  
  
  # sign out on exit ####
  session$onSessionEnded(function(){
    message("Bye!")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
