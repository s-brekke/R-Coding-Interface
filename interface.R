#                 _____          _ _                 _       _             __               
#                / ____|        | (_)               (_)     | |           / _|              
#               | |     ___   __| |_ _ __   __ _     _ _ __ | |_ ___ _ __| |_ __ _  ___ ___ 
#               | |    / _ \ / _` | | '_ \ / _` |   | | '_ \| __/ _ \ '__|  _/ _` |/ __/ _ \
#               | |___| (_) | (_| | | | | | (_| |   | | | | | ||  __/ |  | || (_| | (_|  __/
#                \_____\___/ \__,_|_|_| |_|\__, |   |_|_| |_|\__\___|_|  |_| \__,_|\___\___|
#                                           __/ |                                         
#                         EUI, 2020         |___/          Written by Stein Arne Brekke
#
# README:
# Before you start coding, you have to decide if you want to use MariaDB/MySQL or SQLite. MariaDB/MySQL runs on
# a centralized server, while SQLite stores the data locally.
# 
# You will need to install the SQL server of your choice on your computer/server if this is not already done.
# 
# You also need to install the following dependencies:
# install.packages("shiny")
# install.packages("DBI")
# install.packages("RMySQL") # For RMySQL
# install.packages("RSQLite") # For SQLite
# Only run these commands once per computer.
# 
# Remember to update systemdata/users.csv, systemdata/config.csv and systemdata/delegation.csv before

library(shiny)
library(DBI)

# Load config file: 
config <- read.csv(file.path("systemdata", "config.csv"), stringsAsFactors = FALSE); rownames(config) <- config$setting
config <- as.data.frame(t(config))[2,]

sql_type <- tolower(config$sql_type)
data_base_name <- as.character(config$data_base_name)
data_set_name <- as.character(config$data_set_name)
showguide <- as.logical(config$showguide)

# variable_list #####
# Read variable_list from excel if possible
if("readxl" %in% rownames(installed.packages())){                                 # can be cut
  library(readxl)                                                                 # can be cut
  variable_list <- read_excel(file.path("systemdata","variable_list.xlsx"))                 # can be cut
  write.csv(variable_list, file.path("systemdata", "variable_list.csv"), row.names = FALSE) # can be cut
} else {                                                                          # can be cut
  variable_list <- read.csv(file.path("systemdata", "variable_list.csv"),  stringsAsFactors=FALSE) # KEEEP THIS!
}                                                                                 # can be cut
variable_list <- variable_list[which(!grepl("invisible value", variable_list$interpretation)),]

columns <- c("ecli", "coded_by", "date_coded", "date_coded_text", unique(variable_list$variable))


if(sql_type == "mysql"){
  library(RMySQL)
  
  # mariaDB or MySQL: Connect to server ####
  dbname <- data_base_name
  con <- dbConnect(MySQL(), host = host,
                   dbname = dbname,
                   username = username,
                   password = password)
  outputdata <- dbGetQuery(con, paste0("SELECT * FROM ", data_set_name))
} else 
{
  library(RSQLite)
  con <- dbConnect(SQLite(), paste0(data_base_name, ".db"))
  if(!data_set_name %in% unlist(dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type = 'table'"))){
    table <- as.data.frame(t(matrix(columns)))[0,]
    colnames(table) <- columns
    dbWriteTable(con, data_set_name, table)
  }
  outputdata <- dbGetQuery(con, paste0("SELECT * FROM ", data_set_name))
}

# Add new variable(s) 
if(length(columns[which(!columns %in% colnames(outputdata))]) > 0){
  for(c in columns[which(!columns %in% colnames(outputdata))]){
    dbSendStatement(con, paste0("ALTER TABLE ", data_set_name, " ADD COLUMN ",
                                paste(c, NA, "TEXT")))
  }
  outputdata <- dbGetQuery(con, paste0("SELECT * FROM ", data_set_name))
}

# Delegate cases to users
users <- read.csv(file.path("systemdata", "users.csv"), stringsAsFactors = FALSE); rownames(users) <- users$usernames
delegated_cases <- read.csv(file.path("systemdata", "delegation.csv"), stringsAsFactors = FALSE)

eclis_to_code <- as.character(delegated_cases$ID)
users_who_code <- as.character(delegated_cases$user)


# variable_list #####
# Read variable_list from excel if possible
if("readxl" %in% rownames(installed.packages())){                                 # can be cut
  library(readxl)                                                                 # can be cut
  variable_list <- read_excel(file.path("systemdata","variable_list.xlsx"))                 # can be cut
  write.csv(variable_list, file.path("systemdata", "variable_list.csv"), row.names = FALSE) # can be cut
} else {                                                                          # can be cut
  variable_list <- read.csv(file.path("systemdata", "variable_list.csv"),  stringsAsFactors=FALSE) # KEEEP THIS!
}                                                                                 # can be cut
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

load(file.path("systemdata", "cjeu", "hyperlinks.rda"))
# if(!"CJEU" %in% rownames(installed.packages())){
load(file.path("systemdata", "cjeu", "Decisions.rda"))
load(file.path("systemdata", "cjeu", "Cases.rda"))
# }
data <- Decisions
data$title <- Cases$title[match(data$case, Cases$case)]



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
    
    if(showguide){
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
      textAreaInput(vars[n], 
                    variable_list$variable_name[which(variable_list$variable== vars[n])][1],
                    placeholder = variable_list$description[which(variable_list$variable== vars[n])][1])
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
                "Paste text snippet:",
                value=text_val,
                placeholder = fillertext)
    },
    tags$hr()
  )
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
      
      textInput("ecli", 
                "ECLI:",
                c(outputdata$ecli[which(outputdata$date_coded == max(c(na.omit(outputdata$date_coded[which(outputdata$coded_by == paste(user_new))])),0) &
                                          outputdata$coded_by == paste(user_new))], "")[1],
                placeholder = "Please sign in before starting"),
      
      tags$div(actionButton("previouscase", "← previous", width="40%"),
               
               actionButton("randomcase", "?", width="15%"),
               
               actionButton("nextcase", "next →", width="40%"),
               style="display:inline-block; width:100%"),
      
      
      tags$h3(textOutput("title")),
      
      tags$i(uiOutput("url")), 
      
      tags$div(tags$b("Case:"),
               style="display:inline-block"),
      tags$div(textOutput("casenumber"),
               style="display:inline-block"),
      tags$br(),
      tags$div(tags$b("ECLI:"),
               style="display:inline-block"),
      tags$div(textOutput("eclinumber"),
               style="display:inline-block"),
      tags$br(),
      
      tags$div(tags$b("CELEX:"),
               style="display:inline-block"),
      tags$div(textOutput("celex"),
               style="display:inline-block"),
      tags$br(),
      
      tags$i(uiOutput("docinfo")), 
      
      tags$b("Subject matter:"),
      textOutput("subject_matter"),
      
      tags$b("Date of lodging:"),
      textOutput("lodged"),
      
      tags$b("Date of document:"),
      textOutput("published"),
      
      tags$b("Last updated:"),
      textOutput("last_update"),
      
      
      checkboxInput("Not_applicable", label="Not decided as free movement of goods", 
                    value=FALSE),
      # uiOutput("case_not_applicable"),
      
      
      
      selectInput("user", "User:", c(users$usernames)),
      textOutput("username"),
      actionButton("signin", "Sign in"),
      tags$hr(),
      div(HTML(headlinelinks)),
      tags$hr(), # ONLY ONLINE
      actionButton("submit", "Save"),
      actionButton("statistics", "User statistics"),
      downloadButton("downloadData", "Download data")
      
      # tags$p("For online users:"), # ONLY ONLINE
      # actionButton("submit", "Save"),
      # downloadButton("downloadData", "Download user data"), # ONLY ONLINE
      # downloadButton("downloadCSV", "Download in CSV format"), # ONLY ONLINE
      # 
    ),
    
    
    # Main panel for the hand coders:
    mainPanel(
      
      
      # THIS IS WHERE IT HAPPENS:
      # add one radio_survey (multiple choice) or textAreaInput (text variable) for every variable
      # in variable_list.csv
      
      tags$div(uiOutput("completed")), 
      
      radio_survey(1),
      radio_survey(2),
      radio_survey(3),
      radio_survey(4),
      radio_survey(5),
      
      tags$hr(),
      
      actionButton("submit2", "Save"),
      actionButton("timer", "Show timer"),
      tags$br(),
      a("Click here to return to the top", href="#top", name="End"),
      
    )
  )
)


# Now for the server side:
server <- function(input, output, session) {
  
  # Collect "ecli". Universal variables have to be defined as reactive functions.
  ecli <- reactive(Decisions$ecli[which(Decisions$ecli == gsub("^\\W*|\\W*$", "", input$ecli) |
                                          Decisions$celex == gsub("^\\W*|\\W*$", "", input$ecli) |
                                          Decisions$case == gsub("^(\\d+/)", "C-\\1", gsub("^\\W*|\\W*$", "", input$ecli)) & 
                                          Decisions$type == "Judgment"
  )][1])
  
  user <- eventReactive(input$signin, {
    input$user
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
        con <<- dbConnect(MySQL(), host = host,
                          dbname = dbname,
                          username = username,
                          password = password)
        
        message("Reconnected!")
      }
    }
    
    x <- which(users$username == user())
    
    newest <- dbGetQuery(con, paste0("SELECT ecli,date_coded,date_coded_text FROM ", data_set_name, " WHERE coded_by = '", as.character(user()) , "'"))
    if(length(na.omit(newest$date_coded)) > 0){
      newest <- newest$ecli[which(newest$date_coded == max(newest$date_coded, na.rm = TRUE))][1]
    } else {
      newest <- ecli()
    }
    updateTextInput(session, "ecli", value="loading...")
    loadecli <- newest
    if(is.na(loadecli)){
      loadecli <- eclis_to_code[which(users_who_code == user())][1]
    }
    updateTextInput(session, "ecli", value=loadecli)
    message("loaded ", loadecli)
    
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
  
  # Descriptive variables ####
  # URL and type
  output$url <- renderUI({
    url <- NA
    if(ecli() %in% hyperlinks$ecli){ 
      url <- hyperlinks$url[which(hyperlinks$ecli == ecli())][1]
    }
    if(!grepl("doclang=en", paste(url), ignore.case = TRUE)){
      url <- paste("http://curia.europa.eu/juris/liste.jsf?critereEcli=", ecli(), sep="")
    }
    hyperlink <- a(paste(unique(unlist(data$type[which(data$ecli==ecli())])), collapse=", "),
                   href=url,
                   target="blank")
    tagList(hyperlink)
  })
  
  output$q4 <- renderUI({
    q <- 4 # TYPEDEF
    if(input$DEF == 1){
      if(length(x) > 0){
        div(
          radio_survey(q,
                       select = ifelse(fleeting_data_line[, radios[q]] == -888, "NA", paste(fleeting_data_line[, radios[q]])),
                       p1_val = fleeting_data_line[, paste(radios[q], "p1", sep="_")],
                       p2_val = fleeting_data_line[, paste(radios[q], "p2", sep="_")],
                       op_val = as.logical(fleeting_data_line[, paste(radios[q], "OP", sep="_")]),
                       text_val = fleeting_data_line[, paste(radios[q], "TEXT", sep="_")]
          ),
          style="margin-left: 40px;")
      } else {
        div(
          radio_survey(q),
          style="margin-left: 40px;")
      }
    }
  })
  
  # output$case_not_applicable <- renderUI({
  #   if(input$Not_applicable){
  #     textAreaInput("Why_not_applicable", label="Why?", placeholder="Please briefly explain why the case is not applicable", 
  #                   value=gsub("^NA$", "", paste(outputdata$Why_not_applicable)))
  #   }
  # })
  
  output$q6 <- renderUI({
    q <- 6 # TYPEDEFOP
    if(input$DEFOP == 1){
      if(length(x) > 0){
        div(
          radio_survey(q,
                       select = ifelse(fleeting_data_line[, radios[q]] == -888, "NA", paste(fleeting_data_line[, radios[q]])),
                       p1_val = fleeting_data_line[, paste(radios[q], "p1", sep="_")],
                       p2_val = fleeting_data_line[, paste(radios[q], "p2", sep="_")],
                       op_val = as.logical(fleeting_data_line[, paste(radios[q], "OP", sep="_")]),
                       text_val = fleeting_data_line[, paste(radios[q], "TEXT", sep="_")]
          ),
          style="margin-left: 40px;")
      } else {
        div(
          radio_survey(q),
          style="margin-left: 40px;")
      }
    }
  })
  output$q24 <- renderUI({
    q <- 24 # DEFEX2
    
    if(!input$DEFEX1 %in% c("0", "NA")){
      if(length(x) > 0){
        div(
          radio_survey(q,
                       select = ifelse(fleeting_data_line[, radios[q]] == -888, "NA", paste(fleeting_data_line[, radios[q]])),
                       p1_val = fleeting_data_line[, paste(radios[q], "p1", sep="_")],
                       p2_val = fleeting_data_line[, paste(radios[q], "p2", sep="_")],
                       op_val = as.logical(fleeting_data_line[, paste(radios[q], "OP", sep="_")]),
                       text_val = fleeting_data_line[, paste(radios[q], "TEXT", sep="_")]
          ),
          style="margin-left: 40px;")
      } else {
        div(
          radio_survey(q),
          style="margin-left: 40px;")
      }
    }
  })
  
  output$docinfo <- renderUI({
    url <- NA
    # if(ecli() %in% blankdata$ecli){ # Create list of URLs somewhere else - master data, rather 
    #   url <- blankdata$url[which(blankdata$ecli == ecli())]
    # } 
    if(!is.na(ecli())){
      url <- paste("https://eur-lex.europa.eu/legal-content/EN/ALL/?uri=ecli:", ecli(), sep="")
      hyperlink1 <- a("Document information; ",
                      href=url,
                      target="blank")
      url <- paste("https://eur-lex.europa.eu/legal-content/EN/SUM/?uri=ecli:", ecli(), sep="")
      hyperlink2 <- a(" Summary",
                      href=url,
                      target="blank")
      tagList(hyperlink1, hyperlink2)
    }
  })
  
  
  output$subject_matter <- renderText({
    paste(unique(unlist(data$subject_matter[which(data$ecli==ecli())])), collapse=", ")
  })
  
  output$eclinumber <- renderText({
    ecli()
  })
  output$casenumber <- renderText({
    Decisions$case[which(Decisions$ecli == ecli())]
  })
  
  output$celex <- renderText({
    Decisions$celex[which(Decisions$ecli == ecli())]
  })
  
  # This function updates most things, not just the title
  output$title <- renderText({
    
    
    if(user() == "Sign in"){
      updateTextInput(session, "ecli", value="Please sign in before starting")
    } else {
      
      outputdata <- dbGetQuery(con, paste0("SELECT * FROM ",data_set_name, " WHERE `ecli` = '", ecli(),  "' AND `coded_by` = '", as.character(user()), "'"))
      
      # Insert new row if missing
      if(nrow(dbGetQuery(con, paste0("SELECT * FROM ", data_set_name, " WHERE ecli = '", ecli(), "' AND coded_by = '", user(), "'"))) == 0){
        dbSendStatement(con, paste0("INSERT INTO ", data_set_name, " (ecli, coded_by) VALUES ('", ecli(), "', '", user(), "')"))
      }
      
      # message("Loaded: ", outputdata$ecli[1], " as ", outputdata$coded_by[1])
      # message(paste(colnames(outputdata), collapse=", "))
      
      for(t in text_variables){
        isolate({
          updateTextAreaInput(session, 
                              t,
                              value=outputdata[, t])
        })
      }
      clicktime <<- Sys.time()
      
      updateCheckboxInput(session,
                          "Not_applicable",
                          value=(paste(outputdata$Not_applicable) == TRUE))
      # Update radio boxes
      for(radio in radios){
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
          operative_part <- ifelse(!ecli() %in% outputdata$ecli,
                                   "FALSE",
                                   paste(outputdata[,paste(radio, "_OP", sep="")]) == TRUE)
          updateCheckboxInput(session,
                              paste(radio, "_OP", sep=""),
                              value=as.logical(operative_part))
        }
        
        isolate({
          if(!is.null(eval(parse(text=paste("input$", radio, "_TEXT", sep=""))))){
            setvalue <- ifelse(is.na(outputdata[, paste(radio, "_TEXT", sep="")]),
                               "",
                               outputdata[, paste(radio, "_TEXT", sep="")])
            updateTextInput(session,
                            paste(radio, "_TEXT", sep=""),
                            value=isolate(setvalue))
          }
          if(!is.null(eval(parse(text=paste("input$", radio, "_p1", sep=""))))){
            
            setvalue <- ifelse(is.na(outputdata[, paste(radio, "_p1", sep="")]),
                               "",
                               outputdata[, paste(radio, "_p1", sep="")])
            updateNumericInput(session,
                               paste(radio, "_p1", sep=""),
                               value=isolate(setvalue))
            setvalue <- ifelse(is.na(outputdata[, paste(radio, "_p2", sep="")]),
                               "",
                               outputdata[, paste(radio, "_p2", sep="")])
            updateNumericInput(session,
                               paste(radio, "_p2", sep=""),
                               value=isolate(setvalue))
          }
        })
      }
    }
    
    # updateTextInput(session, "firstpar_feedback", value=outputdata$firstpar[])
    # updateTextInput(session, "note", value=outputdata$note[])
    data$title[which(data$ecli==ecli())]
  })
  
  output$completed <- renderUI({
    
    if(!exists("lastmissings")){
      lastmissings <- 0
      lastecli <- "missing"
    }
    if(!exists("lastecli2")){
      lastecli2 <- "missing"
    }
    
    outputdata <- dbGetQuery(con, paste0("SELECT * FROM ", data_set_name, " WHERE `ecli` = '", ecli(),  "' AND `coded_by` = '", user(), "'"))
    
    fleeting_data_line <<- outputdata #this solution is UGLY - but helps load optional fields
    
    case_complete <- NULL
    
    for(radio in radios){
      case_complete <- c(case_complete, !TRUE %in% eval(parse(text=paste("c(input$", radio, "[1] == \"NA\", is.null(input$", radio, "))",  sep=""))))
    }
    
    # message("ecli: ", ecli())
    
    if(!is.na(ecli()) & !is.na(user()) & paste(lastecli2) == paste(ecli())){
      
      # Reconnect if connection to server is lost
      
      # Save on the go:
      colnames <- colnames(outputdata)[5:ncol(outputdata)]
      
      in_input <- lapply(colnames, function(y) 
        eval(parse(text=paste0("input$", y)))
      )
      
      colnames <- colnames[which(!unlist(lapply(in_input, is.null)))]
      in_input <- as.character(unlist(lapply(in_input[which(!unlist(lapply(in_input, is.null)))], function(y) paste(y, collapse = ","))))
      
      
      
      in_output  <- outputdata[,colnames]
      # in_input <- dbEscapeStrings(con, in_input)
      in_input <- in_input
      
      x <- which(paste(in_output[colnames]) != paste(in_input))
      
      if(length(x) > 0 & Sys.time()-1 > clicktime){
        if(TRUE %in% c(in_input[x] == "NA" & grepl("\\d", paste(in_output[x])))){
          showNotification(paste("WARNING: Attempting to overwrite previously recorded data with NA values.
        Automatic saving is disabled.
        _____________________________
        Please reload the page to continue coding.
        _____________________________
        Press the 'Save' button to override. THIS WILL CAUSE LOSS OF DATA"),
                           duration=15)
        } else {
          
          if(last_write < (Sys.time()-200)){
            # More than 200 seconds (3 min) since last save
            # Reconnect to server in case of time out
            if(sql_type == "mysql"){
              try(dbDisconnect(con), silent=TRUE)
              con <<- dbConnect(MySQL(), host = host,
                                dbname = dbname,
                                username = username,
                                password = password)
              
              message("Reconnected!")
            }
          }
          
          variables <- c(colnames[x], "date_coded", "date_coded_text")
          values <- c(in_input[x], Sys.time(), as.character(Sys.time()))
          
          values <- gsub(":", "dot COLON dot", values, fixed=TRUE)
          values <- gsub("-", "dot DASH dot", values, fixed=TRUE)
          values <- gsub(".", "dot D O T dot", values, fixed=TRUE)
          values <- gsub(",", "comma D O T comma", values, fixed=TRUE)
          values <- gsub("\\", "/", values, fixed=TRUE)
          values <- gsub("\\W", " ", values)
          values <- gsub("comma D O T comma", ",", values, fixed=TRUE)
          values <- gsub("dot D O T dot", ".", values, fixed=TRUE)
          values <- gsub("dot DASH dot", "-", values, fixed=TRUE)
          values <- gsub("dot COLON dot", ":", values, fixed=TRUE)
          
          # message("user: ", user())
          
          dbExecute(con, paste0("UPDATE ", data_set_name, " SET ", paste0("`", variables, "` = '", values, "'", collapse = ", "), " WHERE `ecli` = '", ecli(), "' AND `coded_by` = '", user(), "'"))
          
          returned <- "no connection"
          returned <- try(dbGetQuery(con, paste0("SELECT `", variables[1], "` FROM ", data_set_name, " WHERE `ecli` = '", ecli(), "' AND `coded_by` = '", user(), "'")),
                          silent=TRUE)
          
          if(returned  != values[1]){
            showNotification(paste("An error appears to have ocurred when saving the data. If the problem persists, please try reloading the page. If that doesn't help, please send me (Stein Arne) an email about it. :)"),
                             duration=10)
          } else {
            last_write <<- Sys.time()
          }
        }
      }
      
      # message(paste(colnames[which(paste(in_output[colnames]) != paste(in_input))], collapse = ", "))
    }
    lastecli2 <<- ecli()
    
    isolate({
      updateActionButton(session, "submit", label="Save")
      updateActionButton(session, "submit2", label="Save")
    })
    
    if(all(case_complete)){
      div(
        p("Case completed!", style="color:green; text-align:center; font-size:30px;"),
        p("No missing values found in this case", style="color:green; text-align:center; font-size:15px; font-style:italic;"))
    } else {
      if(length(which(!case_complete)) < 6){
        
        lastmissings <<- length(which(!case_complete))
        lastecli <<- ecli()
        div(
          p("Almost done!", style="color:orange; text-align:center; font-size:30px;"),
          p(paste("Still missing:", paste(variable_list$variable_name[match(radios[which(!case_complete)], variable_list$variable)], collapse=", ")),
            style="color:orange; text-align:center; font-size:15px; font-style:italic;"))
      } else {
        if(is.na(ecli())){
          div(
            p("Welcome!", style="color:black; text-align:center; font-size:30px;"),
            p("Please sign in and open a case before you start.", style="color:black; text-align:center; font-size:15px; font-style:italic;"))
        } else {
          if(input$Not_applicable){
            
            lastmissings <<- length(which(!case_complete))
            lastecli <<- ecli()
            div(
              p("Case completed!", style="color:green; text-align:center; font-size:30px;"),
              p("Not applicable", style="color:green; text-align:center; font-size:15px; font-style:italic;"))
          } else {
            # div(
            #   p("Work in progress!", style="color:red; text-align:center; font-size:30px;"),
            #   p("I am completely changing how data is stored, please wait. Sorry about the inconvenience.", style="color:red; text-align:center; font-size:15px; font-style:italic;"))
            ""
          }
        }
      }
    }
  })
  
  output$last_update <- renderText({
    paste0(dbGetQuery(con, paste0("SELECT `date_coded_text` FROM ", data_set_name, " WHERE `ecli` = '", ecli(), "' AND `coded_by` = '", user(), "'")))
  })
  
  output$published <- renderText({
    format(data$date_document[which(data$ecli==ecli())], "%x")
  })
  output$lodged <- renderText({
    format(data$date_lodged[which(data$ecli==ecli())], "%x")
  })
  
  output$downloadData <- downloadHandler(
    filename = paste0(data_set_name, Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(dbGetQuery(con, paste0("SELECT * FROM ", data_set_name)), file, row.names = FALSE)
    }
  )
  
  # # Early XML test
  # output$firstpar <- renderText({
  #   firstpar <- getPar(ecli(), "1")
  #   ifelse(is.na(firstpar), "First paragraph not found.", firstpar)
  # })
  
  # Save data and manouver through cases: ####
  
  
  observeEvent(input$submit,
               {
                 if(!is.na(ecli())){
                   
                   columns <- NULL
                   values <- NULL
                   
                   for(r in colnames(outputdata)[5:ncol(outputdata)]){
                     if(!is.null(eval(parse(text=paste("input$", r, sep=""))))){
                       value <- eval(parse(text=paste("paste(input$", r, ", collapse=\",\")", sep="")))
                     } else {
                       value <- "NA"
                     }
                     
                     columns <- c(columns, r)
                     values <- c(values, value)
                   }
                   
                   # This can and should be solved much better - should be easy as well.
                   # Maybe use RmariaDB instead of RMySQL.
                   # In SQLite it can be solved with ? and list(), but that doesn't work in MySQL
                   values <- gsub(":", "dot COLON dot", values, fixed=TRUE)
                   values <- gsub("-", "dot DASH dot", values, fixed=TRUE)
                   values <- gsub(".", "dot D O T dot", values, fixed=TRUE)
                   values <- gsub(",", "comma D O T comma", values, fixed=TRUE)
                   values <- gsub("\\", "/", values, fixed=TRUE)
                   values <- gsub("\\W", " ", values)
                   values <- gsub("comma D O T comma", ",", values, fixed=TRUE)
                   values <- gsub("dot D O T dot", ".", values, fixed=TRUE)
                   values <- gsub("dot DASH dot", "-", values, fixed=TRUE)
                   values <- gsub("dot COLON dot", ":", values, fixed=TRUE)
                   
                   # Update SQL
                   dbExecute(con, paste0("UPDATE ", data_set_name, " SET ", paste0("`", columns, "` = '", values, "'", collapse = ", "), " WHERE `ecli` = '", ecli(), "' AND `coded_by` = '", user(), "'"))
                   
                   updateActionButton(session, "submit", label="Saved!")
                 }
               })  
  
  observeEvent(input$submit2,
               {
                 if(!is.na(ecli())){
                   broken_radios <- NULL
                   if(input$DEF == 0){broken_radios <- c(broken_radios, "TYPEDEF")}
                   if(input$DEFOP == 0){broken_radios <- c(broken_radios, "TYPEDEFOP")}
                   if(input$DEFEX1 == 0){broken_radios <- c(broken_radios, "DEFEX2")}
                   
                   columns <- NULL
                   values <- NULL
                   
                   for(r in colnames(outputdata)[grepl("[[:upper:]]", colnames(outputdata))]){
                     if(!is.null(eval(parse(text=paste("input$", r, sep=""))))){
                       value <- eval(parse(text=paste("paste(input$", r, ", collapse=\",\")", sep="")))
                       if(r %in% broken_radios){
                         value <- "-888"
                       }
                     } else {
                       value <- "NA"
                     }
                     
                     columns <- c(columns, r)
                     values <- c(values, value)
                   }
                   
                   
                   values <- gsub(":", "dot COLON dot", values, fixed=TRUE)
                   values <- gsub("-", "dot DASH dot", values, fixed=TRUE)
                   values <- gsub(".", "dot D O T dot", values, fixed=TRUE)
                   values <- gsub(",", "comma D O T comma", values, fixed=TRUE)
                   values <- gsub("\\", "/", values, fixed=TRUE)
                   values <- gsub("\\W", " ", values)
                   values <- gsub("comma D O T comma", ",", values, fixed=TRUE)
                   values <- gsub("dot D O T dot", ".", values, fixed=TRUE)
                   values <- gsub("dot DASH dot", "-", values, fixed=TRUE)
                   values <- gsub("dot COLON dot", ":", values, fixed=TRUE)
                   
                   # Update SQL
                   dbExecute(con, paste0("UPDATE ", data_set_name, " SET ", paste0("`", columns, "` = '", values, "'", collapse = ", "), " WHERE `ecli` = '", ecli(), "' AND `coded_by` = '", user(), "'"))
                   
                   
                   updateActionButton(session, "submit2", label="Saved!")
                 }
               })  
  
  observeEvent(input$nextcase,
               {
                 
                 # message("From ", ecli())
                 
                 if(user() %in% users_who_code){
                   eclis_to_code <- eclis_to_code[which(users_who_code == user())]
                 } else {
                   eclis_to_code <- unique(eclis_to_code)
                   users_who_code <- rep(user(), length(eclis_to_code))
                 }
                 if(ecli() %in% eclis_to_code & ecli() != eclis_to_code[length(eclis_to_code)]){
                   newecli <- eclis_to_code[which(eclis_to_code == ecli())+1]
                 } else {
                   newecli <- eclis_to_code[length(eclis_to_code)]
                 }
                 if(is.na(newecli)){
                   newecli <- ecli()
                 }
                 
                 # message("To ",newecli)
                 
                 isolate({
                   updateTextInput(session, "ecli", value=newecli)
                 })
                 clicktime <<- Sys.time()
               })
  
  observeEvent(input$previouscase,
               {
                 # Update ECLI code
                 if(user() %in% users_who_code){
                   eclis_to_code <- eclis_to_code[which(users_who_code == user())]
                 } else {
                   eclis_to_code <- unique(eclis_to_code)
                   users_who_code <- rep(user(), length(eclis_to_code))
                 }
                 if(ecli() %in% eclis_to_code & ecli() != eclis_to_code[1]){
                   newecli <- eclis_to_code[which(eclis_to_code == ecli())-1]
                 } else {
                   newecli <- eclis_to_code[1]
                 }
                 if(is.na(newecli)){
                   newecli <- ecli()
                 }
                 
                 isolate({
                   updateTextInput(session, "ecli", value=newecli)
                 })
                 clicktime <<- Sys.time()
               })
  
  observeEvent(input$randomcase,
               {
                 data_by_user <- dbGetQuery(con, paste0("SELECT * FROM ", data_set_name, " WHERE coded_by = '", user(), "'"))
                 
                 if(user() %in% users_who_code){
                   eclis_to_code <- eclis_to_code[which(users_who_code == user())]
                   eclis_to_code <- eclis_to_code[which(!eclis_to_code %in% data_by_user[which(!apply(data_by_user, 1, function(y) "NA" %in% y)),"ecli"])]
                 } else {
                   eclis_to_code <- unique(eclis_to_code)
                   users_who_code <- rep(user(), length(eclis_to_code))
                 }
                 
                 newecli <- sample(eclis_to_code,1)
                 
                 if(is.na(newecli)){
                   newecli <- ecli()
                 }
                 
                 isolate({
                   updateTextInput(session, "ecli", value=newecli)
                 })
                 
                 clicktime <<- Sys.time()
                 
                 
               })
  
  observeEvent(input$statistics,
               {
                 if(user() != "Sign in"){
                   coded <- dbGetQuery(con, paste0("SELECT ",
                                                   paste("`", c("ecli", "date_coded_text", unique(variable_list$variable)),"`", collapse=",", sep=""),
                                                   " FROM ", data_set_name, " WHERE coded_by = '", user() , "'"))
                   
                   
                   coded$date_coded_text[which(is.na(coded$date_coded_text))] <- "Unknown"
                   
                   coded_TF <- apply(coded, 2, function(y) paste(y) != "NA")
                   
                   n_cases <- paste0(length(which(apply(coded_TF, 1, sum)==ncol(coded_TF))),
                                     " of ",
                                     length(eclis_to_code[which(users_who_code == user())]),
                                     " cases coded (", 
                                     round(length(which(apply(coded_TF, 1, sum)==ncol(coded_TF)))/length(eclis_to_code[which(users_who_code == user())])*100),
                                     "%)")
                   
                   percentages <- round((apply(coded_TF, 1, sum)-4)/(ncol(coded_TF)-4)*100)
                   if(length(which(percentages < 100 & percentages > 30)) > 0){
                     table <- data.frame(ecli=coded$ecli[which(percentages < 100 & percentages > 30)],
                                         completion=paste0(percentages[which(percentages < 100 & percentages > 30)], "%"),
                                         `date`= gsub(" .*$", "", coded$date_coded_text[which(percentages < 100 & percentages > 30)]),
                                         `time`= gsub("^.* ", "", coded$date_coded_text[which(percentages < 100 & percentages > 30)])
                     )
                   } else {
                     table <- data.frame(ecli="No cases appear to be partially completed",
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
                                                   paste("`", c("ecli", "date_coded_text", "coded_by", "Not_applicable", unique(variable_list$variable)),"`", collapse=",", sep=""),
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
