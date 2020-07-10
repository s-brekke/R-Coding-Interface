# CJEU Coding Interface

R Shiny based hand coding interface created for CJEU-related purposes. Can be converted into different use
cases, provided that hard-coded references to ECLI numbers (ID codes used for CJEU case law) are changed.

## Getting started 
Four tables need to be updated before you start coding, all of which are located in the **systemdata** folder. These are:

#### users.csv
This contains a list of users of the interface. If only one user is needed, provide only one user in the table.

#### config.csv
Configuration files. Use this to change the name of the data base or data table, or to use MySQL/MariaDB instead of
SQLite. MySQL and MariaDB must be used if the app is uploaded to shinyapps.io.

#### delegation.csv
A list of ECLI numbers and the user names of the users who these cases are assigned to.

#### variable_list.xlsx / variable_list.csv
A list of variable names, descriptions, and possible values. 

Changes in *variable_list.xlsx* will automatically be adapted and saved as *variable_list.csv* if the "readxl"
package is installed in R. The script can also be easily changed to always read *variable_list.csv*, but do so
with caution as this is a complex csv file that can not be easily saved from Microsoft Excel.

### Make changes to interface
After updating the variable list, these changes must be reflected in *interface.R*.

Find the headline in the code named "LIST OF VARIABLES".

Under this headline there are two types of outputs: 

```R
radio_survey(n)
```

for variables with multiple alternatives, and the less optimalized 

```R
textAreaInput(text_variables[x], # the third variable is the first text variable
                    variable_list$variable_name[which(variable_list$variable== text_variables[x])][1],
                    placeholder = variable_list$description[which(variable_list$variable== text_variables[x])][1]),
```

For text variables. n and x indicates counts of these variables: include one such element for each variable. Note
that text variables and multiple choice variables are counted separately.

### Type of data base
Before you start coding, you have to decide if you want to use MariaDB/MySQL or SQLite. MariaDB/MySQL runs on
a centralized server, while SQLite stores the data locally in the "data" folder (database.db).

SQLite will automatically generate the relevant data base if correctly installed. For MySQL or MariaDB, please
set up a data base separately before connecting.

You will need to install the SQL server of your choice on your computer/server if this is not already done.


## Dependencies
You also need to install the following dependencies:
```R
install.packages("shiny")
install.packages("DBI")
install.packages("readxl") # To read variable_list.xlsx
install.packages("RSQLite") # For SQLite
install.packages("RMySQL") # For RMySQL
```

Only run these commands once per computer.


