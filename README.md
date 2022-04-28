# R Coding Interface

R Shiny based hand coding interface. Designed for CJEU related uses, but generalized to work for a number of scenarios.

## Getting started
Four tables need to be updated before you start coding, all of which are located in the **systemdata** folder. These
tables are dependant on being stored in a csv format identical to the one they are presented in by default.

If you are in doubt or editing from excel, save your edited table as a .xlsx file rather than .csv, and make sure the
*readxl* package is installed in R. The script will automatically identify the excel tables and work with these instead.

By default, excel format is used for the *variable_list*, due to its complex nature of containing free text.

#### users.csv
This contains a list of users of the interface. If only one user is needed, provide only one user in the table.

#### config.csv
Configuration files. Use this to change the name of the data base or data table, or to use MySQL/MariaDB instead of
SQLite. MySQL and MariaDB must be used if the app is uploaded to shinyapps.io.

#### delegation.csv
A list of ID numbers and the user names of the users who these cases are assigned to.

#### variable_list.xlsx / variable_list.csv
A list of variable names, descriptions, and possible values.

`variable`: Name of variable (in database)
`variable_name`: Name of variable (for human eyes)
`value`: Potential values - one per line
`interpretation`: Optional description of the variable. R looks for certain keywords such as "multichoice"
`description`: Description of the individual values, as they appear in the interface.
`guide`: Code guide. Will appear above the variable if enabled.
`text` Decide if a text field should be included for description. "yes" or "no", defaults to "no".
`headline`: Section headline. Optional.


### Make changes to interface
After updating the variable list, these changes must be reflected in *interface.R*.

Find the headline in the code named "LIST OF VARIABLES".

Under this headline, add the following line for each unique variable:

```R
radio_survey(n),
```

Where n is a count from 1 to the number of unique variables.

### Type of data base
Before you start coding, you have to decide if you want to use MariaDB/MySQL or SQLite. MariaDB/MySQL runs on
a centralized server, while SQLite stores the data locally in the "data" folder (database.db).

SQLite will automatically generate the relevant data base if correctly installed. For MySQL or MariaDB, please
set up a data base separately before connecting. This data base needs to contain four columns:

1. ID: ID code for coded observations
2. coded_by: user name of hand coders
3. date_updated: `DATETIME`.
4. completed: `BOOL`, preferably `DEFAULT 0`

It could look something like this:

```SQL
create table data_table_name (
  `ID` TEXT,
  `coded_by` TEXT,
  `date_updated` DATETIME default now(),
  `completed` BOOL default 0
);
```


### Run
Make sure the working directory is the home folder of the interface before running.

## Dependencies
You also need to install the following dependencies:
```R
install.packages("shiny")
install.packages("DBI")
install.packages("readxl") # To read variable_list.xlsx
install.packages("RSQLite") # For SQLite
install.packages("RMariaDB") # For MariaDB
```

A working install of MySQL, MariaDB or SQLite is required for the interface to work.


## Contact
Stein Arne Brekke, 2020
stein[dot]brekke[at]eui[dot]eu
