# CJEU Coding Interface

Before you start coding, you have to decide if you want to use MariaDB/MySQL or SQLite. MariaDB/MySQL runs on
a centralized server, while SQLite stores the data locally in the "data" folder (database.db).

SQLite will automatically generate the relevant data base if correctly installed. For MySQL or MariaDB, please
set up a data base separately before connecting.

You will need to install the SQL server of your choice on your computer/server if this is not already done.


Remember to update systemdata/users.csv, systemdata/config.csv and systemdata/delegation.csv before


## Dependencies
You also need to install the following dependencies:
```R
install.packages("shiny")
install.packages("DBI")
install.packages("RMySQL") # For RMySQL
install.packages("RSQLite") # For SQLite
```

Only run these commands once per computer.


