
# set up cycling database and connection

# this will connect to DB instance w/o specifying database

con <- DBI::dbConnect(
  RMariaDB::MariaDB(), 
  user = Sys.getenv("MYSQL_U"), 
  password = Sys.getenv("MYSQL_P"), 
  db = "", 
  host = "p-c-database.cqrwmwxsn1fv.us-east-2.rds.amazonaws.com")

# now create the database cycling

dbSendQuery(con, "CREATE DATABASE IF NOT EXISTS cycling")

#

dbSendQuery(con, "CREATE DATABASE IF NOT EXISTS golf_dfs")


