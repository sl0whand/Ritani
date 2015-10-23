library(DBI)
library(RMySQL)
library(dbConnect)

con<-dbConnect(MySQL(),
               user="attrread",
               password="diamonds",
               host="attribution.cxg3u5tpgfux.us-east-1.rds.amazonaws.com",
               port=3306,
               dbname="log_analysis"
)


# tables=dbListTables(con)
# tablename=tables[1]
# query1=paste0("SHOW TABLES LIKE '",tablename,"'")
# dbGetQuery(con,query1)

# dbWriteTable(con, "tablename", dataframe, overwrite = TRUE)
# query1=paste("SELECT COUNT(*) FROM",tablename)
# dbGetQuery(con,query1)

# data=dbReadTable(con, tablename)

# dbRemoveTable(con, " tablename ")
# dbDisconnect(con)
