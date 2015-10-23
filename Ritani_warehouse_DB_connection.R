library(DBI)
library(RMySQL)
library(dbConnect)

con<-dbConnect(MySQL(),
               user="readonly",
               password="diamonds",
               host="production-read-replica-3.cxg3u5tpgfux.us-east-1.rds.amazonaws.com",
               port=3306,
               dbname="warehouse"
)

