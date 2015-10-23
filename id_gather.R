library(tidyr)
library(dplyr)

source("Ritani_log_analysis_DB_connection.R")
source("Ritani_main_DB_connection.R")
source("Ritani_warehouse_DB_connection.R")


tables=dbListTables(con)

table_ids=NULL
for (tablename in tables) {
  query1=paste("desc",tablename,"")
  desc_tab=dbGetQuery(con,query1)
  table_ids=c(table_ids,desc_tab$Field)
}

table_ids=tbl_df(data.frame(table_ids))
# var_counts=table_ids %>% group_by(table_ids) %>% summarise(count=n()) %>% arrange(desc(count))

# name_counts=table_ids[grep("name",table_ids$table_ids),] %>% group_by(table_ids) %>% 
  # summarise(count=n()) %>% arrange(desc(count))

id_counts=table_ids[grep("id",table_ids$table_ids),] %>% group_by(table_ids) %>% 
  summarise(count=n()) %>% arrange(desc(count))
# pander(id_counts[1:5,])

var_ids=as.character(as.data.frame(id_counts[2:nrow(id_counts),])[,1])
var=var_ids[1]


tbl_query=paste0("
SELECT DISTINCT TABLE_NAME, TABLE_SCHEMA 
FROM INFORMATION_SCHEMA.COLUMNS 
WHERE COLUMN_NAME LIKE '", var, "'
")
relevant_tables=dbGetQuery(con,tbl_query)

tablename=relevant_tables[1,1]


query1=paste("SELECT * FROM",tablename," LIMIT 100")
table_temp=dbGetQuery(con,query1)
names(table_temp)

count_query=paste("SELECT count(*) FROM",tablename)
dbGetQuery(con,count_query)




