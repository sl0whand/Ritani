library(dplyr)

tables=dbListTables(con)

table_ids=NULL
for (tablename in tables) {
  query1=paste("desc",tablename,"")
  desc_tab=dbGetQuery(con,query1)
  table_ids=c(table_ids,desc_tab$Field)
}

table_ids=tbl_df(data.frame(table_ids))
var_counts=table_ids %>% group_by(table_ids) %>% summarise(count=n()) %>% arrange(desc(count))
id_counts=table_ids[grep("id",table_ids$table_ids),] %>% group_by(table_ids) %>% summarise(count=n()) %>% arrange(desc(count))
id_counts[1:20,]


# 
# SELECT DISTINCT TABLE_NAME 
# FROM INFORMATION_SCHEMA.COLUMNS
# WHERE COLUMN_NAME LIKE 'channel_id'
# AND TABLE_SCHEMA='log_analysis'
# ;