
source("Ritani_log_analysis_DB_connection.R")
tables=dbListTables(con)


##touchpoints tables
grep_inds=grep("touchpoints",tables)
touchpoint_tables=tables[grep_inds]

tablename=touchpoint_tables[2]
query1=paste("desc",tablename,"")
desc_tab=dbGetQuery(con,query1)

count_query=paste("SELECT count(*) FROM",tablename)
dbGetQuery(con,count_query)
query1=paste("SELECT * FROM",tablename," LIMIT 10")
table_temp=dbGetQuery(con,query1)


##sessions tables
grep_inds=grep("session",tables)
sessions_tables=tables[grep_inds]

tablename=sessions_tables[5]
query1=paste("desc",tablename,"")
desc_tab=dbGetQuery(con,query1)

count_query=paste("SELECT count(*) FROM",tablename)
dbGetQuery(con,count_query)
query1=paste("SELECT * FROM",tablename," LIMIT 10")
table_temp=dbGetQuery(con,query1)

##channel tables
grep_inds=grep("channel",tables)
channel_tables=tables[grep_inds]

tablename=channel_tables[1]
query1=paste("desc",tablename,"")
desc_tab=dbGetQuery(con,query1)

count_query=paste("SELECT count(*) FROM",tablename)
dbGetQuery(con,count_query)
query1=paste("SELECT * FROM",tablename," LIMIT 10")
table_temp=dbGetQuery(con,query1)





