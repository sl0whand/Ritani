


tablename="channels"
count_query=paste("SELECT count(*) FROM",tablename)
dbGetQuery(con,count_query)
query1=paste("SELECT * FROM",tablename," LIMIT 100")
channels=dbGetQuery(con,query1)


tablename="channel_touchpoints"
count_query=paste("SELECT count(*) FROM",tablename)
dbGetQuery(con,count_query)
query1=paste("SELECT * FROM",tablename," LIMIT 100")
channel_touchpoints=dbGetQuery(con,query1)


tablename="sessions"
count_query=paste("SELECT count(*) FROM",tablename)
dbGetQuery(con,count_query)
query1=paste("SELECT * FROM",tablename," LIMIT 100")
sessions=dbGetQuery(con,query1)


tablename="clickstreams"
count_query=paste("SELECT count(*) FROM",tablename)
dbGetQuery(con,count_query)
query1=paste("SELECT * FROM",tablename," LIMIT 100")
clickstreams=dbGetQuery(con,query1)

tablename="marketing_spend"
count_query=paste("SELECT count(*) FROM",tablename)
dbGetQuery(con,count_query)
query1=paste("SELECT * FROM",tablename," LIMIT 100")
marketing_spend =dbGetQuery(con,query1)


