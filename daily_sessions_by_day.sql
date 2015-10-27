SELECT summary_date AS date, channel, sum(sessions) AS sessions
FROM warehouse.daily_summaries 
GROUP BY date, channel
ORDER BY date desc
;