SELECT summary_date AS date, channel, sum(sessions) AS sessions
FROM warehouse.daily_summaries 
GROUP BY summary_date, channel
ORDER BY summary_date desc
;