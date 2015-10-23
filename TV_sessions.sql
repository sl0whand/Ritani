SELECT date, name, sum(attributed_revenue) AS revenue, count(*) as sessions
FROM (
SELECT DATE(start_time) as date, attributed_revenue, name
FROM (
	SELECT *
	FROM (
		SELECT id, start_time, attributed_revenue
		FROM sessions	
	) s_sub JOIN (
		SELECT session_id, channel_id
		FROM channel_touchpoints
	) ct_sub ON s_sub.id=ct_sub.session_id
) cts_sub JOIN (
	SELECT id, channels.name, has_attributed_revenue
	FROM channels
) c_sub ON cts_sub.channel_id=c_sub.id
WHERE has_attributed_revenue >0
) pen_table
GROUP BY date, name
ORDER BY date DESC
;