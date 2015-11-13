
SELECT order_number, name AS channel, vendor, state,
 max(total) AS revenue, sum(vendor_views) AS session_count
FROM (
		SELECT channel_id, clickstream_id, session_id
		FROM channel_touchpoints
	) ct INNER JOIN (
		SELECT id, name, has_attributed_revenue
		FROM channels
        WHERE has_attributed_revenue=1
	) ch ON ct.channel_id=ch.id
    INNER JOIN (
		SELECT id, pre_purchase
		FROM sessions
        WHERE pre_purchase=1
   ) se ON ct.session_id=se.id
	INNER JOIN (
--     revenue
		SELECT id, order_number, total, state
		FROM clickstreams
    ) cl ON ct.clickstream_id=cl.id
	INNER JOIN (
--    ad vendor information
		SELECT clickstream_id, vendor, count(*) AS vendor_views
		FROM display_impressions
        GROUP BY clickstream_id, vendor
        HAVING vendor IN (
						SELECT vendor 
						FROM (
							SELECT vendor, count(*) AS count
							FROM display_impressions
							GROUP BY  vendor
							HAVING count>150
							ORDER BY count DESC
						) dm
						)
    ) di ON ct.clickstream_id=di.clickstream_id
GROUP BY order_number, channel, vendor
HAVING state LIKE '%shipped%' OR state LIKE '%accepted%' OR state LIKE '%delivered%'
		OR state LIKE '%store%' OR state LIKE '%charged%' OR state LIKE '%authorized%'
        OR state LIKE '%shipped%'

;


SELECT order_number, name AS channel, state,
 max(total) AS revenue, count(*) as session_count
FROM (
		SELECT channel_id, clickstream_id, session_id
		FROM channel_touchpoints
	) ct INNER JOIN (
		SELECT id, name, has_attributed_revenue
		FROM channels
	) ch ON ct.channel_id=ch.id
    INNER JOIN (
		SELECT id, pre_purchase
		FROM sessions
        WHERE pre_purchase=1
   ) se ON ct.session_id=se.id
	INNER JOIN (
		SELECT id, order_number, total, state
		FROM clickstreams
    ) cl ON ct.clickstream_id=cl.id
GROUP BY order_number, channel
HAVING state LIKE '%shipped%' OR state LIKE '%accepted%' OR state LIKE '%delivered%'
		OR state LIKE '%store%' OR state LIKE '%charged%' OR state LIKE '%authorized%'
        OR state LIKE '%shipped%'

;

