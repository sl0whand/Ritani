SELECT channel_id, clickstream_id, session_id, pre_purchase
FROM channel_touchpoints
;

SELECT id, name
FROM channels
;

SELECT id, pre_purchase
FROM sessions
;

SELECT id, order_number, user_id, total, state
FROM clickstreams
;

SELECT clickstream_id, impression_type, convertro_impressions_id
FROM display_impressions
;

