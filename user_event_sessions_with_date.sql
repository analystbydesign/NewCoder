-- 0) Clean up anything old in the right order
DROP VIEW IF EXISTS user_event_sessions_with_date;
DROP TABLE IF EXISTS user_event_sessions;

-- 1) Create the base table
CREATE TABLE user_event_sessions (
    UserID          BIGINT          NOT NULL,
    SessionID       BIGINT          NOT NULL,
    TimestampUTC    TIMESTAMPTZ     NOT NULL,
    EventDate       DATE            NOT NULL,
    DeviceType      TEXT            NOT NULL,
    AppVersion      TEXT            NOT NULL,
    EventType       TEXT            NOT NULL,
    DurationSeconds INTEGER         NOT NULL,
    EventDetails    JSONB           NULL,
    PRIMARY KEY (UserID, SessionID, TimestampUTC)
);

-- 2) Insert some example rows
INSERT INTO user_event_sessions (
    UserID, SessionID, TimestampUTC, EventDate, DeviceType,
    AppVersion, EventType, DurationSeconds, EventDetails
)
VALUES
(1001, 55501, '2024-01-10 08:15:00+00', '2024-01-10',
 'iPhone 14', '3.2.1', 'AppOpen', 12, '{"screen":"home"}'),

(1001, 55501, '2024-01-10 08:15:12+00', '2024-01-10',
 'iPhone 14', '3.2.1', 'ButtonClick', 3, '{"button":"login"}'),

(2002, 60099, '2024-02-05 19:45:00+00', '2024-02-05',
 'Samsung S22', '5.4.0', 'VideoPlay', 67,
 '{"video_id":3421,"quality":"1080p"}');

-- 3) (Optional) Recreate the view
CREATE VIEW user_event_sessions_with_date AS
SELECT
    UserID,
    SessionID,
    TimestampUTC,
    EventDate,
    DeviceType,
    AppVersion,
    EventType,
    DurationSeconds,
    EventDetails
FROM user_event_sessions;

-- 4) This SELECT is what shows in your data output panel
SELECT *
FROM user_event_sessions
ORDER BY TimestampUTC;


---------------------


-- 0) Clean up old objects (run this first)
DROP VIEW IF EXISTS user_event_sessions_with_date;
DROP TABLE IF EXISTS user_event_sessions;

-- 1) Create the base table
CREATE TABLE user_event_sessions (
    UserID          BIGINT          NOT NULL,
    SessionID       BIGINT          NOT NULL,
    TimestampUTC    TIMESTAMPTZ     NOT NULL,
    EventDate       DATE            NOT NULL,
    DeviceType      TEXT            NOT NULL,
    AppVersion      TEXT            NOT NULL,
    EventType       TEXT            NOT NULL,
    DurationSeconds INTEGER         NOT NULL,
    EventDetails    JSONB           NULL,
    PRIMARY KEY (UserID, SessionID, TimestampUTC)
);

-- 2) Insert monthly data from 2022-01 to 2024-12
--    ~3 events per month → 36 months * 3 = 108 rows (>= 100)

WITH base_months AS (
    -- one timestamp per month (1st of each month at 08:00 UTC)
    SELECT
        gs::timestamptz AS base_ts
    FROM generate_series(
        '2022-01-01'::date,
        '2024-12-01'::date,
        '1 month'
    ) AS gs
),
events AS (
    -- create 3 events per month with slightly different times
    SELECT
        row_number() OVER (ORDER BY base_ts, n) AS rn,
        base_ts + (n * 2 || ' hours')::interval AS ts,  -- 0h, 2h, 4h after base
        n
    FROM base_months
    CROSS JOIN generate_series(0, 2) AS n           -- 3 rows per month
)
INSERT INTO user_event_sessions (
    UserID,
    SessionID,
    TimestampUTC,
    EventDate,
    DeviceType,
    AppVersion,
    EventType,
    DurationSeconds,
    EventDetails
)
SELECT
    1000 + rn                                      AS UserID,
    50000 + rn                                     AS SessionID,
    ts                                             AS TimestampUTC,
    ts::date                                       AS EventDate,
    CASE (rn % 3)
        WHEN 0 THEN 'iPhone 14'
        WHEN 1 THEN 'Samsung S22'
        ELSE 'Pixel 7'
    END                                            AS DeviceType,
    CASE (rn % 3)
        WHEN 0 THEN '3.2.1'
        WHEN 1 THEN '4.0.0'
        ELSE '5.1.2'
    END                                            AS AppVersion,
    CASE n
        WHEN 0 THEN 'AppOpen'
        WHEN 1 THEN 'ButtonClick'
        ELSE 'VideoPlay'
    END                                            AS EventType,
    10 + (rn % 300)                                AS DurationSeconds,
    jsonb_build_object(
        'month', to_char(ts, 'YYYY-MM'),
        'note',  'synthetic demo event'
    )                                              AS EventDetails
FROM events;

-- 3) (Optional) recreate the view for convenience
CREATE VIEW user_event_sessions_with_date AS
SELECT
    UserID,
    SessionID,
    TimestampUTC,
    EventDate,
    DeviceType,
    AppVersion,
    EventType,
    DurationSeconds,
    EventDetails
FROM user_event_sessions;

-- 4) Show the data in your output pane
SELECT *
FROM user_event_sessions
ORDER BY TimestampUTC
LIMIT 200;




















