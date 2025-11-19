----- Focus: Data Session Records for mobile session for MB used, device, cell ID, start/end time


CREATE TABLE data_session_records1 (
    user_id           BIGINT        NOT NULL,
    device_model      VARCHAR(100)  NOT NULL,
    operating_system  VARCHAR(50)   NOT NULL,
    device_id         VARCHAR(100)  NOT NULL,
    cell_id           VARCHAR(50)   NOT NULL,
    start_time        TIMESTAMP     NOT NULL,
    end_time          TIMESTAMP     NOT NULL,
    data_mb           DECIMAL(10,2) NOT NULL,
    duration_seconds  INT           NOT NULL
);

SELECT
    user_id,
    device_model,
    operating_system,
    device_id,
    cell_id,
    start_time,
    end_time,
    data_mb,
    EXTRACT(EPOCH FROM (end_time - start_time)) AS duration_seconds
FROM data_session_records1;


INSERT INTO data_session_records1 (
    user_id,
    device_model,
    operating_system,
    device_id,
    cell_id,
    start_time,
    end_time,
    data_mb,
    duration_seconds
)
SELECT
    user_id,
    device_model,
    operating_system,
    device_id,
    cell_id,
    start_time,
    end_time,
    data_mb,
    EXTRACT(EPOCH FROM (end_time - start_time))::INT AS duration_seconds
FROM (
    VALUES
    (1001, 'iPhone 14', 'iOS', 'DEV-IPH-001', 'CELL-7832',
     TIMESTAMP '2025-11-18 09:15:00', TIMESTAMP '2025-11-18 09:37:30', 125.4),

    (1002, 'Galaxy S23', 'Android', 'DEV-SAM-342', 'CELL-7832',
     TIMESTAMP '2025-11-18 12:05:10', TIMESTAMP '2025-11-18 12:20:40', 89.7),

    (1003, 'Pixel 8', 'Android', 'DEV-PXL-887', 'CELL-4561',
     TIMESTAMP '2025-11-18 18:45:00', TIMESTAMP '2025-11-18 19:10:15', 210.2),

    (1001, 'iPhone 14', 'iOS', 'DEV-IPH-001', 'CELL-7832',
     TIMESTAMP '2025-11-19 08:00:00', TIMESTAMP '2025-11-19 08:05:00', 12.8),

    (1004, 'iPhone 13', 'iOS', 'DEV-IPH-144', 'CELL-9021',
     TIMESTAMP '2025-11-19 13:27:10', TIMESTAMP '2025-11-19 13:55:40', 340.0),

    (1005, 'Galaxy A54', 'Android', 'DEV-SAM-998', 'CELL-9021',
     TIMESTAMP '2025-11-19 21:10:05', TIMESTAMP '2025-11-19 21:45:05', 57.6),

    (1006, 'Xiaomi 13', 'Android', 'DEV-XMI-555', 'CELL-3110',
     TIMESTAMP '2025-11-20 10:10:00', TIMESTAMP '2025-11-20 10:12:30', 6.3),

    (1002, 'Galaxy S23', 'Android', 'DEV-SAM-342', 'CELL-4561',
     TIMESTAMP '2025-11-20 23:59:00', TIMESTAMP '2025-11-21 00:10:30', 44.9)
) AS v (
    user_id,
    device_model,
    operating_system,
    device_id,
    cell_id,
    start_time,
    end_time,
    data_mb
);


-- 4. Output the fully loaded table
SELECT
    user_id,
    device_model,
    operating_system,
    device_id,
    cell_id,
    start_time,
    end_time,
    data_mb,
    duration_seconds
FROM data_session_records1
ORDER BY start_time;

--- I wanted to create a longer time-period
INSERT INTO data_session_records1
SELECT
    (1000 + s.id) AS user_id,
    (ARRAY['iPhone 6','iPhone 7','iPhone X','iPhone 12','iPhone 14',
           'Galaxy S6','Galaxy S9','Galaxy S20','Galaxy S23',
           'Pixel 2','Pixel 4','Pixel 6','Pixel 8'])[floor(random()*13)+1] AS device_model,
    (ARRAY['iOS','Android'])[floor(random()*2)+1] AS operating_system,
    'DEV-' || floor(random()*900000 + 100000)::TEXT AS device_id,
    'CELL-' || floor(random()*9000 + 1000)::TEXT AS cell_id,
    ts AS start_time,
    ts + (interval '1 second' * floor(random()*3600)) AS end_time,
    round((random()*500)::numeric, 2) AS data_mb,
    EXTRACT(EPOCH FROM ((ts + (interval '1 second' * floor(random()*3600))) - ts))::INT AS duration_seconds
FROM (
    SELECT id,
           timestamp '2014-01-01' +
           (random() * (extract(epoch FROM timestamp '2024-12-31') -
                        extract(epoch FROM timestamp '2014-01-01'))) * interval '1 second' AS ts
    FROM generate_series(1,1000) AS id
) s;

SELECT
    user_id,
    device_model,
    operating_system,
    device_id,
    cell_id,
    start_time,
    end_time,
    data_mb,
    duration_seconds
FROM data_session_records1
ORDER BY start_time;


























