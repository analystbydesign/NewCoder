-- NO POSTGIS REQUIRED
CREATE TABLE IF NOT EXISTS network_kpis (
  id BIGSERIAL PRIMARY KEY,
  timestamp TIMESTAMPTZ NOT NULL,
  lat DOUBLE PRECISION NOT NULL,
  lon DOUBLE PRECISION NOT NULL,
  throughput_mbps DOUBLE PRECISION,
  latency_ms DOUBLE PRECISION,
  packet_loss_pct DOUBLE PRECISION,
  cssr_pct DOUBLE PRECISION,
  fiveg_avail_pct DOUBLE PRECISION
);

CREATE INDEX IF NOT EXISTS idx_kpis_ts ON network_kpis (timestamp);

-- London City Centre Weekly Aggregates
SELECT
  date_trunc('week', timestamp)::date AS week,
  AVG(throughput_mbps)      AS throughput_mbps,
  AVG(latency_ms)           AS latency_ms,
  AVG(packet_loss_pct)      AS packet_loss_pct,
  AVG(cssr_pct)             AS cssr_pct,
  AVG(fiveg_avail_pct)      AS fiveg_avail_pct
FROM network_kpis
WHERE timestamp >= '2024-01-01' AND timestamp < '2025-01-01'
  AND lon BETWEEN -0.17 AND -0.06
  AND lat BETWEEN 51.49 AND 51.54
GROUP BY 1
ORDER BY 1;

-- ==========================================================
-- 1) OPTIONAL: start clean (uncomment if you want a fresh fill)
-- ==========================================================
-- TRUNCATE TABLE network_kpis;

-- ==========================================================
-- 2) Seed the table with synthetic 2024 London city-centre data
--    - BBox: lon [-0.17, -0.06], lat [51.49, 51.54]
--    - N_ROWS: adjust as needed (e.g., 50_000)
--    - Light weekly/monthly seasonality + noise
-- ==========================================================
-- Make random() reproducible for repeatable fills (optional)
SELECT setseed(0.42);

WITH params AS (
  SELECT
    50000::int                         AS n_rows,           -- <<< change volume here
    timestamp '2024-01-01 00:00:00'    AS start_ts,
    interval  '365 days'               AS span,
    -0.17::double precision            AS lon_min,
    -0.06::double precision            AS lon_max,
    51.49::double precision            AS lat_min,
    51.54::double precision            AS lat_max
),
seq AS (
  SELECT generate_series(1, (SELECT n_rows FROM params)) AS i
),
samples AS (
  SELECT
    -- uniform timestamp across 2024 (+ random minutes)
    (p.start_ts + (random() * p.span))::timestamptz                          AS ts,
    -- uniform lat/lon inside bbox
    (p.lat_min + random() * (p.lat_max - p.lat_min))                         AS lat,
    (p.lon_min + random() * (p.lon_max - p.lon_min))                         AS lon
  FROM seq s
  CROSS JOIN params p
),
features AS (
  SELECT
    ts, lat, lon,
    EXTRACT(DOW   FROM ts) AS dow,     -- 0=Sun..6=Sat
    EXTRACT(MONTH FROM ts) AS mon
  FROM samples
),
kpis AS (
  SELECT
    ts AS timestamp,
    lat,
    lon,

    -- Throughput (Mbps): base ~180, mild weekly/monthly seasonality + noise
    LEAST(400, GREATEST(40,
      180
      + 10 * sin(2*pi()*(dow/7.0))
      +  6 * sin(2*pi()*(mon/12.0))
      - (random()-0.5) * 16
    ))::double precision                                   AS throughput_mbps,

    -- Latency (ms): base ~18, inverse-ish to throughput pattern + noise
    LEAST(80, GREATEST(8,
      18
      + 2 * cos(2*pi()*(dow/7.0))
      + 1.5 * sin(2*pi()*(mon/12.0))
      + (random()-0.5) * 6
    ))::double precision                                   AS latency_ms,

    -- Packet loss (%): small values with gentle seasonality
    LEAST(2.5, GREATEST(0,
      0.25
      + 0.15 * cos(2*pi()*(dow/7.0))
      + 0.10 * sin(2*pi()*(mon/12.0))
      + (random()-0.5) * 0.24
    ))::double precision                                   AS packet_loss_pct,

    -- CSSR (%): high, small variability
    LEAST(100, GREATEST(94,
      98.6
      - 0.2  * sin(2*pi()*(dow/7.0))
      - 0.15 * sin(2*pi()*(mon/12.0))
      - (random()-0.5) * 0.6
    ))::double precision                                   AS cssr_pct,

    -- 5G availability (%): moderate-high, weekly/monthly pattern
    LEAST(99, GREATEST(40,
      78
      + 4 * sin(2*pi()*(mon/12.0))
      + 2 * sin(2*pi()*(dow/7.0))
      + (random()-0.5) * 4.4
    ))::double precision                                   AS fiveg_avail_pct

  FROM features
)
INSERT INTO network_kpis
(timestamp, lat, lon, throughput_mbps, latency_ms, packet_loss_pct, cssr_pct, fiveg_avail_pct)
SELECT
  timestamp, lat, lon, throughput_mbps, latency_ms, packet_loss_pct, cssr_pct, fiveg_avail_pct
FROM kpis;


-- Verify fill and London 2024 constraints
SELECT COUNT(*) AS rows_inserted FROM network_kpis;

SELECT
  MIN(timestamp) AS min_ts,
  MAX(timestamp) AS max_ts,
  MIN(lat) AS min_lat, MAX(lat) AS max_lat,
  MIN(lon) AS min_lon, MAX(lon) AS max_lon
FROM network_kpis;


-- Weekly Aggregates (London city centre, 2024)
SELECT
  date_trunc('week', timestamp)::date AS week,
  AVG(throughput_mbps)      AS throughput_mbps,
  AVG(latency_ms)           AS latency_ms,
  AVG(packet_loss_pct)      AS packet_loss_pct,
  AVG(cssr_pct)             AS cssr_pct,
  AVG(fiveg_avail_pct)      AS fiveg_avail_pct
FROM network_kpis
WHERE timestamp >= '2024-01-01' AND timestamp < '2025-01-01'
  AND lon BETWEEN -0.17 AND -0.06
  AND lat BETWEEN 51.49 AND 51.54
GROUP BY 1
ORDER BY 1;



