----> Centralized Threat Analytics Dashboard

-- Core event table
-- Core event table
CREATE TABLE IF NOT EXISTS security_events (
  id         BIGSERIAL PRIMARY KEY,
  ts         TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  product    TEXT        NOT NULL,          -- e.g., "CrowdStrike", "Suricata"
  severity   TEXT        NOT NULL CHECK (severity IN ('low','medium','high','critical')),
  event_type TEXT        NOT NULL,          -- e.g., "malware", "scan", "login"
  src_ip     INET,
  dest_ip    INET,
  user_name  TEXT,
  message    TEXT
);

-- Bread-and-butter indexes for fast dashboards
CREATE INDEX IF NOT EXISTS idx_events_ts       ON security_events (ts DESC);
CREATE INDEX IF NOT EXISTS idx_events_severity ON security_events (severity);
CREATE INDEX IF NOT EXISTS idx_events_product  ON security_events (product);
CREATE INDEX IF NOT EXISTS idx_events_src_ip   ON security_events (src_ip);

-- A) Timeline: alerts per minute (last 60m)
CREATE OR REPLACE VIEW v_alerts_last_hour AS
SELECT date_trunc('minute', ts) AS minute, COUNT(*) AS total
FROM security_events
WHERE ts >= now() - interval '60 minutes'
GROUP BY 1
ORDER BY 1;

-- B) Severity breakdown (last 60m)
CREATE OR REPLACE VIEW v_severity_last_hour AS
SELECT severity, COUNT(*) AS total
FROM security_events
WHERE ts >= now() - interval '60 minutes'
GROUP BY severity
ORDER BY total DESC;

-- C) Top talkers (last 24h)
CREATE OR REPLACE VIEW v_top_sources AS
SELECT src_ip, COUNT(*) AS events
FROM security_events
WHERE ts >= now() - interval '24 hours'
GROUP BY src_ip
ORDER BY events DESC
LIMIT 10;

-- D) Hot list: latest high/critical
CREATE OR REPLACE VIEW v_recent_critical AS
SELECT id, ts, product, event_type, severity, src_ip, dest_ip, message
FROM security_events
WHERE severity IN ('high','critical')
ORDER BY ts DESC
LIMIT 100;



-- Total alerts (5m)
SELECT COUNT(*) AS alerts_5m
FROM security_events
WHERE ts >= now() - interval '5 minutes';

-- Mean time since last critical
SELECT NOW() - MAX(ts) AS time_since_last_critical
FROM security_events
WHERE severity = 'critical';


---- for the Tableau streets
# pip install fastapi uvicorn psycopg[binary]
from fastapi import FastAPI
import psycopg
app = FastAPI()
DSN = "postgresql://user:pass@localhost:5432/secops"

@app.get("/api/severity")
def severity():
    with psycopg.connect(DSN) as conn:
        rows = conn.execute("SELECT * FROM v_severity_last_hour").fetchall()
    return [{"severity": s, "total": int(t)} for (s, t) in rows]

@app.get("/api/timeline")
def timeline():
    with psycopg.connect(DSN) as conn:
        rows = conn.execute("SELECT minute, total FROM v_alerts_last_hour").fetchall()
    return [{"minute": m.isoformat(), "total": int(t)} for (m, t) in rows]


--Wire it up: point Grafana/Tableau directly at the views, 
--or build a small web page that fetch()es these endpoints and renders charts. 
--You now have a clean, fast, and extensible SQL backbone for your dashboard.










