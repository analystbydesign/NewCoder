
CREATE TABLE public.login_attempts (
  attempt_id   SERIAL PRIMARY KEY,
  username     VARCHAR(50) NOT NULL,
  ip_address   INET NOT NULL,
  success      BOOLEAN NOT NULL,
  attempt_time TIMESTAMP DEFAULT NOW()
);

-- 2) Seed some test data
INSERT INTO public.login_attempts (username, ip_address, success) VALUES
('admin','45.155.205.2',false),
('admin','45.155.205.2',false),
('admin','45.155.205.2',false),
('admin','45.155.205.2',false),
('admin','45.155.205.2',false),
('admin','45.155.205.2',false),
('guest','102.44.1.7',true);

-- 3) Your brute-force detector
SELECT username, ip_address, COUNT(*) AS failed_attempts
FROM public.login_attempts
WHERE success = false
GROUP BY username, ip_address
HAVING COUNT(*) > 5;

-- 1) Helpful indexes for speed (by user/time and IP/time)
CREATE INDEX IF NOT EXISTS idx_login_attempts_user_time
  ON public.login_attempts (username, attempt_time DESC);
CREATE INDEX IF NOT EXISTS idx_login_attempts_ip_time
  ON public.login_attempts (ip_address, attempt_time DESC);
CREATE INDEX IF NOT EXISTS idx_login_attempts_success_time
  ON public.login_attempts (success, attempt_time DESC);

-- 2) Aggregate view: failed attempts per user/IP in last 5 minutes
CREATE OR REPLACE VIEW public.failed_attempts_5min AS
SELECT
  username,
  ip_address,
  COUNT(*)::int AS failures_5min,
  MIN(attempt_time) AS first_seen_window,
  MAX(attempt_time) AS last_seen_window
FROM public.login_attempts
WHERE success = false
  AND attempt_time >= now() - interval '5 minutes'
GROUP BY username, ip_address;


-- 3) Alerts table: what we’ll raise when threshold is crossed
CREATE TABLE IF NOT EXISTS public.security_alerts (
  alert_id       BIGSERIAL PRIMARY KEY,
  alert_type     TEXT NOT NULL,                       -- e.g., 'BRUTE_FORCE'
  username       VARCHAR(50),
  ip_address     INET,
  failure_count  INT NOT NULL,
  window_start   TIMESTAMP NOT NULL,
  window_end     TIMESTAMP NOT NULL,
  created_at     TIMESTAMP NOT NULL DEFAULT now()
);


-- 4) Trigger function: on each insert, check window & raise an alert once
CREATE OR REPLACE FUNCTION public.raise_bruteforce_alert()
RETURNS TRIGGER AS $$
DECLARE
  v_failures INT;
  v_win_start TIMESTAMP := now() - interval '5 minutes';
  v_win_end   TIMESTAMP := now();
BEGIN
  -- Only care about failed logins
  IF NEW.success THEN
    RETURN NEW;
  END IF;

-- 0) Clean slate: remove any old trigger/function
DROP TRIGGER IF EXISTS trg_bruteforce_alert ON public.login_attempts;
DROP FUNCTION IF EXISTS public.raise_bruteforce_alert();

-- 1) Recreate alerts table (safe if already exists)
CREATE TABLE IF NOT EXISTS public.security_alerts (
  alert_id       BIGSERIAL PRIMARY KEY,
  alert_type     TEXT NOT NULL,
  username       VARCHAR(50),
  ip_address     INET,
  failure_count  INT NOT NULL,
  window_start   TIMESTAMP NOT NULL,
  window_end     TIMESTAMP NOT NULL,
  created_at     TIMESTAMP NOT NULL DEFAULT now()
);


-- 2) ✅ Correct PL/pgSQL trigger function (note LANGUAGE plpgsql)
CREATE OR REPLACE FUNCTION public.raise_bruteforce_alert()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $func$
DECLARE
  v_failures   INT;
  v_win_start  TIMESTAMP := now() - interval '5 minutes';
  v_win_end    TIMESTAMP := now();
BEGIN
  -- Only act on failed attempts
  IF NEW.success THEN
    RETURN NEW;
  END IF;


_____

DROP FUNCTION IF EXISTS public.raise_bruteforce_alert();

CREATE OR REPLACE FUNCTION public.raise_bruteforce_alert()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $func$
DECLARE
  v_failures   INT;
  v_win_start  TIMESTAMP := now() - interval '5 minutes';
  v_win_end    TIMESTAMP := now();
BEGIN
  -- Only act on failed attempts
  IF NEW.success THEN
    RETURN NEW;
  END IF;

  -- Count recent failures for same user+IP in the last 5 minutes
  SELECT COUNT(*) INTO v_failures
  FROM public.login_attempts la
  WHERE la.success = false
    AND la.username = NEW.username
    AND la.ip_address = NEW.ip_address
    AND la.attempt_time >= v_win_start;

  -- If threshold reached (>=5), raise alert once per 5-min window
  IF v_failures >= 5 THEN
    INSERT INTO public.security_alerts
      (alert_type, username, ip_address, failure_count, window_start, window_end)
    SELECT 'BRUTE_FORCE', NEW.username, NEW.ip_address, v_failures, v_win_start, v_win_end
    WHERE NOT EXISTS (
      SELECT 1
      FROM public.security_alerts a
      WHERE a.alert_type = 'BRUTE_FORCE'
        AND a.username  = NEW.username
        AND a.ip_address = NEW.ip_address
        AND a.created_at >= now() - interval '5 minutes'
    );
  END IF;

  RETURN NEW;
END;
$func$;



DROP TRIGGER IF EXISTS trg_bruteforce_alert ON public.login_attempts;

CREATE TRIGGER trg_bruteforce_alert
AFTER INSERT ON public.login_attempts
FOR EACH ROW
EXECUTE FUNCTION public.raise_bruteforce_alert();


INSERT INTO public.login_attempts (username, ip_address, success)
VALUES
('bob','203.0.113.10',false),
('bob','203.0.113.10',false),
('bob','203.0.113.10',false),
('bob','203.0.113.10',false),
('bob','203.0.113.10',false);

SELECT * FROM public.security_alerts ORDER BY created_at DESC;






























