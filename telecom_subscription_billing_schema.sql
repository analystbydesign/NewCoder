


-- =========================================================
-- Telecommunications Subscription & Billing System
-- SQL Schema + Seed Data for 1,000 Customers
-- =========================================================

-- Drop tables in dependency order (if re-running)
DROP TABLE IF EXISTS payments;
DROP TABLE IF EXISTS bill_items;
DROP TABLE IF EXISTS bills;
DROP TABLE IF EXISTS usage_sessions;
DROP TABLE IF EXISTS subscription_addons;
DROP TABLE IF EXISTS subscriptions;
DROP TABLE IF EXISTS addons;
DROP TABLE IF EXISTS plans;
DROP TABLE IF EXISTS customers;

-- ====================
-- SCHEMA DEFINITION
-- ====================

-- 1. Customers
CREATE TABLE customers (
    customer_id     SERIAL PRIMARY KEY,
    first_name      VARCHAR(100) NOT NULL,
    last_name       VARCHAR(100) NOT NULL,
    email           VARCHAR(255) NOT NULL UNIQUE,
    phone_number    VARCHAR(30)  NOT NULL,
    billing_address TEXT,
    created_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- 2. Plans
CREATE TABLE plans (
    plan_id                 SERIAL PRIMARY KEY,
    plan_name               VARCHAR(150) NOT NULL,
    monthly_fee             DECIMAL(10,2) NOT NULL CHECK (monthly_fee >= 0),
    voice_minutes_included  INT NOT NULL DEFAULT 0,
    sms_included            INT NOT NULL DEFAULT 0,
    data_mb_included        INT NOT NULL DEFAULT 0,
    is_active               BOOLEAN NOT NULL DEFAULT TRUE
);

-- 3. Addons
CREATE TABLE addons (
    addon_id    SERIAL PRIMARY KEY,
    addon_name  VARCHAR(150) NOT NULL,
    addon_type  VARCHAR(50)  NOT NULL, -- e.g. 'extra_data', 'intl_calls'
    monthly_fee DECIMAL(10,2) NOT NULL CHECK (monthly_fee >= 0),
    is_active   BOOLEAN NOT NULL DEFAULT TRUE
);

-- 4. Subscriptions
CREATE TABLE subscriptions (
    subscription_id SERIAL PRIMARY KEY,
    customer_id     INT NOT NULL,
    plan_id         INT NOT NULL,
    start_date      DATE NOT NULL,
    end_date        DATE,
    status          VARCHAR(20) NOT NULL
        CHECK (status IN ('active','suspended','cancelled')),
    created_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT fk_sub_customer FOREIGN KEY (customer_id)
        REFERENCES customers(customer_id),
    CONSTRAINT fk_sub_plan FOREIGN KEY (plan_id)
        REFERENCES plans(plan_id)
);

-- 5. Subscription Addons (junction table)
CREATE TABLE subscription_addons (
    subscription_id INT NOT NULL,
    addon_id        INT NOT NULL,
    start_date      DATE NOT NULL,
    end_date        DATE,
    PRIMARY KEY (subscription_id, addon_id, start_date),
    CONSTRAINT fk_sa_subscription FOREIGN KEY (subscription_id)
        REFERENCES subscriptions(subscription_id) ON DELETE CASCADE,
    CONSTRAINT fk_sa_addon FOREIGN KEY (addon_id)
        REFERENCES addons(addon_id)
);

-- 6. Usage Sessions
CREATE TABLE usage_sessions (
    usage_id        SERIAL PRIMARY KEY,
    subscription_id INT NOT NULL,
    usage_date_time TIMESTAMP NOT NULL,
    usage_type      VARCHAR(10) NOT NULL
        CHECK (usage_type IN ('call','sms','data')),
    quantity        INT NOT NULL CHECK (quantity >= 0),
    destination     VARCHAR(255),
    CONSTRAINT fk_usage_subscription FOREIGN KEY (subscription_id)
        REFERENCES subscriptions(subscription_id) ON DELETE CASCADE
);

-- 7. Bills
CREATE TABLE bills (
    bill_id              SERIAL PRIMARY KEY,
    subscription_id      INT NOT NULL,
    billing_period_start DATE NOT NULL,
    billing_period_end   DATE NOT NULL,
    issue_date           DATE NOT NULL,
    due_date             DATE NOT NULL,
    total_amount         DECIMAL(10,2) NOT NULL CHECK (total_amount >= 0),
    status               VARCHAR(20) NOT NULL
        CHECK (status IN ('unpaid','paid','overdue')),
    CONSTRAINT fk_bill_subscription FOREIGN KEY (subscription_id)
        REFERENCES subscriptions(subscription_id)
);

-- 8. Bill Items
CREATE TABLE bill_items (
    bill_item_id SERIAL PRIMARY KEY,
    bill_id      INT NOT NULL,
    item_type    VARCHAR(20) NOT NULL
        CHECK (item_type IN ('base_plan','addon','overage','discount','tax')),
    description  VARCHAR(255) NOT NULL,
    amount       DECIMAL(10,2) NOT NULL,
    CONSTRAINT fk_billitem_bill FOREIGN KEY (bill_id)
        REFERENCES bills(bill_id) ON DELETE CASCADE
);

-- 9. Payments
CREATE TABLE payments (
    payment_id   SERIAL PRIMARY KEY,
    bill_id      INT NOT NULL,
    payment_date TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    amount       DECIMAL(10,2) NOT NULL CHECK (amount > 0),
    method       VARCHAR(30) NOT NULL, -- 'card','bank','wallet',...
    CONSTRAINT fk_payment_bill FOREIGN KEY (bill_id)
        REFERENCES bills(bill_id) ON DELETE CASCADE
);

-- ====================
-- SEED DATA
-- ====================

-- Base plans
INSERT INTO plans (plan_name, monthly_fee, voice_minutes_included, sms_included, data_mb_included)
VALUES
('Basic 5GB',        25.00, 300,   200,   5120),
('Standard 20GB',    40.00, 1000,  1000, 20480),
('Unlimited Max',    60.00, 10000, 5000, 51200);

-- Addons
INSERT INTO addons (addon_name, addon_type, monthly_fee)
VALUES
('Extra 2GB Data',          'extra_data',  8.00),
('International Calls Pack','intl_calls', 15.00),
('Social Media Booster',    'extra_data',  5.00);

-- 1,000 customers
INSERT INTO customers (first_name, last_name, email, phone_number, billing_address)
SELECT
    'User' || i,
    'Demo' || i,
    'user' || i || '@example.com',
    '555-' || LPAD(i::text, 4, '0'),
    'Address ' || i || ', Main Street'
FROM generate_series(1, 1000) AS s(i);

-- One subscription per customer, random plan, random start date in last year
INSERT INTO subscriptions (customer_id, plan_id, start_date, status)
SELECT
    c.customer_id,
    (SELECT plan_id FROM plans ORDER BY random() LIMIT 1),
    (CURRENT_DATE - (floor(random() * 365))::int),
    'active'
FROM customers c;

-- Some random usage sessions (about 5,000 rows)
INSERT INTO usage_sessions (subscription_id, usage_date_time, usage_type, quantity, destination)
SELECT
    s.subscription_id,
    NOW() - (floor(random() * 60)) * INTERVAL '1 day',
    (ARRAY['call','sms','data'])[1 + floor(random()*3)::int],
    1 + floor(random()*500)::int,
    CASE WHEN random() < 0.5 THEN '555-' || LPAD((1 + floor(random()*9999))::text, 4, '0') ELSE NULL END
FROM subscriptions s
CROSS JOIN LATERAL generate_series(1,5) g;  -- ~5 usage records per subscription
