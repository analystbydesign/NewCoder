-- GDP by year and country
CREATE TABLE gdp (
    country VARCHAR(50),
    year INT,
    gdp_usd_billions DECIMAL(10,2)
);

-- Inflation rates by year and country
CREATE TABLE inflation (
    country VARCHAR(50),
    year INT,
    inflation_rate DECIMAL(5,2) -- percentage
);

-- Unemployment data
CREATE TABLE unemployment (
    country VARCHAR(50),
    year INT,
    unemployment_rate DECIMAL(5,2) -- percentage
);

INSERT INTO gdp VALUES
('UK', 2020, 2700.00),
('UK', 2021, 3120.50),
('UK', 2022, 3275.40),
('USA', 2020, 21433.20),
('USA', 2021, 22965.00),
('USA', 2022, 25464.80),
('India', 2020, 2626.00),
('India', 2021, 3170.30),
('India', 2022, 3380.70);

INSERT INTO inflation VALUES
('UK', 2020, 0.90),
('UK', 2021, 2.50),
('UK', 2022, 9.10),
('USA', 2020, 1.20),
('USA', 2021, 7.00),
('USA', 2022, 6.50),
('India', 2020, 6.60),
('India', 2021, 5.10),
('India', 2022, 6.70);

INSERT INTO unemployment VALUES
('UK', 2020, 4.8),
('UK', 2021, 4.5),
('UK', 2022, 3.7),
('USA', 2020, 8.1),
('USA', 2021, 5.3),
('USA', 2022, 3.6),
('India', 2020, 7.1),
('India', 2021, 6.2),
('India', 2022, 5.8);

SELECT 
    country,
    year,
    gdp_usd_billions,
    LAG(gdp_usd_billions) OVER (PARTITION BY country ORDER BY year) AS prev_year_gdp,
    ROUND(
        ((gdp_usd_billions - LAG(gdp_usd_billions) OVER (PARTITION BY country ORDER BY year))
        / LAG(gdp_usd_billions) OVER (PARTITION BY country ORDER BY year)) * 100, 2
    ) AS gdp_growth_percent
FROM gdp
ORDER BY country, year;

SELECT country, year, inflation_rate
FROM inflation 
WHERE inflation_rate > 6
ORDER BY inflation_rate DESC;

WITH growth AS (
	Select
		country,
		year, 
		((gdp_usd_billions - LAG(gdp_usd_billions) OVER (PARTITION BY country ORDER BY year))
		/ LAG(gdp_usd_billions) OVER (PARTITION BY country ORDER BY year))	* 100 AS growth_rate
	FROM gdp
)
SELECT country, ROUND(AVG(growth_rate), 2) AS avg_growth_rate
FROM growth
GROUP BY country
ORDER BY avg_growth_rate DESC;


SELECT 
    g.country,
    g.year,
    g.gdp_usd_billions,
    u.unemployment_rate,
    ROUND(g.gdp_usd_billions / u.unemployment_rate, 2) AS efficiency_index
FROM gdp g
JOIN unemployment u ON g.country = u.country AND g.year = u.year
WHERE g.year = 2022
ORDER BY efficiency_index DESC;














