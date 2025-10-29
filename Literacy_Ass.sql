-- Remove all existing rows (optional, only if you want to replace data)
TRUNCATE TABLE literacy_assessments;

-- Reinsert the data from the example
INSERT INTO literacy_assessments (assessment_id, inmate_id, assessment_year, literacy_score, passed)
VALUES
  (1, 1, 2015, 55.20, false),
  (2, 1, 2018, 68.90, true),
  (3, 1, 2021, 72.30, true),
  (4, 2, 2016, 61.50, true),
  (5, 2, 2020, 70.10, true),
  (6, 2, 2023, 74.50, true),
  (7, 3, 2015, 40.00, false),
  (8, 3, 2019, 58.00, false),
  (9, 3, 2022, 66.50, true),
  (10, 4, 2017, 62.00, true),
  (11, 4, 2021, 70.80, true),
  (12, 5, 2015, 48.50, false),
  (13, 5, 2019, 65.50, true),
  (14, 6, 2018, 59.40, false),
  (15, 6, 2020, 63.20, true),
  (16, 6, 2023, 69.90, true);

------
------

DROP TABLE IF EXISTS literacy_assessments;

CREATE TABLE literacy_assessments (
  assessment_id   SERIAL PRIMARY KEY,
  inmate_id       INTEGER NOT NULL,
  assessment_year INTEGER NOT NULL,
  literacy_score  NUMERIC(5,2),
  passed          BOOLEAN
);

INSERT INTO literacy_assessments (assessment_id, inmate_id, assessment_year, literacy_score, passed)
VALUES
  (1, 1, 2015, 55.20, false),
  (2, 1, 2018, 68.90, true),
  (3, 1, 2021, 72.30, true),
  (4, 2, 2016, 61.50, true),
  (5, 2, 2020, 70.10, true),
  (6, 2, 2023, 74.50, true),
  (7, 3, 2015, 40.00, false),
  (8, 3, 2019, 58.00, false),
  (9, 3, 2022, 66.50, true),
  (10, 4, 2017, 62.00, true),
  (11, 4, 2021, 70.80, true),
  (12, 5, 2015, 48.50, false),
  (13, 5, 2019, 65.50, true),
  (14, 6, 2018, 59.40, false),
  (15, 6, 2020, 63.20, true),
  (16, 6, 2023, 69.90, true);

------
------
SELECT * FROM literacy_assessments ORDER BY assessment_id;
