-- PS3
-- Murphy Chesley
-- Question 5

-- (a) Read in the Florida insurance data CSV file
CREATE TABLE IF NOT EXISTS florida_insurance (
    policyID            INTEGER,
    statecode           TEXT,
    county              TEXT,
    eq_site_limit       REAL,
    hu_site_limit       REAL,
    fl_site_limit       REAL,
    fr_site_limit       REAL,
    tiv_2011            REAL,
    tiv_2012            REAL,
    eq_site_deductible  REAL,
    hu_site_deductible  REAL,
    fl_site_deductible  REAL,
    fr_site_deductible  REAL,
    point_latitude      REAL,
    point_longitude     REAL,
    line                TEXT,
    construction        TEXT,
    point_granularity   INTEGER
);

.mode csv
.headers on
.import FL_insurance_sample.csv florida_insurance

-- (b) Print the first 10 rows
SELECT *
FROM florida_insurance
LIMIT 10;

-- (c)List unique counties in the sample
SELECT DISTINCT county
FROM florida_insurance
ORDER BY county;

--(d) Compute average property appreciation from 2011 to 2012
SELECT ROUND(AVG(tiv_2012 - tiv_2011), 2) AS avg_appreciation
FROM florida_insurance;

--(e) Frequency table of the construction variable
SELECT
    construction,
    COUNT(*)                                            AS frequency,
    ROUND(COUNT(*) * 100.0 / SUM(COUNT(*)) OVER (), 2) AS percentage
FROM florida_insurance
GROUP BY construction
ORDER BY frequency DESC;
