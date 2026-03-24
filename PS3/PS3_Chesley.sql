--Read in Floria insurance csv

.mode csv
.import FL_insurance_sample.csv insurance_data

--Print out first 10 rows of the data set

SELECT * FROM insurance_data LIMIT 10;

--List which counties are in the sample (i.e. list unique values of the county vari-
able)
--Ordering by county to make it organized.

SELECT DISTINCT county 
FROM insurance_data 
ORDER BY county;

--Compute the average property appreciation from 2011 to 2012 (i.e. compute the
mean of tiv_2012 - tiv_2011)

SELECT AVG(tiv_2012 - tiv_2011) AS avg_property_appreciation
FROM insurance_data;

--Create a frequency table of the construction variable to show fraction of
 buildings by construction material

SELECT 
    construction,
    COUNT(*) AS frequency,
    ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM insurance_data), 2) AS percentage
FROM insurance_data
GROUP BY construction
ORDER BY frequency DESC;
