/*
  HOL_SQLtoCSV.sql
  
  Task:  export a .CSV file that contains total tuberculosis cases by country by year
  
  Here is some code that may give you some insight into a solution path
*/

SELECT * FROM tb LIMIT 20;

SELECT country, year, sex, child+adult+elderly AS cases
FROM tb;

DROP TABLE IF EXISTS tb_cases;

CREATE TABLE tb_cases AS
 SELECT country, year, sex, child+adult+elderly AS cases
FROM tb;

SELECT *
FROM tb_cases
WHERE country = 'Afghanistan'
ORDER BY country, year
LIMIT 50;

SELECT country, year, SUM(cases) AS cases
 FROM tb_cases
GROUP BY country, year
HAVING cases IS NOT NULL
ORDER BY country, year;

/* Note that this statement will not overwrite file if it already exists
   Make sure that you are writing your .CSV file to a place on your Mac or PC
   where MySQL has write privileges
*/

SELECT country, year, SUM(cases) AS cases INTO OUTFILE '/Users/Public/tb.csv'
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"'
ESCAPED BY '\\'
LINES TERMINATED BY '\n'
FROM tb_cases
GROUP BY country, year
HAVING cases IS NOT NULL
ORDER BY country, year;