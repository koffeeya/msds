/*
  tb.sql
*/

DROP TABLE IF EXISTS tb;

CREATE TABLE tb 
(
  country varchar(100) NOT NULL,
  year int NOT NULL,
  sex varchar(6) NOT NULL,
  child int NULL,
  adult int NULL,
  elderly int NULL
);

SELECT * FROM tb;

LOAD DATA LOCAL INFILE 'C:/Users/Kavya/Desktop/Education/MS Data Science/DATA 607 (Data Acquisition and Management)/Labs/Lab 02.02/tb(1).csv' 
INTO TABLE tb
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(country, year, sex, @child, @adult, @elderly)
SET
child = nullif(@child,-1),
adult = nullif(@adult,-1),
elderly = nullif(@elderly,-1)
;

SELECT * FROM tb WHERE elderly IS NULL;
SELECT COUNT(*) FROM tb;

SELECT country, year, SUM(child + adult + elderly) AS cases 
INTO OUTFILE '/Users/Public/kbtb.csv'
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"'
ESCAPED BY '\\'
LINES TERMINATED BY '\n'
FROM tb 
WHERE child != '' AND adult != '' AND elderly != '' 
GROUP BY country, year;