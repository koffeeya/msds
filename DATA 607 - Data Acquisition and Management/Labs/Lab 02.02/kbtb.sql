SELECT country, year, SUM(child + adult + elderly) AS cases 
INTO OUTFILE 'C:/ProgramData/MySQL/MySQL Server 5.7/Uploads/kbtb.csv'
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"'
ESCAPED BY '\\'
LINES TERMINATED BY '\n'
FROM tb 
WHERE child != '' AND adult != '' AND elderly != '' 
GROUP BY country, year;