CREATE DATABASE movies;

USE movies;

DROP TABLE movieratings;

CREATE TABLE movies.movieratings (
submitted VARCHAR(100), 
movie VARCHAR(25), 
rating int);

LOAD DATA LOCAL INFILE 'C:/Users/Kavya/Desktop/Education/MS Data Science/DATA 607 (Data Acquisition and Management)/Assignments/Assignment 02/movie-ratings.csv'
INTO TABLE movieratings
FIELDS TERMINATED BY ','
    ENCLOSED BY '"';