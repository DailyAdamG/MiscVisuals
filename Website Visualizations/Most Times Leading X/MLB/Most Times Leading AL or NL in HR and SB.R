#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most Times Leading X/MLB")

#Load necessary libraries

library(sqldf)
library(tidyverse)
library(zoo)
library(data.table)
library(Lahman)

#Get data from Lahman Database

data <- sqldf(' WITH sub1 AS(SELECT People.playerID, Batting.yearID, Batting.lgID, People.nameFirst, People.nameLast,
GROUP_CONCAT(TeamsFranchises.franchName) AS Teams, TeamsFranchises.franchID, SUM(Batting.HR) AS HR, SUM(Batting.SB) AS SB,
DENSE_RANK () OVER(PARTITION BY Batting.yearID, Batting.lgID ORDER BY SUM(Batting.HR) DESC) AS HR_Rank,
DENSE_RANK () OVER(PARTITION BY Batting.yearID, Batting.lgID ORDER BY SUM(Batting.SB) DESC) AS SB_Rank,
DENSE_RANK () OVER(PARTITION BY People.playerID ORDER BY Batting.yearID) AS SeasonNumber
                    FROM People
                    INNER JOIN Batting
                    ON People.playerID = Batting.playerID
                    INNER JOIN Teams
                    ON Batting.teamID = Teams.teamID
                    AND Batting.yearID = Teams.yearID
                    INNER JOIN TeamsFranchises
                    ON Teams.franchID = TeamsFranchises.franchID
                    GROUP BY Batting.playerID, Batting.yearID, Batting.lgID)
              SELECT *
              FROM sub1
              WHERE HR_Rank = 1
              AND SB_Rank = 1
              AND SB IS NOT NULL')