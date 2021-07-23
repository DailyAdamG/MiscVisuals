library(Lahman)
library(tidyverse)
library(sqldf)

sqldf("SELECT (nameFirst || ' ' || nameLast) AS Name, GROUP_CONCAT(DISTINCT TeamsFranchises.franchName) AS Teams, COUNT(DISTINCT TeamsFranchises.franchID) AS Franchises
      FROM Master
      INNER JOIN Batting
      ON Master.playerID = Batting.playerID
      INNER JOIN Teams
      ON Batting.teamID = Teams.teamID
      AND Batting.yearID = Teams.yearID
      INNER JOIN TeamsFranchises
      ON Teams.franchID = TeamsFranchises.franchID
      INNER JOIN AllstarFull
      ON Batting.playerID = AllstarFull.playerID
      AND Batting.yearID = AllstarFull.yearID
      AND Batting.teamID = AllstarFull.teamID
      GROUP BY Master.playerID
      HAVING Franchises >= 3
      ORDER BY Franchises DESC")