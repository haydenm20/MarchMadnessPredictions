# Library Functions
library(caret)
library(ggplot2)
library(tidyverse)


RSdetailed <- read.csv("RegularSeasonDetailedResults.csv")
Tcompact <- read.csv("TourneyCompactResults.csv")
Tdetailed <- read.csv("TourneyDetailedResults.csv")
Tseeds <- read.csv("TourneySeeds.csv")

#Check for missing values
RSdetailed[!complete.cases(RSdetailed),]
Tcompact[!complete.cases(Tcompact),]
Tdetailed[!complete.cases(Tdetailed),]
Tseeds[!complete.cases(Tseeds),]

#View data files
glimpse(RSdetailed)
glimpse(Tcompact)
glimpse(Tdetailed)
glimpse(Tseeds)


#Sort data by team not W/L
winnerStats <- RSdetailed[c(1:4, 6:21)] 

#Change column names
names(winnerStats) <- c("Season", "Day", "Team", "Score", "OppScore", "Loc", "OT", "Fgm", 
                        "Fga", "3pm", "3pa", "Ftm", "Fta", "Oreb", "Dreb", "Ast", "TO", "Stl", "Blk", "PF")

#Add win/loss variable
winnerStats$Win <- 1


loserStats <- RSdetailed[c(1:2, 4:8, 22:34)]
loserStats <- loserStats[c(1:2, 4:5, 3, 6:20)]
loserStats$Wloc[loserStats$Wloc == "A"] <- "X"
loserStats$Wloc[loserStats$Wloc == "H"] <- "A"
loserStats$Wloc[loserStats$Wloc == "X"] <- "H"

#Change column names
names(loserStats) <- c("Season", "Day", "Team", "Score", "OppScore", "Loc", "OT", "Fgm", 
                        "Fga", "3pm", "3pa", "Ftm", "Fta", "Oreb", "Dreb", "Ast", "TO", "Stl", "Blk", "PF")

#Add win/loss variable
loserStats$Win <- 0

#Combine for season stats
SeasonStats <- rbind(winnerStats, loserStats)

#Create FG% variable
SeasonStats <- transform(SeasonStats, 'FGper' = Fgm/Fga)

#Create 3PT% variable
SeasonStats <- transform(SeasonStats, '3PTper' = X3pm/X3pa)


#Season averages
SeasonAvgStats <- aggregate(SeasonStats, by=list(SeasonStats$Season,SeasonStats$Team), FUN = mean)
SeasonAvgStats <- subset(SeasonAvgStats, select = -c(Day, Loc))
SeasonAvgStats <- subset(SeasonAvgStats, select = -c(1,2))

#2nd half season averages
SeasonStats2 <- SeasonStats[which(SeasonStats$Day > 61),]
SeasonAvgStats2 <- aggregate(SeasonStats2, by=list(SeasonStats2$Season,SeasonStats2$Team), FUN = mean)
SeasonAvgStats2 <- subset(SeasonAvgStats2, select = -c(Day, Loc))
SeasonAvgStats2 <- subset(SeasonAvgStats2, select = -c(1,2))


#Neutral/Away game averages
SeasonStats3 <- SeasonStats[which(SeasonStats$Loc != 'H'),]
SeasonAvgStats3 <- aggregate(SeasonStats3, by=list(SeasonStats3$Season,SeasonStats3$Team), FUN = mean)
SeasonStats3 <- subset(SeasonAvgStats3, select = -c(Day, Loc))
SeasonStats3 <- subset(SeasonAvgStats3, select = -c(1,2))



#Compare stats
CompareStats1 <- subset(SeasonAvgStats, select = c("Season", "Team", "Score", "OppScore", "Win"))
CompareStats1$Type <- "Full"
CompareStats2 <- subset(SeasonAvgStats2, select = c("Season", "Team", "Score", "OppScore", "Win"))
CompareStats2$Type <- "Second Half"
CompareStats3 <- subset(SeasonAvgStats3, select = c("Season", "Team", "Score", "OppScore", "Win"))
CompareStats3$Type <- "Not Home"

CompareStats <- rbind(CompareStats1, CompareStats2, CompareStats3)
CompareStats

ggplot(CompareStats, aes(x = Score, y = OppScore,color = Type)) + 
            geom_point() + theme_bw() + theme(legend.position = c(.9, .1))


#Stat Differences
CompareStatsSecDiff <- CompareStats1[,c("Score", "OppScore", "Win"),] - CompareStats2[,c("Score", "OppScore", "Win")]
CompareStatsSecDiff
mean(CompareStatsSecDiff$Score)
mean(CompareStatsSecDiff$OppScore)
mean(CompareStatsSecDiff$Win)


#Teams Averages
#Full Season
mean(CompareStats1$Score)
mean(CompareStats1$OppScore)
mean(CompareStats1$Win)

#Second half Season
mean(CompareStats2$Score)
mean(CompareStats2$OppScore)
mean(CompareStats2$Win)

#Not Home Games
mean(CompareStats3$Score)
mean(CompareStats3$OppScore)
mean(CompareStats3$Win)




#Tournament seed averages

#Seperate seed value by region and seed number
Tseeds$Seeding <- as.numeric(substr(Tseeds$Seed, 2,3))
Tseeds$Region <- as.factor(substr(Tseeds$Seed, 1, 1))
Tseeds$Seed <- Tseeds$Seeding
Tseeds = subset(Tseeds, select = -Seeding)

#Sort data by team not W/L
winnerTournStats <- Tdetailed[c(1:4, 6, 8:21)] 

#Change column names
names(winnerTournStats) <- c("Season", "Day", "Team", "Score", "OppScore", "OT", "Fgm", 
                        "Fga", "3pm", "3pa", "Ftm", "Fta", "Oreb", "Dreb", "Ast", "TO", "Stl", "Blk", "PF")

#Add win/loss variable
winnerTournStats$Win <- 1


loserTournStats <- Tdetailed[c(1:2, 5:6, 4, 8, 22:34)]

#Change column names
names(loserTournStats) <- c("Season", "Day", "Team", "Score", "OppScore", "OT", "Fgm", 
                       "Fga", "3pm", "3pa", "Ftm", "Fta", "Oreb", "Dreb", "Ast", "TO", "Stl", "Blk", "PF")

#Add win/loss variable
loserTournStats$Win <- 0

#Combine for season stats
TournStats <- rbind(winnerTournStats, loserTournStats)

#Create FG% variable
TournStats <- transform(TournStats, 'FGper' = Fgm/Fga)

#Create 3PT% variable
TournStats <- transform(TournStats, '3PTper' = X3pm/X3pa)



#Add seed
TournAvgStats <- aggregate(TournStats, by=list(TournStats$Season,TournStats$Team), FUN = mean)
TournAvgStats <- subset(TournAvgStats, select = -c(1,2))

TournAvgStats <- merge(TournAvgStats, Tseeds, by = c("Season", "Team"))

TournRankStats <- aggregate(TournAvgStats[4:23], by = list(TournAvgStats$Seed), FUN = mean)
names(TournRankStats)[1] <- "Rank"

#Change Tourn info from winner/loser to team1 team2
Tcompact$Team1 <- ifelse(Tcompact$Wteam < Tcompact$Lteam, Tcompact$Wteam, Tcompact$Lteam)
Tcompact$Team2 <- ifelse(Tcompact$Wteam > Tcompact$Lteam, Tcompact$Wteam, Tcompact$Lteam)
Tcompact$Team1Win <- ifelse(Tcompact$Wteam == Tcompact$Team1, 1, 0)



#Train df with total season averages
Train1 <- merge(Tcompact[,c("Season","Daynum","Team1","Team2","Team1Win")],
                SeasonAvgStats[,c("Season", "Team", "Score", "OppScore", "OT", "Fgm", "Fga", "X3pm", "X3pa", "Ftm", "Fta", "Oreb", "Dreb", "Ast",    
                                  "TO", "Stl", "Blk", "PF", "Win", "FGper", "X3PTper")], 
                by.x = c("Season","Team1"), by.y = c("Season","Team"))

names(Train1)[6:24] <- paste("Team1_",names(Train1)[6:24], sep = "")

Train1 <- merge(Train1, SeasonAvgStats[,c("Season", "Team", "Score", "OppScore", "OT", "Fgm", "Fga", "X3pm", "X3pa", "Ftm", "Fta", "Oreb", "Dreb", "Ast",    
                                          "TO", "Stl", "Blk", "PF", "Win", "FGper", "X3PTper")], 
                by.x = c("Season","Team2"), by.y = c("Season","Team"))

names(Train1)[25:43] <- paste("Team2_", names(Train1)[25:43], sep = "")

#Train df with only 2nd half season averages
Train2 <- merge(Tcompact[,c("Season","Daynum","Team1","Team2","Team1Win")],
                SeasonAvgStats2[,c("Season", "Team", "Score", "OppScore", "OT", "Fgm", "Fga", "X3pm", "X3pa", "Ftm", "Fta", "Oreb", "Dreb", "Ast",    
                                  "TO", "Stl", "Blk", "PF", "Win", "FGper", "X3PTper")], 
                by.x = c("Season","Team1"), by.y = c("Season","Team"))

names(Train2)[6:24] <- paste("Team1_",names(Train2)[6:24], sep = "")

Train2 <- merge(Train2, SeasonAvgStats2[,c("Season", "Team", "Score", "OppScore", "OT", "Fgm", "Fga", "X3pm", "X3pa", "Ftm", "Fta", "Oreb", "Dreb", "Ast",    
                                          "TO", "Stl", "Blk", "PF", "Win", "FGper", "X3PTper")], 
                by.x = c("Season","Team2"), by.y = c("Season","Team"))

names(Train2)[25:43] <- paste("Team2_", names(Train2)[25:43], sep = "")

#Train df with only Away/Neutral site game averages
Train3 <- merge(Tcompact[,c("Season","Daynum","Team1","Team2","Team1Win")],
                SeasonAvgStats3[,c("Season", "Team", "Score", "OppScore", "OT", "Fgm", "Fga", "X3pm", "X3pa", "Ftm", "Fta", "Oreb", "Dreb", "Ast",    
                                  "TO", "Stl", "Blk", "PF", "Win", "FGper", "X3PTper")], 
                by.x = c("Season","Team1"), by.y = c("Season","Team"))

names(Train3)[6:24] <- paste("Team1_",names(Train3)[6:24], sep = "")

Train3 <- merge(Train3, SeasonAvgStats3[,c("Season", "Team", "Score", "OppScore", "OT", "Fgm", "Fga", "X3pm", "X3pa", "Ftm", "Fta", "Oreb", "Dreb", "Ast",    
                                          "TO", "Stl", "Blk", "PF", "Win", "FGper", "X3PTper")], 
                by.x = c("Season","Team2"), by.y = c("Season","Team"))

names(Train3)[25:43] <- paste("Team2_", names(Train3)[25:43], sep = "")

#Calculate stat differences for testing
Train1$TeamScoreDiff <- Train1$Team1_Score - Train1$Team2_Score
Train1$TeamOppScoreDiff <- Train1$Team1_OppScore - Train1$Team2_OppScore
Train1$TeamOTDiff <- (Train1$Team1_OT - Train1$Team2_OT) * 100
Train1$TeamFgmDiff <- Train1$Team1_Fgm - Train1$Team2_Fgm
Train1$TeamFgaDiff <- Train1$Team1_Fga - Train1$Team2_Fga
Train1$Team3pmDiff <- Train1$Team1_X3pm - Train1$Team2_X3pm
Train1$Team3paDiff <- Train1$Team1_X3pa - Train1$Team2_X3pa
Train1$TeamFtmDiff <- Train1$Team1_Ftm - Train1$Team2_Ftm
Train1$TeamFtaDiff <- Train1$Team1_Fta - Train1$Team2_Fta
Train1$TeamOrebDiff <- Train1$Team1_Oreb - Train1$Team2_Oreb
Train1$TeamDrebDiff <- Train1$Team1_Dreb - Train1$Team2_Dreb
Train1$TeamAstDiff <- Train1$Team1_Ast - Train1$Team2_Ast
Train1$TeamTODiff <- Train1$Team1_TO - Train1$Team2_TO
Train1$TeamStlDiff <- Train1$Team1_Stl - Train1$Team2_Stl
Train1$TeamBlkDiff <- Train1$Team1_Blk - Train1$Team2_Blk
Train1$TeamPFDiff <- Train1$Team1_PF - Train1$Team2_PF
Train1$TeamWinDiff <- (Train1$Team1_Win - Train1$Team2_Win)*100
Train1$TeamFGperbDiff <- (Train1$Team1_FGper - Train1$Team2_FGper)*100
Train1$Team3PTperDiff <- (Train1$Team1_X3PTper - Train1$Team2_X3PTper)*100



Train2$TeamScoreDiff <- Train2$Team1_Score - Train2$Team2_Score
Train2$TeamOppScoreDiff <- Train2$Team1_OppScore - Train2$Team2_OppScore
Train2$TeamOTDiff <- (Train2$Team1_OT - Train2$Team2_OT) * 100
Train2$TeamFgmDiff <- Train2$Team1_Fgm - Train2$Team2_Fgm
Train2$TeamFgaDiff <- Train2$Team1_Fga - Train2$Team2_Fga
Train2$Team3pmDiff <- Train2$Team1_X3pm - Train2$Team2_X3pm
Train2$Team3paDiff <- Train2$Team1_X3pa - Train2$Team2_X3pa
Train2$TeamFtmDiff <- Train2$Team1_Ftm - Train2$Team2_Ftm
Train2$TeamFtaDiff <- Train2$Team1_Fta - Train2$Team2_Fta
Train2$TeamOrebDiff <- Train2$Team1_Oreb - Train2$Team2_Oreb
Train2$TeamDrebDiff <- Train2$Team1_Dreb - Train2$Team2_Dreb
Train2$TeamAstDiff <- Train2$Team1_Ast - Train2$Team2_Ast
Train2$TeamTODiff <- Train2$Team1_TO - Train2$Team2_TO
Train2$TeamStlDiff <- Train2$Team1_Stl - Train2$Team2_Stl
Train2$TeamBlkDiff <- Train2$Team1_Blk - Train2$Team2_Blk
Train2$TeamPFDiff <- Train2$Team1_PF - Train2$Team2_PF
Train2$TeamWinDiff <- (Train2$Team1_Win - Train2$Team2_Win)*100
Train2$TeamFGperbDiff <- (Train2$Team1_FGper - Train2$Team2_FGper)*100
Train2$Team3PTperDiff <- (Train2$Team1_X3PTper - Train2$Team2_X3PTper)*100



Train3$TeamScoreDiff <- Train3$Team1_Score - Train3$Team2_Score
Train3$TeamOppScoreDiff <- Train3$Team1_OppScore - Train3$Team2_OppScore
Train3$TeamOTDiff <- (Train3$Team1_OT - Train3$Team2_OT) * 100
Train3$TeamFgmDiff <- Train3$Team1_Fgm - Train3$Team2_Fgm
Train3$TeamFgaDiff <- Train3$Team1_Fga - Train3$Team2_Fga
Train3$Team3pmDiff <- Train3$Team1_X3pm - Train3$Team2_X3pm
Train3$Team3paDiff <- Train3$Team1_X3pa - Train3$Team2_X3pa
Train3$TeamFtmDiff <- Train3$Team1_Ftm - Train3$Team2_Ftm
Train3$TeamFtaDiff <- Train3$Team1_Fta - Train3$Team2_Fta
Train3$TeamOrebDiff <- Train3$Team1_Oreb - Train3$Team2_Oreb
Train3$TeamDrebDiff <- Train3$Team1_Dreb - Train3$Team2_Dreb
Train3$TeamAstDiff <- Train3$Team1_Ast - Train3$Team2_Ast
Train3$TeamTODiff <- Train3$Team1_TO - Train3$Team2_TO
Train3$TeamStlDiff <- Train3$Team1_Stl - Train3$Team2_Stl
Train3$TeamBlkDiff <- Train3$Team1_Blk - Train3$Team2_Blk
Train3$TeamPFDiff <- Train3$Team1_PF - Train3$Team2_PF
Train3$TeamWinDiff <- (Train3$Team1_Win - Train3$Team2_Win)*100
Train3$TeamFGperbDiff <- (Train3$Team1_FGper - Train3$Team2_FGper)*100
Train3$Team3PTperDiff <- (Train3$Team1_X3PTper - Train3$Team2_X3PTper)*100



#Add in Seed and Seed stats
Train1 <- merge(Train1, Tseeds[,c("Season", "Team", "Seed")], 
                by.x = c("Season","Team1"), by.y = c("Season","Team"))

names(Train1)[63] <- paste("Team1_",names(Train1)[63], sep = "")


Train1 <- merge(Train1, Tseeds[,c("Season", "Team", "Seed")], 
                by.x = c("Season","Team2"), by.y = c("Season","Team"))

names(Train1)[64] <- paste("Team2_",names(Train1)[64], sep = "")

Train1 <- merge(Train1, TournRankStats[,c("Rank", "Score", "OppScore", "OT", "Fgm", "Fga", "X3pm", "X3pa", "Ftm", "Fta", "Oreb", "Dreb", "Ast", "TO",      
                                          "Stl", "Blk", "PF", "Win", "FGper", "X3PTper")], 
                by.x = "Team1_Seed", by.y = "Rank")

names(Train1)[65:83] <- paste("Seed1_",names(Train1)[65:83], sep = "")

Train1 <- merge(Train1, TournRankStats[,c("Rank", "Score", "OppScore", "OT", "Fgm", "Fga", "X3pm", "X3pa", "Ftm", "Fta", "Oreb", "Dreb", "Ast", "TO",      
                                          "Stl", "Blk", "PF", "Win", "FGper", "X3PTper")], 
                by.x = "Team2_Seed", by.y = "Rank")

names(Train1)[84:102] <- paste("Seed2_",names(Train1)[84:102], sep = "")




Train2 <- merge(Train2, Tseeds[,c("Season", "Team", "Seed")], 
                by.x = c("Season","Team1"), by.y = c("Season","Team"))

names(Train2)[63] <- paste("Team1_",names(Train2)[63], sep = "")

Train2 <- merge(Train2, Tseeds[,c("Season", "Team", "Seed")], 
                by.x = c("Season","Team2"), by.y = c("Season","Team"))

names(Train2)[64] <- paste("Team2_",names(Train2)[64], sep = "")

Train2 <- merge(Train2, TournRankStats[,c("Rank", "Score", "OppScore", "OT", "Fgm", "Fga", "X3pm", "X3pa", "Ftm", "Fta", "Oreb", "Dreb", "Ast", "TO",      
                                          "Stl", "Blk", "PF", "Win", "FGper", "X3PTper")], 
                by.x = "Team1_Seed", by.y = "Rank")

names(Train2)[65:83] <- paste("Seed1_",names(Train2)[65:83], sep = "")

Train2 <- merge(Train2, TournRankStats[,c("Rank", "Score", "OppScore", "OT", "Fgm", "Fga", "X3pm", "X3pa", "Ftm", "Fta", "Oreb", "Dreb", "Ast", "TO",      
                                          "Stl", "Blk", "PF", "Win", "FGper", "X3PTper")], 
                by.x = "Team2_Seed", by.y = "Rank")

names(Train2)[84:102] <- paste("Seed2_",names(Train2)[84:102], sep = "")


Train3 <- merge(Train3, Tseeds[,c("Season", "Team", "Seed")], 
                by.x = c("Season","Team1"), by.y = c("Season","Team"))

names(Train3)[63] <- paste("Team1_",names(Train3)[63], sep = "")

Train3 <- merge(Train3, Tseeds[,c("Season", "Team", "Seed")], 
                by.x = c("Season","Team2"), by.y = c("Season","Team"))

names(Train3)[64] <- paste("Team2_",names(Train3)[64], sep = "")

Train3 <- merge(Train3, TournRankStats[,c("Rank", "Score", "OppScore", "OT", "Fgm", "Fga", "X3pm", "X3pa", "Ftm", "Fta", "Oreb", "Dreb", "Ast", "TO",      
                                          "Stl", "Blk", "PF", "Win", "FGper", "X3PTper")], 
                by.x = "Team1_Seed", by.y = "Rank")

names(Train3)[65:83] <- paste("Seed1_",names(Train3)[65:83], sep = "")

Train3 <- merge(Train3, TournRankStats[,c("Rank", "Score", "OppScore", "OT", "Fgm", "Fga", "X3pm", "X3pa", "Ftm", "Fta", "Oreb", "Dreb", "Ast", "TO",      
                                          "Stl", "Blk", "PF", "Win", "FGper", "X3PTper")], 
                by.x = "Team2_Seed", by.y = "Rank")

names(Train3)[84:102] <- paste("Seed2_",names(Train3)[84:102], sep = "")

#Create differences in seed stats to use in training the model
Train1$SeedDiff <- Train1$Team1_Seed - Train1$Team2_Seed
Train1$SeedScoreDiff <- Train1$Seed1_Score - Train1$Seed2_Score
Train1$SeedOppScoreDiff <- Train1$Seed1_OppScore - Train1$Seed2_OppScore
Train1$SeedOTDiff <- (Train1$Seed1_OT - Train1$Seed2_OT) * 100
Train1$SeedFgmDiff <- Train1$Seed1_Fgm - Train1$Seed2_Fgm
Train1$SeedFgaDiff <- Train1$Seed1_Fga - Train1$Seed2_Fga
Train1$Seed3pmDiff <- Train1$Seed1_X3pm - Train1$Seed2_X3pm
Train1$Seed3paDiff <- Train1$Seed1_X3pa - Train1$Seed2_X3pa
Train1$SeedFtmDiff <- Train1$Seed1_Ftm - Train1$Seed2_Ftm
Train1$SeedFtaDiff <- Train1$Seed1_Fta - Train1$Seed2_Fta
Train1$SeedOrebDiff <- Train1$Seed1_Oreb - Train1$Seed2_Oreb
Train1$SeedDrebDiff <- Train1$Seed1_Dreb - Train1$Seed2_Dreb
Train1$SeedAstDiff <- Train1$Seed1_Ast - Train1$Seed2_Ast
Train1$SeedTODiff <- Train1$Seed1_TO - Train1$Seed2_TO
Train1$SeedStlDiff <- Train1$Seed1_Stl - Train1$Seed2_Stl
Train1$SeedBlkDiff <- Train1$Seed1_Blk - Train1$Seed2_Blk
Train1$SeedPFDiff <- Train1$Seed1_PF - Train1$Seed2_PF
Train1$SeedWinDiff <- (Train1$Seed1_Win - Train1$Seed2_Win)*100
Train1$SeedFGperbDiff <- (Train1$Seed1_FGper - Train1$Seed2_FGper)*100
Train1$Seed3PTperDiff <- (Train1$Seed1_X3PTper - Train1$Seed2_X3PTper)*100



Train2$SeedDiff <- Train2$Team1_Seed - Train2$Team2_Seed
Train2$SeedScoreDiff <- Train2$Seed1_Score - Train2$Seed2_Score
Train2$SeedOppScoreDiff <- Train2$Seed1_OppScore - Train2$Seed2_OppScore
Train2$SeedOTDiff <- (Train2$Seed1_OT - Train2$Seed2_OT) * 100
Train2$SeedFgmDiff <- Train2$Seed1_Fgm - Train2$Seed2_Fgm
Train2$SeedFgaDiff <- Train2$Seed1_Fga - Train2$Seed2_Fga
Train2$Seed3pmDiff <- Train2$Seed1_X3pm - Train2$Seed2_X3pm
Train2$Seed3paDiff <- Train2$Seed1_X3pa - Train2$Seed2_X3pa
Train2$SeedFtmDiff <- Train2$Seed1_Ftm - Train2$Seed2_Ftm
Train2$SeedFtaDiff <- Train2$Seed1_Fta - Train2$Seed2_Fta
Train2$SeedOrebDiff <- Train2$Seed1_Oreb - Train2$Seed2_Oreb
Train2$SeedDrebDiff <- Train2$Seed1_Dreb - Train2$Seed2_Dreb
Train2$SeedAstDiff <- Train2$Seed1_Ast - Train2$Seed2_Ast
Train2$SeedTODiff <- Train2$Seed1_TO - Train2$Seed2_TO
Train2$SeedStlDiff <- Train2$Seed1_Stl - Train2$Seed2_Stl
Train2$SeedBlkDiff <- Train2$Seed1_Blk - Train2$Seed2_Blk
Train2$SeedPFDiff <- Train2$Seed1_PF - Train2$Seed2_PF
Train2$SeedWinDiff <- (Train2$Seed1_Win - Train2$Seed2_Win)*100
Train2$SeedFGperbDiff <- (Train2$Seed1_FGper - Train2$Seed2_FGper)*100
Train2$Seed3PTperDiff <- (Train2$Seed1_X3PTper - Train2$Seed2_X3PTper)*100



Train3$SeedDiff <- Train3$Team1_Seed - Train3$Team2_Seed
Train3$SeedScoreDiff <- Train3$Seed1_Score - Train3$Seed2_Score
Train3$SeedOppScoreDiff <- Train3$Seed1_OppScore - Train3$Seed2_OppScore
Train3$SeedOTDiff <- (Train3$Seed1_OT - Train3$Seed2_OT) * 100
Train3$SeedFgmDiff <- Train3$Seed1_Fgm - Train3$Seed2_Fgm
Train3$SeedFgaDiff <- Train3$Seed1_Fga - Train3$Seed2_Fga
Train3$Seed3pmDiff <- Train3$Seed1_X3pm - Train3$Seed2_X3pm
Train3$Seed3paDiff <- Train3$Seed1_X3pa - Train3$Seed2_X3pa
Train3$SeedFtmDiff <- Train3$Seed1_Ftm - Train3$Seed2_Ftm
Train3$SeedFtaDiff <- Train3$Seed1_Fta - Train3$Seed2_Fta
Train3$SeedOrebDiff <- Train3$Seed1_Oreb - Train3$Seed2_Oreb
Train3$SeedDrebDiff <- Train3$Seed1_Dreb - Train3$Seed2_Dreb
Train3$SeedAstDiff <- Train3$Seed1_Ast - Train3$Seed2_Ast
Train3$SeedTODiff <- Train3$Seed1_TO - Train3$Seed2_TO
Train3$SeedStlDiff <- Train3$Seed1_Stl - Train3$Seed2_Stl
Train3$SeedBlkDiff <- Train3$Seed1_Blk - Train3$Seed2_Blk
Train3$SeedPFDiff <- Train3$Seed1_PF - Train3$Seed2_PF
Train3$SeedWinDiff <- (Train3$Seed1_Win - Train3$Seed2_Win)*100
Train3$SeedFGperbDiff <- (Train3$Seed1_FGper - Train3$Seed2_FGper)*100
Train3$Seed3PTperDiff <- (Train3$Seed1_X3PTper - Train3$Seed2_X3PTper)*100



#Change wins from 0/1 to win/loss
Train1$Team1Win <- ifelse(Train1$Team1Win == 1, "Win", "Loss")
Train2$Team1Win <- ifelse(Train1$Team1Win == 1, "Win", "Loss")
Train3$Team1Win <- ifelse(Train1$Team1Win == 1, "Win", "Loss")


#Change Daynum to round
Train1$Round <- Train1$Daynum
Train1$Round[Train1$Daynum == 134] <- 0
Train1$Round[Train1$Daynum == 135] <- 0
Train1$Round[Train1$Daynum == 136] <- 1
Train1$Round[Train1$Daynum == 137] <- 1
Train1$Round[Train1$Daynum == 138] <- 2
Train1$Round[Train1$Daynum == 139] <- 2
Train1$Round[Train1$Daynum == 143] <- 3
Train1$Round[Train1$Daynum == 144] <- 3
Train1$Round[Train1$Daynum == 145] <- 4
Train1$Round[Train1$Daynum == 146] <- 4
Train1$Round[Train1$Daynum == 152] <- 5
Train1$Round[Train1$Daynum == 154] <- 6

Train2$Round <- Train2$Daynum
Train2$Round[Train2$Daynum == 134] <- 0
Train2$Round[Train2$Daynum == 135] <- 0
Train2$Round[Train2$Daynum == 136] <- 1
Train2$Round[Train2$Daynum == 137] <- 1
Train2$Round[Train2$Daynum == 138] <- 2
Train2$Round[Train2$Daynum == 139] <- 2
Train2$Round[Train2$Daynum == 143] <- 3
Train2$Round[Train2$Daynum == 144] <- 3
Train2$Round[Train2$Daynum == 145] <- 4
Train2$Round[Train2$Daynum == 146] <- 4
Train2$Round[Train2$Daynum == 152] <- 5
Train2$Round[Train2$Daynum == 154] <- 6

Train3$Round <- Train3$Daynum
Train3$Round[Train3$Daynum == 134] <- 0
Train3$Round[Train3$Daynum == 135] <- 0
Train3$Round[Train3$Daynum == 136] <- 1
Train3$Round[Train3$Daynum == 137] <- 1
Train3$Round[Train3$Daynum == 138] <- 2
Train3$Round[Train3$Daynum == 139] <- 2
Train3$Round[Train3$Daynum == 143] <- 3
Train3$Round[Train3$Daynum == 144] <- 3
Train3$Round[Train3$Daynum == 145] <- 4
Train3$Round[Train3$Daynum == 146] <- 4
Train3$Round[Train3$Daynum == 152] <- 5
Train3$Round[Train3$Daynum == 154] <- 6

#Using only difference stats
Train1 <- subset(Train1, select = c(1:7, 46:64, 103:123))
Train2 <- subset(Train2, select = c(1:7, 46:64, 103:123))
Train3 <- subset(Train3, select = c(1:7, 46:64, 103:123))

#Look at upsets
Upsets <- subset(Train1, (Team1_Seed - Team2_Seed) > 0 & Team1Win == "Win") 
Upsets <- rbind(Upsets, subset(Train1, (Team1_Seed - Team2_Seed) < 0 & Team1Win == "Loss"))


#Percent of games won by better seed
1 - (249/914)

#Figure to show potential upset
ggplot(Upsets, aes(x = Daynum, y = SeedDiff)) +
  geom_count(shape = 9, color = "Blue") +
  theme_bw() + ggtitle("Seed Difference vs Game Day")

#Plot to show relationship between seed difference and team win percentage
ggplot(Train1, aes(x = SeedDiff, y = TeamWinDiff)) + geom_smooth(method = loess) +
  geom_point(position = "jitter", color = "Purple", size = 3) + theme_bw() +
  ggtitle("Seed Difference vs Win Percentage Difference")

#Plot seed difference vs score diff
ggplot(Train1, aes(x = SeedDiff, y = TeamScoreDiff)) + geom_smooth(method = loess) +
  geom_point(position = "jitter", color = "Purple", size = 3) + theme_bw() +
  ggtitle("Seed Difference vs Team Score Difference")

#Plot Seed Difference vs opponent score difference
ggplot(Train1, aes(x = SeedDiff, y = TeamOppScoreDiff)) + geom_smooth(method = loess) +
  geom_point(position = "jitter", color = "Purple", size = 3) + theme_bw() +
  ggtitle("Seed Difference vs Opponent Score Difference") 

#Plot win % difference vs score difference
ggplot(Train1, aes(x = TeamWinDiff, y = TeamScoreDiff)) + geom_smooth(method = loess) +
  geom_point(position = "jitter", color = "Purple", size = 3) + theme_bw() +
  ggtitle("Win Percentage Difference vs Team Score Difference")

#Plot win % vs opponent score difference
ggplot(Train1, aes(x = TeamWinDiff, y = TeamOppScoreDiff)) + geom_smooth(method = loess) +
  geom_point(position = "jitter", color = "Purple", size = 3) + theme_bw() +
  ggtitle("Win Percentage Difference vs Opponent Score Difference") 


#Train for part 1
Train1P1 <- Train1[which(Train1$Season <= 2012),]
Test1P1 <- Train1[which(Train1$Season >= 2013),]

Train2P1 <- Train1[which(Train1$Season <= 2012),]
Test2P1 <- Train1[which(Train1$Season >= 2013),]

Train3P1 <- Train1[which(Train1$Season <= 2012),]
Test3P1 <- Train1[which(Train1$Season >= 2013),]




#Data for Part 2
Teams2017 <- subset(Tseeds[Tseeds$Season == 2017,], select = c("Team"))
names(Teams2017)[1] <- "Team1"
Teams2017$Team2 <- Teams2017$Team1

Teams2017 <- cross_df(Teams2017)
Teams2017 <- Teams2017[which(Teams2017$Team1 < Teams2017$Team2),]
Teams2017$Season <- 2017

Teams2017 <- merge(Teams2017, Tseeds[,c("Season", "Seed", "Team", "Region")], 
                by.x = c("Season", "Team1"), by.y = c("Season", "Team"))

names(Teams2017)[4:5] <- paste("Team1_",names(Teams2017)[4:5], sep = "")

Teams2017 <- merge(Teams2017, Tseeds[,c("Season", "Seed", "Team", "Region")], 
                   by.x = c("Season", "Team2"), by.y = c("Season", "Team"))

names(Teams2017)[6:7] <- paste("Team2_",names(Teams2017)[6:7], sep = "")

Teams2017 <- merge(Teams2017,
                SeasonAvgStats3[,c("Season", "Team", "Score", "OppScore", "OT", "Fgm", "Fga", "X3pm", "X3pa", "Ftm", "Fta", "Oreb", "Dreb", "Ast",    
                                   "TO", "Stl", "Blk", "PF", "Win", "FGper", "X3PTper")], 
                by.x = c("Season","Team1"), by.y = c("Season","Team"))

names(Teams2017)[8:26] <- paste("Team1_",names(Teams2017)[8:26], sep = "")

Teams2017 <- merge(Teams2017, SeasonAvgStats3[,c("Season", "Team", "Score", "OppScore", "OT", "Fgm", "Fga", "X3pm", "X3pa", "Ftm", "Fta", "Oreb", "Dreb", "Ast",    
                                           "TO", "Stl", "Blk", "PF", "Win", "FGper", "X3PTper")], 
                by.x = c("Season","Team2"), by.y = c("Season","Team"))

names(Teams2017)[27:45] <- paste("Team2_", names(Teams2017)[27:45], sep = "")

Teams2017$TeamScoreDiff <- Teams2017$Team1_Score - Teams2017$Team2_Score
Teams2017$TeamOppScoreDiff <- Teams2017$Team1_OppScore - Teams2017$Team2_OppScore
Teams2017$TeamOTDiff <- (Teams2017$Team1_OT - Teams2017$Team2_OT) * 100
Teams2017$TeamFgmDiff <- Teams2017$Team1_Fgm - Teams2017$Team2_Fgm
Teams2017$TeamFgaDiff <- Teams2017$Team1_Fga - Teams2017$Team2_Fga
Teams2017$Team3pmDiff <- Teams2017$Team1_X3pm - Teams2017$Team2_X3pm
Teams2017$Team3paDiff <- Teams2017$Team1_X3pa - Teams2017$Team2_X3pa
Teams2017$TeamFtmDiff <- Teams2017$Team1_Ftm - Teams2017$Team2_Ftm
Teams2017$TeamFtaDiff <- Teams2017$Team1_Fta - Teams2017$Team2_Fta
Teams2017$TeamOrebDiff <- Teams2017$Team1_Oreb - Teams2017$Team2_Oreb
Teams2017$TeamDrebDiff <- Teams2017$Team1_Dreb - Teams2017$Team2_Dreb
Teams2017$TeamAstDiff <- Teams2017$Team1_Ast - Teams2017$Team2_Ast
Teams2017$TeamTODiff <- Teams2017$Team1_TO - Teams2017$Team2_TO
Teams2017$TeamStlDiff <- Teams2017$Team1_Stl - Teams2017$Team2_Stl
Teams2017$TeamBlkDiff <- Teams2017$Team1_Blk - Teams2017$Team2_Blk
Teams2017$TeamPFDiff <- Teams2017$Team1_PF - Teams2017$Team2_PF
Teams2017$TeamWinDiff <- (Teams2017$Team1_Win - Teams2017$Team2_Win)*100
Teams2017$TeamFGperbDiff <- (Teams2017$Team1_FGper - Teams2017$Team2_FGper)*100
Teams2017$Team3PTperDiff <- (Teams2017$Team1_X3PTper - Teams2017$Team2_X3PTper)*100

Test2 <- merge(Teams2017, TournRankStats[,c("Rank", "Score", "OppScore", "OT", "Fgm", "Fga", "X3pm", "X3pa", "Ftm", "Fta", "Oreb", "Dreb", "Ast", "TO",      
                                          "Stl", "Blk", "PF", "Win", "FGper", "X3PTper")], 
                by.x = "Team1_Seed", by.y = "Rank")

names(Test2)[65:83] <- paste("Seed1_",names(Test2)[65:83], sep = "")

Test2 <- merge(Test2, TournRankStats[,c("Rank", "Score", "OppScore", "OT", "Fgm", "Fga", "X3pm", "X3pa", "Ftm", "Fta", "Oreb", "Dreb", "Ast", "TO",      
                                          "Stl", "Blk", "PF", "Win", "FGper", "X3PTper")], 
                by.x = "Team2_Seed", by.y = "Rank")

names(Test2)[84:102] <- paste("Seed2_",names(Test2)[84:102], sep = "")

Test2$SeedDiff <- Test2$Team1_Seed - Test2$Team2_Seed
Test2$SeedScoreDiff <- Test2$Seed1_Score - Test2$Seed2_Score
Test2$SeedOppScoreDiff <- Test2$Seed1_OppScore - Test2$Seed2_OppScore
Test2$SeedOTDiff <- (Test2$Seed1_OT - Test2$Seed2_OT) * 100
Test2$SeedFgmDiff <- Test2$Seed1_Fgm - Test2$Seed2_Fgm
Test2$SeedFgaDiff <- Test2$Seed1_Fga - Test2$Seed2_Fga
Test2$Seed3pmDiff <- Test2$Seed1_X3pm - Test2$Seed2_X3pm
Test2$Seed3paDiff <- Test2$Seed1_X3pa - Test2$Seed2_X3pa
Test2$SeedFtmDiff <- Test2$Seed1_Ftm - Test2$Seed2_Ftm
Test2$SeedFtaDiff <- Test2$Seed1_Fta - Test2$Seed2_Fta
Test2$SeedOrebDiff <- Test2$Seed1_Oreb - Test2$Seed2_Oreb
Test2$SeedDrebDiff <- Test2$Seed1_Dreb - Test2$Seed2_Dreb
Test2$SeedAstDiff <- Test2$Seed1_Ast - Test2$Seed2_Ast
Test2$SeedTODiff <- Test2$Seed1_TO - Test2$Seed2_TO
Test2$SeedStlDiff <- Test2$Seed1_Stl - Test2$Seed2_Stl
Test2$SeedBlkDiff <- Test2$Seed1_Blk - Test2$Seed2_Blk
Test2$SeedPFDiff <- Test2$Seed1_PF - Test2$Seed2_PF
Test2$SeedWinDiff <- (Test2$Seed1_Win - Test2$Seed2_Win)*100
Test2$SeedFGperbDiff <- (Test2$Seed1_FGper - Test2$Seed2_FGper)*100
Test2$Seed3PTperDiff <- (Test2$Seed1_X3PTper - Test2$Seed2_X3PTper)*100





#Formulas for testing
Formula.all <- Team1Win ~ SeedDiff + TeamScoreDiff + TeamOppScoreDiff  +
                      TeamOTDiff + TeamFgmDiff + TeamFgaDiff + Team3pmDiff +
                      Team3paDiff + TeamFtmDiff + TeamFtaDiff + TeamOrebDiff +
                      TeamDrebDiff + TeamAstDiff + TeamTODiff + TeamStlDiff +
                      TeamBlkDiff + TeamPFDiff + TeamWinDiff + TeamFGperbDiff + 
                      Team3PTperDiff + SeedScoreDiff + SeedOppScoreDiff + SeedOTDiff + 
                      SeedFgmDiff + SeedFgaDiff + Seed3pmDiff + Seed3paDiff + 
                      SeedFtmDiff + SeedFtaDiff + SeedOrebDiff + SeedDrebDiff + 
                      SeedAstDiff + SeedTODiff + SeedStlDiff+ SeedBlkDiff + 
                      SeedPFDiff + SeedWinDiff + SeedFGperbDiff + Seed3PTperDiff + Daynum


Formula <- Team1Win ~ SeedDiff + TeamScoreDiff + TeamOppScoreDiff  +
                          TeamOTDiff + TeamFgmDiff + TeamFgaDiff + Team3pmDiff +
                          Team3paDiff + TeamFtaDiff + TeamOrebDiff +
                          TeamDrebDiff + TeamAstDiff +  TeamPFDiff + 
                          TeamWinDiff + TeamFGperbDiff + 
                          Team3PTperDiff + SeedScoreDiff + SeedOppScoreDiff + SeedOTDiff + 
                          SeedFgmDiff + SeedFgaDiff +  Seed3paDiff + 
                          SeedFtaDiff + SeedOrebDiff + SeedDrebDiff + 
                          SeedAstDiff + SeedTODiff + SeedBlkDiff 




#Train Control
crossValidation <- trainControl(method = "repeatedcv",
                                number = 10,
                                repeats = 5,
                                summaryFunction=mnLogLoss,
                                classProbs = TRUE)

####### GLM #######
train.fit.glm <- train(Formula, 
                       data = Train3P1, 
                       method = 'glm',
                       trControl = crossValidation,
                       family = 'binomial',
                       metric = 'logLoss'
)
train.fit.glm

prediction_prob_glm <- predict(train.fit.glm, Test3P1, type = "prob")
prediction_prob_glm

predictions_glm <- predict(train.fit.glm, Test3P1, type = 'raw')
#predictions_glm


Test2$Predict_glm <- predictions_glm
Test1P1$Correct_glm <- ifelse(Test1P1$Predict_glm == Test1P1$Team1Win, 1, 0)
mean(Test1P1$Correct_glm)

summary(train.fit.glm)

##Part 1 Results##
Part1Results <- subset(Test3P1, select = c("Team1", "Team2"))
Part1Results$Team1Win <- prediction_prob_glm[2]
view(Part1Results)

############ EARTH #################

hyperparameters.earth <-  expand.grid(degree = c(1:5),
                                      nprune = c(1:5))

train.fit.earth <- train(Formula, 
                         data = Train3P1, 
                         method='earth',
                         tuneGrid=hyperparameters.earth,
                         trControl=crossValidation, 
                         metric = 'logLoss')

train.fit.earth

prediction_prob_earth <- predict(train.fit.earth, Test1P1, type = "prob")
prediction_prob_earth

predictions_earth <- predict(train.fit.earth, Test1P1, type = 'raw')
predictions_earth


Test1P1$Predict_earth <- predictions_earth
Test1P1$Correct_earth <- ifelse(Test1P1$Predict_earth == Test1P1$Team1Win, 1, 0)
mean(Test1P1$Correct_earth)

summary(train.fit.earth)

########### Neural Netwrok ############
tune_grid.nn <- expand.grid(decay=c(.175),
                            size=c(1:5))

train.fit.nn <- train(Formula,
                      data = Train3P1,
                      method = 'nnet',
                      verbose = TRUE,
                      trControl = crossValidation,
                      tuneControl = tune_grid.nn)

train.fit.nn

prediction_prob_nn <- predict(train.fit.nn, Test1P1, type = "prob")
prediction_prob_nn

predictions_nn <- predict(train.fit.nn, Test1P1, type = 'raw')
predictions_nn


Test1P1$Predict_nn <- predictions_nn
Test1P1$Correct_nn <- ifelse(Test1P1$Predict_nn == Test1P1$Team1Win, 1, 0)
mean(Test1P1$Correct_nn)

summary(train.fit.nn)


##Part 2 Results##
Part2Results <- subset(Test2, select = c(1:7, 123))
view(Part2Results)
