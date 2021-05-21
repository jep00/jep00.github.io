library(dplyr)

co.we <- c("E0", "E1", "E2", "E3")  #Website country codes (football-data)
seasons <- c("1112", "1213", "1314", "1415", "1516", "1617", "1718")
matchdataTemp <- NULL; matchdata <- NULL
for (i in seasons){
  for (j in 1:4){
    matchdataTemp <- read.csv(paste0('https://www.football-data.co.uk/mmz4281/', i, '/', co.we[j],'.csv'), fileEncoding = 'latin1')
    matchdataTemp <- matchdataTemp[ ,c("Div", "Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR")]
    matchdata <- rbind(matchdata, matchdataTemp)
  }
}
#matchdata <- na.omit(matchdata)

pointdata <- NULL
for (i in unique(matchdata$HomeTeam)){
  pointdata <- rbind(pointdata, i)
}
pointdata <- data.frame(pointdata)
pointdata$points <- with(pointdata, 0)
pointdata$games <- with(pointdata, 0)

pointdata$W <- with(pointdata, 0)
pointdata$D <- with(pointdata, 0)
pointdata$L <- with(pointdata, 0)

colnames(pointdata) <- c("team", "points", "games", "W", "D", "L")
rownames(pointdata) <- 1:nrow(pointdata)

for (i in 1:nrow(pointdata)){
  for (j in 1:nrow(matchdata)){
    if (matchdata$HomeTeam[j] == pointdata$team[i]){
      if (matchdata$FTR[j] == "H"){
        pointdata$points[i] <- pointdata$points[i] + 3
        pointdata$W[i] <- pointdata$W[i] + 1}
      else if (matchdata$FTR[j] == "D"){
        pointdata$points[i] <- pointdata$points[i] + 1
        pointdata$D[i] <- pointdata$D[i] + 1}
      else{pointdata$L[i] <- pointdata$L[i] + 1}
      pointdata$games[i] <- pointdata$games[i] + 1}
    
    else if (matchdata$AwayTeam[j] == pointdata$team[i]){
      if (matchdata$FTR[j] == "A"){
        pointdata$points[i] <- pointdata$points[i] + 3
        pointdata$W[i] <- pointdata$W[i] + 1}
      else if (matchdata$FTR[j] == "D"){
        pointdata$points[i] <- pointdata$points[i] + 1
        pointdata$D[i] <- pointdata$D[i] + 1}
      else{pointdata$L[i] <- pointdata$L[i] + 1}
      pointdata$games[i] <- pointdata$games[i] + 1
    }
  }
}

#Goal difference
pointdata$gFor <- with(pointdata, 0)
pointdata$gAgainst <- with(pointdata, 0)
for (i in 1:nrow(pointdata)){
  for (j in 1:nrow(matchdata)){
    if (matchdata$HomeTeam[j] == pointdata$team[i]){
      pointdata$gFor[i] <- pointdata$gFor[i] + matchdata$FTHG[j]
      pointdata$gAgainst[i] <- pointdata$gAgainst[i] + matchdata$FTAG[j]}
    else if (matchdata$AwayTeam[j] == pointdata$team[i]){
      pointdata$gFor[i] <- pointdata$gFor[i] + matchdata$FTAG[j]
      pointdata$gAgainst[i] <- pointdata$gAgainst[i] + matchdata$FTHG[j]
    }
  }
}
pointdata$gDiff <- with(pointdata, gFor - gAgainst)

#Final Table
pointdata$ppg <- with(pointdata, 0)
for (i in 1:nrow(pointdata)){
  pointdata$ppg[i] <- round(pointdata$points[i] / pointdata$games[i],3)
}
#Order, highest to lowest PPG
pointdata <- pointdata[order(-pointdata$ppg),]
#Positions
rownames(pointdata) <- 1:nrow(pointdata)
pointdata





