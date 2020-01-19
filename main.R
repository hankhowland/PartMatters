library("tidyverse")
plays_simple <- X2017_18_pbp[-c(1,2,4,6,7,10:30,33,34)]

plays_simple$mins <- plays_simple$PCTIMESTRING
plays_simple$secs <- plays_simple$PCTIMESTRING
plays_simple$mins <- substr(plays_simple$mins,1,2)
plays_simple$secs <- substr(plays_simple$secs,4,5)
plays_simple <- plays_simple[which(plays_simple$EVENTMSGTYPE == 1 | plays_simple$EVENTMSGTYPE == 2 | plays_simple$EVENTMSGTYPE == 5), ]

for (y in seq_len(dim(plays_simple)[1]-1)){
  print(y)
  if (is.na(plays_simple$SCOREMARGIN[y+1]) & plays_simple$GAME_ID[y+1] == plays_simple$GAME_ID[y]){
    plays_simple$SCOREMARGIN[y+1] <- plays_simple$SCOREMARGIN[y]
  }
}

plays_simple <- plays_simple[which(plays_simple$PERIOD == 1 | plays_simple$PERIOD == 2 | plays_simple$PERIOD == 3 | plays_simple$PERIOD == 4), ]

for (y in seq_len(dim(plays_simple)[1]-1)){
  print(y)
  if (plays_simple$PERIOD[y+1] != plays_simple$PERIOD[y]) {
    plays_simple$mins[y+1] <- 12
  }
}

plays_simple$ID <- seq.int(nrow(plays_simple))

plays_simple <- plays_simple[which(plays_simple$mins[plays_simple$ID +1] != plays_simple$mins[plays_simple$ID]), ]

#######translating to rows being individual games and columns being the score every minute######
plays_simple$ID <- seq.int(nrow(plays_simple))

games <- plays_simple[which(plays_simple$GAME_ID[plays_simple$ID +1] != plays_simple$GAME_ID[plays_simple$ID]), ]

games$result <- games$SCOREMARGIN
games$fourone <- games$SCOREMARGIN
test <- games
test <- test[-c(1,3,4,5,7,8,9)]
#fourth quarter one minute left
for (y in seq_len(dim(plays_simple)[1]-1)){
  print(y)
  for (i in seq_len(dim(games)[1]-1)){
    if (plays_simple$GAME_ID[y] == games$GAME_ID[i] & plays_simple$PERIOD[y] == 4 & plays_simple$mins[y] == "01"){
      test$fourone[i] <- plays_simple$SCOREMARGIN[y]
    }
  }
}

#fourth quarter six minutes left
for (y in seq_len(dim(plays_simple)[1]-1)){
  print(y)
  for (i in seq_len(dim(games)[1]-1)){
    if (plays_simple$GAME_ID[y] == games$GAME_ID[i] & plays_simple$PERIOD[y] == 4 & plays_simple$mins[y] == "06"){
      test$foursix[i] <- plays_simple$SCOREMARGIN[y]
    }
  }
}

#fourth 12 minutes left
test$fourtwelve <- test$foursix
for (y in seq_len(dim(plays_simple)[1]-1)){
  print(y)
  for (i in seq_len(dim(games)[1]-1)){
    if (plays_simple$GAME_ID[y] == games$GAME_ID[i] & plays_simple$PERIOD[y] == 4 & plays_simple$mins[y] == "12"){
      test$fourtwelve[i] <- plays_simple$SCOREMARGIN[y]
    }
  }
}

#third 6 minutes left
for (y in seq_len(dim(plays_simple)[1]-1)){
  print(y)
  for (i in seq_len(dim(games)[1]-1)){
    if (plays_simple$GAME_ID[y] == games$GAME_ID[i] & plays_simple$PERIOD[y] == 3 & plays_simple$mins[y] == "06"){
      test$thirdsix[i] <- plays_simple$SCOREMARGIN[y]
    }
  }
}

#third 12 minutes left
for (y in seq_len(dim(plays_simple)[1]-1)){
  print(y)
  for (i in seq_len(dim(games)[1]-1)){
    if (plays_simple$GAME_ID[y] == games$GAME_ID[i] & plays_simple$PERIOD[y] == 3 & plays_simple$mins[y] == "12"){
      test$thirdtwelve[i] <- plays_simple$SCOREMARGIN[y]
    }
  }
}

#2nd 6 minutes left
for (y in seq_len(dim(plays_simple)[1]-1)){
  print(y)
  for (i in seq_len(dim(games)[1]-1)){
    if (plays_simple$GAME_ID[y] == games$GAME_ID[i] & plays_simple$PERIOD[y] == 2 & plays_simple$mins[y] == "06"){
      test$secondsix[i] <- plays_simple$SCOREMARGIN[y]
    }
  }
}

#2nd 12 minutes left
for (y in seq_len(dim(plays_simple)[1]-1)){
  print(y)
  for (i in seq_len(dim(games)[1]-1)){
    if (plays_simple$GAME_ID[y] == games$GAME_ID[i] & plays_simple$PERIOD[y] == 2 & plays_simple$mins[y] == "12"){
      test$secondtwelve[i] <- plays_simple$SCOREMARGIN[y]
    }
  }
}

#1st 6 minutes left
for (y in seq_len(dim(plays_simple)[1]-1)){
  print(y)
  for (i in seq_len(dim(games)[1]-1)){
    if (plays_simple$GAME_ID[y] == games$GAME_ID[i] & plays_simple$PERIOD[y] == 1 & plays_simple$mins[y] == "06"){
      test$firstsix[i] <- plays_simple$SCOREMARGIN[y]
    }
  }
}

#1st 0 minutes left: 0-0

#################making TIe into 0#################
test$result[which(test$result == 'TIE')] <- 0
test$foursix[which(test$foursix == 'TIE')] <- 0
test$fourtwelve[which(test$fourtwelve == 'TIE')] <- 0
test$thirdsix[which(test$thirdsix == 'TIE')] <- 0
test$thirdtwelve[which(test$thirdtwelve == 'TIE')] <- 0
test$secondsix[which(test$secondsix == 'TIE')] <- 0
test$secondtwelve[which(test$secondtwelve== 'TIE')] <- 0
test$firstsix[which(test$firstsix == 'TIE')] <- 0


#########translating to score differentials#######
test$firstsixd <- as.numeric(test$firstsix)
test$secondtwelved <- as.numeric(test$secondtwelve) - as.numeric(test$firstsix)
test$secondsixd <- as.numeric(test$secondsix) - as.numeric(test$secondtwelve)
test$thirdtwelved <- as.numeric(test$thirdtwelve) - as.numeric(test$secondsix)
test$thirdsixd <- as.numeric(test$thirdsix) - as.numeric(test$thirdtwelve)
test$fourthtwelved <- as.numeric(test$fourtwelve) - as.numeric(test$thirdsix)
test$fourthsixd <- as.numeric(test$foursix) - as.numeric(test$fourtwelve)
test$fourthzerod <- as.numeric(test$result) - as.numeric(test$foursix)

########making result into win or lose######
test <- test[which(test$result != 0), ]
test$winorlose <- test$result
test$winorlose[which(as.numeric(test$winorlose) < 0)] <- 'Loss'
test$winorlose[which(as.numeric(test$winorlose) > 0)] <- 'Win'

####logistic model of score differential in each 6 minute period on winning the game#######
firstsixmodel <- glm((winorlose == 'Win') ~ firstsixd, data = test, family = "binomial")
secondsixmodel <- glm((winorlose == 'Win') ~ secondsixd, data = test, family = "binomial")
secondtwelvemodel <- glm((winorlose == 'Win') ~ secondtwelved, data = test, family = "binomial")
thirdtwelvemodel <- glm((winorlose == 'Win') ~ thirdtwelved, data = test, family = "binomial")
fourthtwelvemodel <- glm((winorlose == 'Win') ~ fourthtwelved, data = test, family = "binomial")
thirdsixmodel <- glm((winorlose == 'Win') ~ thirdsixd, data = test, family = "binomial")
fourthsixmodel <- glm((winorlose == 'Win') ~ fourthsixd, data = test, family = "binomial")
fourthzeromodel <- glm((winorlose == 'Win') ~ fourthzerod, data = test, family = "binomial")

summary(firstsixmodel)
summary(secondtwelvemodel)
summary(secondsixmodel)
summary(thirdtwelvemodel)
summary(thirdsixmodel)
summary(fourthtwelvemodel)
summary(fourthsixmodel)
summary(fourthzeromodel)

####logistic models on samples limited to close games
fourthzeroclose <- test[which(abs(as.numeric(test$foursix)) <= 5), ]
fourthzeroclosemodel <- glm((winorlose == 'Win') ~ fourthzerod, data = fourthzeroclose, family = "binomial")
summary(fourthzeroclosemodel)

fourthsixclose <- test[which(abs(as.numeric(test$fourtwelve)) <= 5), ]
fourthsixclosemodel <- glm((winorlose == 'Win') ~ fourthsixd, data = fourthsixclose, family = "binomial")
summary(fourthsixclosemodel)

fourthtwelveclose <- test[which(abs(as.numeric(test$thirdsix)) <= 5), ]
fourthtwelveclosemodel <- glm((winorlose == 'Win') ~ fourthtwelved, data = fourthtwelveclose, family = "binomial")
summary(fourthtwelveclosemodel)

thirdsixclose <- test[which(abs(as.numeric(test$thirdtwelve)) <= 5), ]
thirdsixclosemodel <- glm((winorlose == 'Win') ~ thirdsixd, data = thirdsixclose, family = "binomial")
summary(thirdsixclosemodel)

thirdtwelveclose <- test[which(abs(as.numeric(test$secondsix)) <= 5), ]
thirdtwelveclosemodel <- glm((winorlose == 'Win') ~ thirdtwelved, data = thirdtwelveclose, family = "binomial")
summary(thirdtwelveclosemodel)

secondsixclose <- test[which(abs(as.numeric(test$secondtwelve)) <= 5), ]
secondsixclosemodel <- glm((winorlose == 'Win') ~ secondsixd, data = secondsixclose, family = "binomial")
summary(secondsixclosemodel)

secondtwelveclose <- test[which(abs(as.numeric(test$firstsix)) <= 5), ]
secondtwelveclosemodel <- glm((winorlose == 'Win') ~ secondtwelved, data = secondtwelveclose, family = "binomial")
summary(secondtwelveclosemodel)

    #firstsixclose == firstsixnormal because the game always starts 0-0

#####different periods in the 4th quarter (when to sub in crunch time lineup)######

for (y in seq_len(dim(plays_simple)[1]-1)){
  print(y)
  for (i in seq_len(dim(games)[1]-1)){
    if (plays_simple$GAME_ID[y] == games$GAME_ID[i] & plays_simple$PERIOD[y] == 4 & plays_simple$mins[y] == "11"){
      test$feleven[i] <- plays_simple$SCOREMARGIN[y]
    }
  }
}

for (y in seq_len(dim(plays_simple)[1]-1)){
  print(y)
  for (i in seq_len(dim(games)[1]-1)){
    if (plays_simple$GAME_ID[y] == games$GAME_ID[i] & plays_simple$PERIOD[y] == 4 & plays_simple$mins[y] == "10"){
      test$ften[i] <- plays_simple$SCOREMARGIN[y]
    }
  }
}

for (y in seq_len(dim(plays_simple)[1]-1)){
  print(y)
  for (i in seq_len(dim(games)[1]-1)){
    if (plays_simple$GAME_ID[y] == games$GAME_ID[i] & plays_simple$PERIOD[y] == 4 & plays_simple$mins[y] == "09"){
      test$fnine[i] <- plays_simple$SCOREMARGIN[y]
    }
  }
}

for (y in seq_len(dim(plays_simple)[1]-1)){
  print(y)
  for (i in seq_len(dim(games)[1]-1)){
    if (plays_simple$GAME_ID[y] == games$GAME_ID[i] & plays_simple$PERIOD[y] == 4 & plays_simple$mins[y] == "08"){
      test$feight[i] <- plays_simple$SCOREMARGIN[y]
    }
  }
}

for (y in seq_len(dim(plays_simple)[1]-1)){
  print(y)
  for (i in seq_len(dim(games)[1]-1)){
    if (plays_simple$GAME_ID[y] == games$GAME_ID[i] & plays_simple$PERIOD[y] == 4 & plays_simple$mins[y] == "07"){
      test$fseven[i] <- plays_simple$SCOREMARGIN[y]
    }
  }
}

for (y in seq_len(dim(plays_simple)[1]-1)){
  print(y)
  for (i in seq_len(dim(games)[1]-1)){
    if (plays_simple$GAME_ID[y] == games$GAME_ID[i] & plays_simple$PERIOD[y] == 4 & plays_simple$mins[y] == "05"){
      test$ffive[i] <- plays_simple$SCOREMARGIN[y]
    }
  }
}

for (y in seq_len(dim(plays_simple)[1]-1)){
  print(y)
  for (i in seq_len(dim(games)[1]-1)){
    if (plays_simple$GAME_ID[y] == games$GAME_ID[i] & plays_simple$PERIOD[y] == 4 & plays_simple$mins[y] == "04"){
      test$ffour[i] <- plays_simple$SCOREMARGIN[y]
    }
  }
}

for (y in seq_len(dim(plays_simple)[1]-1)){
  print(y)
  for (i in seq_len(dim(games)[1]-1)){
    if (plays_simple$GAME_ID[y] == games$GAME_ID[i] & plays_simple$PERIOD[y] == 4 & plays_simple$mins[y] == "03"){
      test$fthree[i] <- plays_simple$SCOREMARGIN[y]
    }
  }
}

for (y in seq_len(dim(plays_simple)[1]-1)){
  print(y)
  for (i in seq_len(dim(games)[1]-1)){
    if (plays_simple$GAME_ID[y] == games$GAME_ID[i] & plays_simple$PERIOD[y] == 4 & plays_simple$mins[y] == "02"){
      test$ftwo[i] <- plays_simple$SCOREMARGIN[y]
    }
  }
}

for (y in seq_len(dim(plays_simple)[1]-1)){
  print(y)
  for (i in seq_len(dim(games)[1]-1)){
    if (plays_simple$GAME_ID[y] == games$GAME_ID[i] & plays_simple$PERIOD[y] == 4 & plays_simple$mins[y] == "01"){
      test$fone[i] <- plays_simple$SCOREMARGIN[y]
    }
  }
}
