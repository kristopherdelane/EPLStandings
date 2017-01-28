EPL_Standings<- function(funcDate, funcSeason){
  
  ##NOTICE
#    funcDate<-"09/1/2013";  ##UNCOMMENT TO TEST IF WORKS WITH OUT FUNCTION
#    funcSeason<- "2013/14";  ##UNCOMMENT TO TEST IF WORKS WITH OUT FUNCTION
  
#  Turn the funcSeason into a usable string for the URL.
funcDate<-funcDate
funcYear<- funcSeason
seasonStart<-substr(funcYear, 3, 4)
seasonEnd<-substr(funcYear, 6,7)
funcYear<-paste(seasonStart,seasonEnd, sep = "")

#you need the Data. Import the Data from the site based on season parameter.
EPL<-read.csv(paste("http://www.football-data.co.uk/mmz4281/", funcYear, "/E0.csv", sep = ""))

#Load Library Car for Reorder
library(car) 

#Format Date into the correct format
Date1<-EPL$Date

#Convert The date from European to American and Make the Year 4 digits
Date1<-as.Date(Date1, "%d/%m/%y")

#Modify the Base Data Frame Based on Date from Function
funcDate <- as.Date(funcDate, "%m/%d/%Y")   #Date from Fuction Modified
EPL$Date<-Date1
EPL<-EPL[EPL$Date<=funcDate,]

#A Touch of Bullet Proofing
if (nrow(EPL)==0){cat("\014"); stop("!*!*!*!*!*!*Date is before specified season began, please check and try again.*!*!*!*!*!*!")}


# Reset the variable Date 1 to reflect smaller Dataframe
Date1<-EPL$Date

Date2<-format.Date(Date1, "%m/%d/%Y")

# Build a Season Column Based on Which Season's Data is imported
Season1<-Date1[1]
Season2<-Date1[length(Date1)]
Season2<-format.Date(Season2, "%y")
Season1<-format.Date(Season1, "%Y")

#Make the Date Proper Form before Loop
EPL$Date<-Date2

#Create the season in (xxxx/xx) format
Season<- paste(Season1,"/",Season2)

#Build a Variable of Team Names for Loops and sort by Alphabet
TeamNameList<- unique(EPL$HomeTeam)
TeamNameList<-sort(TeamNameList, decreasing = FALSE)

#Create A dataframe for all my Information that I create.
EPLStandings <- data.frame( 
                            #Season = "x", Date = "x", 
                            TeamName = "x", 
                            Record = "x", HomeRec = "x", AwayRec = "x",
                            MatchesPlayed = "x", Points = "x", PPM = "x", PtPct = "x", 
                            GS = "x", GSM ="x", GA ="x", GAM = "x", 
                            Last10="x", 
                            Streak="x",
                            stringsAsFactors = FALSE)
EPLStandings <-EPLStandings [-1,]

#Initializing a few values for future use in loops
MatchesPlayed <- 0
TeamName <- NULL
HomeWins<-0
HomeTies<-0
HomeLosses<-0
AwayWins <-0
AwayLosses <-0
AwayTies <-0

# Set Match Results to Character type
EPL$FTR <- as.character(EPL$FTR)

#Loop for moving through the List that will run through each team in alphabetical order
for(i in 1 : length(TeamNameList))
  {
  #Creates a Variable of Team name for my Table
    TeamName <-TeamNameList[i] 
  #the number of goals scored by the team at home 
    HomeGoalsScored <- sum(EPL$FTHG[EPL$HomeTeam==TeamNameList[i]]) 
  #the number of goals allowed by the team at home
    HomeGoalsAllowed <- sum(EPL$FTAG[EPL$HomeTeam==TeamNameList[i]]) 
  #the number of goals scored by the team while Away 
    AwayGoalsScored <- sum(EPL$FTAG[EPL$AwayTeam==TeamNameList[i]])
  #the number of goals allowed by the team while away
    AwayGoalsAllowed <- sum(EPL$FTHG[EPL$AwayTeam==TeamNameList[i]]) 
    
  #The number of Home Wins
    HomeWins<-length(subset(EPL$FTR[EPL$HomeTeam==TeamNameList[i]], 
                            EPL$FTR[EPL$HomeTeam==TeamNameList[i]]=="H"))
  #The number of Home Losses
    HomeLosses <-length(subset(EPL$FTR[EPL$HomeTeam==TeamNameList[i]], 
                               EPL$FTR[EPL$HomeTeam==TeamNameList[i]]=="A"))
  #The number of Home Ties
    HomeTies <-length(subset(EPL$FTR[EPL$HomeTeam==TeamNameList[i]], 
                             EPL$FTR[EPL$HomeTeam==TeamNameList[i]]=="D"))
  #The Record at Home
    HomeRec <- paste(HomeWins, "-", HomeLosses, "-", HomeTies, sep = "")
    
  #The number of Away Wins
    AwayWins<-length(subset(EPL$FTR[EPL$AwayTeam==TeamNameList[i]], 
                            EPL$FTR[EPL$AwayTeam==TeamNameList[i]]=="A"))
  #The number of Away Losses
    AwayLosses <-length(subset(EPL$FTR[EPL$AwayTeam==TeamNameList[i]], 
                               EPL$FTR[EPL$AwayTeam==TeamNameList[i]]=="H"))
  #The number of Away Ties
    AwayTies <-length(subset(EPL$FTR[EPL$AwayTeam==TeamNameList[i]], 
                             EPL$FTR[EPL$AwayTeam==TeamNameList[i]]=="D"))
  #The Teams Record on the Road
    AwayRec <- paste(AwayWins, "-", AwayLosses, "-", AwayTies, sep = "")
    
  #Total Record to Date
    Record <- paste(HomeWins+AwayWins, "-", HomeLosses+AwayLosses, "-", HomeTies+AwayTies, sep = "")
    
  #The number of matches played     
    MatchesPlayed <- HomeWins+HomeTies+HomeLosses+AwayWins+AwayTies+AwayLosses
  
  #Teams Points Calculation
    Points <- (HomeWins+AwayWins)*3+HomeTies+AwayTies
  #Teams Points Per Match
    PPM <- Points/MatchesPlayed
  #Team Point Percentage
    PtPct <- Points/(3*MatchesPlayed)

  #Goals Scored
    GS <- HomeGoalsScored + AwayGoalsScored
  #Goals Scored Per Match
    GSM <- GS/MatchesPlayed
  #Goals Allowed
    GA <- AwayGoalsAllowed + HomeGoalsAllowed  
  #Goals Allowed Per Match
    GAM <- GA/MatchesPlayed
    
  #Create a Table with a Teams Final 10 Games
    EPLL10<-EPL[EPL$AwayTeam==TeamNameList[i] | EPL$HomeTeam==TeamNameList[i],] 
    EPLL10<-tail(EPLL10, n = 10)
  #Count the number of Ties
    L10Ties <-length(subset(EPLL10$FTR,EPLL10$FTR=="D"))
  #The number of Home Wins
    L10Wins<-length(subset(EPLL10$FTR[EPLL10$HomeTeam==TeamNameList[i]], 
                           EPLL10$FTR[EPLL10$HomeTeam==TeamNameList[i]]=="H"))
  #The number of Home Losses
    L10Losses <-length(subset(EPLL10$FTR[EPLL10$HomeTeam==TeamNameList[i]], 
                              EPLL10$FTR[EPLL10$HomeTeam==TeamNameList[i]]=="A"))
  #Add in Away Wins and Losses
    L10Wins<- L10Wins + length(subset(EPLL10$FTR[EPLL10$AwayTeam==TeamNameList[i]], 
                                      EPLL10$FTR[EPLL10$AwayTeam==TeamNameList[i]]=="A"))
    L10Losses <- L10Losses + length(subset(EPLL10$FTR[EPLL10$AwayTeam==TeamNameList[i]], 
                                           EPLL10$FTR[EPLL10$AwayTeam==TeamNameList[i]]=="H"))
  #Create the Record
    Last10 <- paste(L10Wins, "-", L10Losses, "-", L10Ties, sep = "")
    
  #Date of last game played - Not Needed for Assignment
  #  Date <- EPLL10$Date[nrow(EPLL10)]
    
    
  # I need to build a dataframe for each teams game on the season in order to see... 
  # how many games they had won lose or draw at home and away combine and organized by date
    EPLStreak<-EPL[EPL$AwayTeam==TeamNameList[i] | EPL$HomeTeam==TeamNameList[i],]
  #Change the H/A/D to W L D using the Reorder fuction
    EPLStreak$FTR[EPLStreak$AwayTeam==TeamNameList[i]]<-recode(EPLStreak$FTR[EPLStreak$AwayTeam==TeamNameList[i]], "'A'='W';'H'='L'"  )
    EPLStreak$FTR[EPLStreak$HomeTeam==TeamNameList[i]]<-recode(EPLStreak$FTR[EPLStreak$HomeTeam==TeamNameList[i]], "'A'='L';'H'='W'" )
  #I need the value from the last record of EPLStreak$FTR.
    StreakVariable <- EPLStreak$FTR[nrow(EPLStreak)]
  #Counter for Streak Total
    StreakCount<- 1
  #Loop for Streaking
    
    while(StreakVariable==EPLStreak$FTR[(nrow(EPLStreak)-StreakCount)]){
        StreakCount <- StreakCount+1 
        if((nrow(EPLStreak)-StreakCount)==0) break
        }
  #Assign results to Streak   
    Streak<- paste(StreakVariable, StreakCount, sep = "")
    

# WRITE IT DOWN!!!!!!!!!!!!!!! in a dataframe :) 
    
    EPLStandings <- rbind(EPLStandings, data.frame( 
                                #Season = Season, Date=Date, 
                                TeamName = TeamName, 
                                Record = Record, HomeRec = HomeRec, AwayRec = AwayRec,
                                MatchesPlayed = MatchesPlayed, Points = Points, PPM = PPM, 
                                PtPct = PtPct, GS = GS, GSM = GSM, GA = GA, GAM = GAM, 
                                Last10 = Last10, Streak = Streak))
  
#Reseting important values for next round in Loop    
    MatchesPlayed <- 0
    TeamName <- NULL
    HomeWins<-0
    HomeTies<-0
    HomeLosses<-0
    AwayWins <-0
    AwayLosses <-0
    AwayTies <-0

}
#Sort by points per match, then wins, then goals scored per match 
#and finally ascending according to goals allowed per match
EPLStandings<-EPLStandings[order(-EPLStandings$PPM, EPLStandings$Record, 
                                 -EPLStandings$GS, EPLStandings$GA),]
#Show the Results
#View(EPLStandings)

#Clear the Console ( Thanks Google )
cat("\014")


return (EPLStandings)

}
#  EPL_Standings("04/25/2013", "2014/15")
#  EPL_Standings("04/25/2015", "2014/15")
#  EPL_Standings("09/01/2014", "2014/15")
  
