
# reading in data ---------
player_data = read.csv('./data/player_data.csv')
player_data$career_len = player_data$year_end - player_data$year_start

player_sub = subset(player_data, career_len >= 5 & career_len <= 15)

Seasons_Stats = read.csv('./data/Seasons_Stats.csv')
Seasons_sub = Seasons_Stats[duplicated(Seasons_Stats$Player),]

common <- intersect(player_sub$name, Seasons_Stats$Player)  
player_sub$name[common,] # give you common rows in data frame 1  
Seasons_Stats$Player[common,] # give you common rows in data frame 2

# initial exploration ------

summary(player_data)
summary(Seasons_Stats)
plot(FGP, Age)
plot(Two_PFGP, Age)
plot(Three_PFGP, Age)


## Vector creations to see set list of names and careers aside from the view in player_sub (players with career lengths of 5-15 years)
CareerVector <- ((player_data$year_end - player_data$year_start) + 1)
Names <- (player_data$name)
CFrame <- data.frame(CareerVector, Names)
summary(CFrame)

MaxCareer <- which(CFrame[2] <= 15)
MinCareer <- which(CFrame[2] >= 5)
AvgCar <- c(MaxCareer, MinCareer)

subset(CFrame, CareerVector <= 15)
subset(CFrame, CareerVector >= 5)

CFrame3 <- subset(CFrame, CareerVector <= 15)
View(CFrame3)

## Aquired list of valid names for tests

CFrame4 <- subset(CFrame3, CareerVector >= 5 )
View(CFrame4)

FinalNames <- CFrame4$Names

## Fit models with appropriate data with only repeats that have a career length of 5-15 years

ggplot(Seasons_sub, aes(x= Seasons_sub$Age , y= Seasons_sub$`FG.`)) +
  geom_point(size=1, shape=23) +
  geom_smooth(method=lm) +
  xlab("Age") + ylab("FG%")

ggplot(Seasons_sub, aes(x= Seasons_sub$Age , y= Seasons_sub$`FT.`)) +
  geom_point(size=1, shape=1) +
  geom_smooth(method=lm) + 
  xlab("Age") + ylab("FT%")

ggplot(Seasons_sub, aes(x= Seasons_sub$Age , y= Seasons_sub$AST)) + 
  geom_point(size=1, shape=2) + 
  geom_smooth(method=lm) + 
  xlab("Age") + ylab("AST")

ggplot(Seasons_sub, aes(x= Seasons_sub$Age , y= Seasons_sub$`TS.`)) + 
  geom_point(size=1, shape=3) + 
  geom_smooth(method=lm) + 
  xlab("Age") + ylab("TS%")

ggplot(Seasons_sub, aes(x= Seasons_sub$Age , y= Seasons_sub$PTS)) + 
  geom_point(size=1, shape=20) + 
  geom_smooth(method=lm) + 
  xlab("Age") + ylab("PTS")

ggplot(Seasons_sub, aes(x= Seasons_sub$Age , y= Seasons_sub$G)) + 
  geom_point(size=1, shape=22) + 
  geom_smooth(method=lm) + 
  xlab("Age") + ylab("Games")

FGP_log <- log(Seasons_sub$`FG.`)
ggplot(Seasons_sub, aes(x= Seasons_sub$Age , y= FGP_log)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm) +
  xlab("Age") + ylab("FGP_log")

hist(Seasons_sub$`FG.`, xlab="FG%", col = "red") 
hist(Seasons_sub$`TS.`, xlab ="TS%", col = "blue")

boxplot(Seasons_sub$`FG.` ~ Seasons_sub$Age, data = Seasons_sub, ylab="FG%", xlab="Age")
boxplot(Seasons_sub$`FT.` ~ Seasons_sub$Age, data = Seasons_sub, ylab="FT%", xlab="Age")
boxplot(Seasons_sub$`TS.` ~ Seasons_sub$Age, data = Seasons_sub, ylab="TS%", xlab="Age")
boxplot(Seasons_sub$PTS ~ Seasons_sub$Age, data = Seasons_sub, ylab="PTS", xlab="Age")
boxplot(Seasons_sub$G ~ Seasons_sub$Age, data = Seasons_sub, ylab="Games", xlab="Age")
boxplot(Seasons_sub$AST ~ Seasons_sub$Age, data = Seasons_sub, ylab="AST", xlab="Age")

## Data analysis with appropriate data with only repeats that have a career length of 5-15 years

FGvsAGE.lm <- lm(Seasons_sub$`FG.` ~ Seasons_sub$Age, data = Seasons_sub)
summary(FGvsAGE.lm)

FTvsAGE.lm <- lm(Seasons_sub$`FT.` ~ Seasons_sub$Age, data = Seasons_sub)
summary(FTvsAGE.lm)

TSvsAGE.lm <- lm(Seasons_sub$`TS.` ~ Seasons_sub$Age, data = Seasons_sub)
summary(TSvsAGE.lm)

PTSvsAGE.lm <- lm(Seasons_sub$PTS ~ Seasons_sub$Age, data = Seasons_sub)
summary(PTSvsAGE.lm)

GamesvsAGE.lm <- lm(Seasons_sub$G ~ Seasons_sub$Age, data = Seasons_sub)
summary(GamesvsAGE.lm)

ASTvsAGE.lm <- lm(Seasons_sub$AST ~ Seasons_sub$Age, data = Seasons_sub)
summary(ASTvsAGE.lm)

#### Analysis of R Squared values and Discuss results



