
## reading in data ---------
player_data = read.csv('./data/player_data.csv')
player_data$career_len = player_data$year_end - player_data$year_start
Seasons_Stats = read.csv('./data/Seasons_Stats.csv')

## subseting the data that way it only presents names between the career range of 5 and 15
player_sub = subset(player_data, career_len >= 5 & career_len <= 15)

## subsetting the data that way it only presents duplicates and removes those who only played one season that way its viable data 
Seasons_sub = Seasons_Stats[duplicated(Seasons_Stats$Player),]

## initial exploration ------
FGP <- (Seasons_Stats$FG.)
Age <- (Seasons_Stats$Age)
Two_PFGP <- (Seasons_Stats$X2P.)
Two_PFGP <- (Seasons_Stats$X3P.)
summary(player_data)
summary(Seasons_Stats)
plot(FGP, Age)
plot(Two_PFGP, Age)
plot(Three_PFGP, Age)


## vector creations to see set list of names and careers aside from the view in player_sub (players with career lengths of 5-15 years)
CareerVector <- ((player_data$year_end - player_data$year_start) + 1)
Names <- (player_data$name)
CFrame <- data.frame(CareerVector, Names)
summary(CFrame)

## subsetting the vector to only present those within career lengths and presents a list to view
subset(CFrame, CareerVector <= 15)
subset(CFrame, CareerVector >= 5)

CFrame3 <- subset(CFrame, CareerVector <= 15)
View(CFrame3)

## Aquired list of valid names for tests

CFrame4 <- subset(CFrame3, CareerVector >= 5 )
View(CFrame4)

FinalNames <- CFrame4$Names

## Fit models with appropriate data with only repeats that have a career length of 5-15 +/- 2 years

## scatter plot between AGE and Field Goal %
ggplot(Seasons_sub, aes(x= Seasons_sub$Age , y= Seasons_sub$`FG.`)) +
  geom_point(size=1, shape=23) +
  geom_smooth(method=lm) +
  xlab("Age") + ylab("FG%") + 
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.1), linetype=2,color='blue') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.5), linetype=2,color='red') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.9), linetype=2,color='blue') +
  geom_line(stat='summary',fun.y = mean, color='green') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.99), linetype=2,color='orange')
 
## scatter plot between AGE and Free Throw %
ggplot(Seasons_sub, aes(x= Seasons_sub$Age , y= Seasons_sub$`FT.`)) +
  geom_point(size=1, shape=1) +
  geom_smooth(method=lm) + 
  xlab("Age") + ylab("FT%") + 
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.1), linetype=2,color='blue') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.5), linetype=2,color='red') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.9), linetype=2,color='blue') +
  geom_line(stat='summary',fun.y = mean, color='green') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.99), linetype=2,color='orange')

## scatter plot between AGE and Assists %
ggplot(Seasons_sub, aes(x= Seasons_sub$Age , y= Seasons_sub$AST)) + 
  geom_point(size=1, shape=2) + 
  geom_smooth(method=lm) + 
  xlab("Age") + ylab("AST") + 
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.1), linetype=2,color='blue') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.5), linetype=2,color='red') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.9), linetype=2,color='blue') +
  geom_line(stat='summary',fun.y = mean, color='green') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.99), linetype=2,color='orange')

## scatter plot between AGE and True Shooting %
ggplot(Seasons_sub, aes(x= Seasons_sub$Age , y= Seasons_sub$`TS.`)) + 
  geom_point(size=1, shape=3) + 
  geom_smooth(method=lm) + 
  xlab("Age") + ylab("TS%") + 
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.1), linetype=2,color='blue') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.5), linetype=2,color='red') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.9), linetype=2,color='blue') +
  geom_line(stat='summary',fun.y = mean, color='green') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.99), linetype=2,color='orange')

## scatter plot between AGE and Points
ggplot(Seasons_sub, aes(x= Seasons_sub$Age , y= Seasons_sub$PTS)) + 
  geom_point(size=1, shape=20) + 
  geom_smooth(method=lm) + 
  xlab("Age") + ylab("PTS") +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.1), linetype=2,color='blue') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.5), linetype=2,color='red') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.9), linetype=2,color='blue') +
  geom_line(stat='summary',fun.y = mean, color='green') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.99), linetype=2,color='orange')

## scatter plot between AGE and Games
ggplot(Seasons_sub, aes(x= Seasons_sub$Age , y= Seasons_sub$G)) + 
  geom_point(size=1, shape=22) + 
  geom_smooth(method=lm) + 
  xlab("Age") + ylab("Games") + 
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.1), linetype=2,color='blue') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.5), linetype=2,color='red') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.9), linetype=2,color='blue') +
  geom_line(stat='summary',fun.y = mean, color='green') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.99), linetype=2,color='orange')

## scatter plot between AGE and Personal Fouls
ggplot(Seasons_sub, aes(x= Seasons_sub$Age , y= Seasons_sub$PF)) + 
  geom_point(size=1, shape=24) + 
  geom_smooth(method=lm) + 
  xlab("Age") + ylab("PF") +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.1), linetype=2,color='blue') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.5), linetype=2,color='red') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.9), linetype=2,color='blue') +
  geom_line(stat='summary',fun.y = mean, color='green') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.99), linetype=2,color='orange')

## scatter plot between AGE and the log of Field Goal % just seeing if there would have been much of a difference when taking the log of the data
FGP_log <- log(Seasons_sub$`FG.`)

ggplot(Seasons_sub, aes(x= Seasons_sub$Age , y= FGP_log)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm) +
  xlab("Age") + ylab("FGP_log") +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.1), linetype=2,color='blue') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.5), linetype=2,color='red') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.9), linetype=2,color='blue') +
  geom_line(stat='summary',fun.y = mean, color='green') +
  geom_line(stat='summary',fun.y=quantile,fun.args = list(0.99), linetype=2,color='orange')

## simple histogram to see the frequency and distribution between AGE and Field Goal %
hist(Seasons_sub$`FG.`,
     xlab="FG%", 
     col = "red"), 

## simple histogram to see the frequency and distribution between AGE and True Shooting %
hist(Seasons_sub$`TS.`, 
     xlab ="TS%", 
     col = "blue")

## simple boxplots between AGE and Field Goal %
boxplot(Seasons_sub$`FG.` ~ Seasons_sub$Age, 
        data = Seasons_sub, 
        ylab="FG%", xlab="Age")

## simple boxplots between AGE and Free Throw %
boxplot(Seasons_sub$`FT.` ~ Seasons_sub$Age, 
        data = Seasons_sub, 
        ylab="FT%", xlab="Age")

## simple boxplots between AGE and True Shooting %
boxplot(Seasons_sub$`TS.` ~ Seasons_sub$Age, 
        data = Seasons_sub, 
        ylab="TS%", xlab="Age")

## simple boxplots between AGE and Points
boxplot(Seasons_sub$PTS ~ Seasons_sub$Age, 
        data = Seasons_sub, 
        ylab="PTS", xlab="Age")

## simple boxplots between AGE and Games
boxplot(Seasons_sub$G ~ Seasons_sub$Age, 
        data = Seasons_sub, 
        ylab="Games", xlab="Age")

## simple boxplots between AGE and Assists
boxplot(Seasons_sub$AST ~ Seasons_sub$Age, 
        data = Seasons_sub, 
        ylab="AST", xlab="Age")

## simple boxplots between AGE and Personal Fouls
boxplot(Seasons_sub$PF ~ Seasons_sub$Age, 
        data = Seasons_sub, 
        ylab="PF", xlab="Age")

## Data analysis with appropriate data with only repeats that have a career length of 5-15 years +/- 2 years

## Data analysis between Age and Field Goal %
FGvsAGE.lm <- lm(Seasons_sub$`FG.` ~ Seasons_sub$Age, 
                 data = Seasons_sub)

summary(FGvsAGE.lm)
anova(FGvsAGE.lm)

## Data analysis between Age and Free Throw %
FTvsAGE.lm <- lm(Seasons_sub$`FT.` ~ Seasons_sub$Age, 
                 data = Seasons_sub)

summary(FTvsAGE.lm)
anova(FTvsAGE.lm)

## Data analysis between Age and True Shooting %
TSvsAGE.lm <- lm(Seasons_sub$`TS.` ~ Seasons_sub$Age, 
                 data = Seasons_sub)

summary(TSvsAGE.lm)
anova(TSvsAGE.lm)

## Data analysis between Age and Points
PTSvsAGE.lm <- lm(Seasons_sub$PTS ~ Seasons_sub$Age,
                  data = Seasons_sub)

summary(PTSvsAGE.lm)
anova(PTSvsAGE.lm)

## Data analysis between Age and Games
GamesvsAGE.lm <- lm(Seasons_sub$G ~ Seasons_sub$Age, 
                    data = Seasons_sub)

summary(GamesvsAGE.lm)
anova(GamesvsAGE.lm)

## Data analysis between Age and Assists
ASTvsAGE.lm <- lm(Seasons_sub$AST ~ Seasons_sub$Age, 
                  data = Seasons_sub)

summary(ASTvsAGE.lm)
anova(ASTvsAGE.lm)

## Data analysis between Age and Personal Fouls
PFvsAGE.lm <- lm(Seasons_sub$PF ~ Seasons_sub$Age,
                 data = Seasons_sub)

summary(PFvsAGE.lm)
anova(PFvsAGE.lm)

## Analysis of R Squared values and  further Correlation tests to test for an association/correlation between paired samples.

## correlation tests between Age and True Shooting %
cor_AGEvsTS <- cor.test(Seasons_sub$Age, Seasons_sub$TS., 
                        method = "pearson")
cor_AGEvsTS

## correlation tests between Age and Field Goal %
cor_AGEvsFG <- cor.test(Seasons_sub$Age, Seasons_sub$FG., 
                        method = "pearson")
cor_AGEvsFG

## correlation tests between Age and Personal Fouls
cor_AGEvsPF <- cor.test(Seasons_sub$Age, Seasons_sub$PF, 
                        method = "pearson")
cor_AGEvsPF

## correlation tests between Age and Assists
cor_AGEvsAST <- cor.test(Seasons_sub$Age, Seasons_sub$AST, 
                        method = "pearson")
cor_AGEvsAST

## correlation tests between Age and Points
cor_AGEvsPTS <- cor.test(Seasons_sub$Age, Seasons_sub$PTS, 
                        method = "pearson")
cor_AGEvsPTS

## correlation tests between Age and Games
cor_AGEvsGames <- cor.test(Seasons_sub$Age, Seasons_sub$G, 
                        method = "pearson")
cor_AGEvsGames

## correlation tests between Age and Free Throw %
cor_AGEvsFT <- cor.test(Seasons_sub$Age, Seasons_sub$FT., 
                        method = "pearson")
cor_AGEvsFT



