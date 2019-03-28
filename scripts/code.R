
# reading in data ---------
player_data = read.csv('./data/player_data.csv')
player_data$career_len = player_data$year_end - player_data$year_start

player_sub = subset(player_data, career_len >= 5 & career_len <= 15)

# initial exploration ------

summary(player_data)
summary(Seasons_Stats)
plot(FGP, Age)
plot(Two_PFGP, Age)
plot(Three_PFGP, Age)

}

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

```

```{r Aquired list of valid names for tests}

CFrame4 <- subset(CFrame3, CareerVector >= 5 )
View(CFrame4)

FinalNames <- CFrame4$Names

```

