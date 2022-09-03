install.packages("dplyr") # für rbind
install.packages("ggplot2") #für ggplot
library("dplyr") #für rbind
library("ggplot2") #für ggplot

# CRISP-DM - Schritt 1: Business Understanding


# Spielergebnisse im Rahmen von Turnieren des Beachvolleyball-Weltverbands 2000 - 2019
# Ein Team besteht aus 2 Spielern (Pro Spieler eine Spalte mit Informationen)
# Ein Team gewinnt oder verliert (Gewinner- und Verliererspalten)
# Ergebnisspalte mit den Satzergebnissen z.B. 10-21, 21-12, 15-10 oder 0-1 retired?!?


# CRISP-DM - Schritt 2: Data Understanding

# Schritt 2.1: Daten laden

beach1 <- read.csv("C:/Users/Mark/Karriere/2021-2024_Master Big Data/2_Semester/Angewandte Programmierung/Sonstige Beteiligung/vb_matches.csv", header = TRUE)

# Schritt 2.2.: Erster Blick auf die Daten

head(beach1)
tail(beach1)
summary(beach1)
str(beach1)

# CRISP-DM - Schritt 3: Data Preparation

# Schritt 3.1: Nur relevante Turnierdaten herausfiltern

beach2 <- subset(beach1, beach1$circuit=="FIVB" 
                 & beach1$gender=="M" 
                 & beach1$bracket!="Bronze Medal"
                 & beach1$bracket!="Qualifier Bracket" 
                 & beach1$bracket!="Qualifier Playoff" 
                 & beach1$bracket!= "Country Quota Matches" 
                 & beach1$bracket!= "Lucky Losers")

# Schritt 3.2: Spalte mit Sieg = 1 & Niederlage = 0
beach2$win <- 1
beach2$lost <- 0

# Schritt 3.3: Teams mit Teamdaten erzeugen
beach2$wteam <- paste(beach2$w_player1, beach2$w_player2, sep="/")
beach2$avg_w_age <- (beach2$w_p1_age + beach2$w_p2_age)/2
beach2$avg_w_heightcm <- ((beach2$w_p1_hgt + beach2$w_p2_hgt)/2)*2.54

beach2$lteam <- paste(beach2$l_player1, beach2$l_player2, sep="/")
beach2$avg_l_age <- (beach2$l_p1_age + beach2$l_p2_age)/2
beach2$avg_l_heightcm <- ((beach2$l_p1_hgt + beach2$l_p2_hgt)/2)*2.54

# Schritt 3.4: Eine Datentabelle nur mit Siegerdaten

beach2winner <- subset(beach2, select=c(year, wteam, w_p1_country,avg_w_age, avg_w_heightcm, win))

# Schritt 3.5: Eine Datentabelle nur mit Verliererdaten

beach2loser <- subset(beach2, select=c(year, lteam, l_p1_country,avg_l_age, avg_l_heightcm, lost))

# Schritt 3.6: Gleiche Bezeichnung der Überschriften
names(beach2winner) <- c("year", "team", "country", "age", "height", "win_lost")
names(beach2loser) <- c("year", "team", "country", "age", "height", "win_lost")

# Schritt 3.7: Daten zusammenfassen in einem Datensatz untereinander
beach3 <- rbind(beach2winner, beach2loser)

# CRISP-DM - Schritt 4: Modelling

# Frage 4.1: Was waren die erfolgreichsten Teams?

topteams <- beach3 %>% 
  group_by (team, country) %>%
  summarize(height = mean(height) ,wins = sum(win_lost), games = n())

topteams$winning_rate <- topteams$wins / topteams$games

topteams50 <- topteams %>% filter(games>50)

# Frage 4.2: Wie ist die Entwicklung der deutschen Teams?

country <- beach3 %>% 
  group_by (country, year) %>%
  summarize(wins = sum(win_lost), games = n())

germany <- country %>% filter(country == "Germany")

ggplot(data=germany, aes(x=germany$year))+
  geom_line(linetype=1, aes(y=germany$wins), color="green") +
  geom_line(linetype=1, aes(y=germany$games)) +
  ggtitle ("Germany Beachvolleyball FIVB Wins & Games 2009 - 2019")+
  xlab("Year") + ylab("Wins")
  
# Wie ist die Entwicklung der Siegquote der deutschen Teams?
  
germany$winning_quote <- germany$wins / germany$games
  
ggplot(data=germany, aes(x=germany$year))+
  geom_line(linetype=1, aes(y=germany$winning_quote), color="blue") +
  ggtitle ("Germany Beachvolleyball FIVB Winning Rate 2000 - 2019")+
  xlab("Year") + ylab("Winning Rate")

# Frage 4.3: Was machen gute Teams aus?

# Frage 4.3.1: Exkurs Histogram Altersverteilung

hist(beach3$age, freq = FALSE, xlab="Age")
mean(beach3$age)

# Frage 4.3.1: Sind erfahrenere Teams (über 30 Jahre) erfolgreicher als junge Teams?
beach3$age_group <- ifelse(beach3$age>30, "experienced", "young & wild")

t.test(win_lost~age_group, data = beach3) 

#Ergebnis: Ja, höchste Signifikanz 
# Erfahren: 0.53 vs. Young & Wild: 0.48


# Frage 4.3.2: Sind größere Teams erfolgreicher als kleinere Teams? 
summary(topteams50)

summary(lm(topteams50$winning_rate~topteams100$height, data=topteams100))
#P-Wert: Hohe Signifikanz 0.00161
#Regressionskoeffizenz: 0.008314 --> 1 cm größer erhöht die Siegquote um 0,8 %
#Bestimmtheitsmaß:0,05393


# CRISP-DM - Schritt 5: Evaluation

#Siehe PowerPoint

# CRISP-DM - Schritt 6: Deployment

#Siehe PowerPoint


