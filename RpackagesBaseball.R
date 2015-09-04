#############################################
# Packages for Analyzing Baseball Data in R #
# Bill Petti                                #
# September 2015                            #
#############################################

# load pacman, which will make loading all the other packages extremely easy

require(pacman)

# use the p_load function to load the other packages. If the package is not already installed on your system, p_load will do so before loading it into your R session

p_load(Lahman, pitchRx, devtools, sqldf, dplyr, reshape2, ggplot2, RMySQL, XML, shiny)

# Lahman examples
# View the first 6 rows of the Master table

head(Master)

# pitchRx example

library(pitchRx)
library(dplyr)
dat <- scrape("2015-05-21", "2015-05-21")
locations <- select(dat$pitch, pitch_type, start_speed, px, pz, des, num, gameday_link)
names <- select(dat$atbat, pitcher, batter, pitcher_name, batter_name, num, gameday_link, event, stand)
data <- names %>% filter(pitcher_name == "Jacob DeGrom") %>% inner_join(locations, ., by = c("num", "gameday_link"))

# restrosheet example
# you need to have devtools loaded and install restrosheet via github for more rescent versions of R

install_github("rmscriven/retrosheet")

getTeamIDs(1969)

retro <- getRetrosheet("roster", 1969)
retro$NYN

retro_sch <- getRetrosheet("schedule", 1969)
NYMa <- filter(retro_sch, VisTeam == "NYN")
NYMh <- filter(retro_sch, HmTeam == "NYN")
NYM1969 <- rbind(NYMa, NYMh) %>% arrange(Date)

# openWAR example

# there are few packages you will have to install outside of the CRAN network to get it to work

install.packages("Sxslt", repos = "http://www.omegahat.org/R", type = "source")
install_github("beanumber/openWAR")
require(openWAR)

# scrape data for all games through August 2015 (WARNING: this can take around an hour, so if you want you can just load in the data I've included in the repository)

all2015 <- getData(start = '2015-04-05', end = '2015-08-31')
head(all2015)
war <- makeWAR(all2015)

# export the downloaded data to your working directory

write.csv(all2015, "all2015.csv", row.names = FALSE)

# RMySQL example
# create connection object
con<-dbConnect(RMySQL::MySQL(), dbname="Lahman Database", username = "root", port = 3306)

thirty_thirty <- dbGetQuery(con, "SELECT CONCAT(m.nameLast, ', ', m.nameFirst) as 'Player', yearID as 'Season', teamID as 'Team', HR, SB FROM batting b JOIN Master m ON b.playerID=m.playerID WHERE HR >= 30 AND SB >= 30 ORDER BY yearID DESC")

# make sure to close your connection and detach the package from your environment before using another SQL-like package, like sqldf below

dbDisconnect(con)

detach("package:RMySQL")

# sqldf
require(sqldf)

thirty_thirty_sqldf <- sqldf("SELECT m.nameLast||', '||m.nameFirst as 'Player', yearID as 'Season', teamID as 'Team', HR, SB FROM Batting b JOIN Master m ON b.playerID=m.playerID WHERE HR >= 30 AND SB >= 30 ORDER BY yearID DESC")

# dplyr

require(dplyr)

thirty_thirty_dplyr <- filter(Lahman::Batting, HR >= 30, SB >= 30) %>% left_join(Lahman::Master, by = "playerID") %>% arrange(desc(yearID)) %>% mutate(Player = paste(nameLast, nameFirst, sep = ", ")) %>% select(Player, yearID, teamID, HR, SB)

count <- thirty_thirty_dplyr %>% group_by(Player) %>% summarise(Count = n()) %>% arrange(desc(Count))

count_season <- thirty_thirty_dplyr %>% group_by(yearID) %>% summarise(Count = n()) %>% arrange(desc(Count))

count_team <- thirty_thirty_dplyr %>% group_by(teamID) %>% summarise(Count = n()) %>% arrange(desc(Count))

hr_y2y <- filter(Lahman::Batting, yearID >= 2000, yearID < 2011) %>% left_join(Lahman::Master, by = "playerID") %>% arrange(desc(yearID)) %>% mutate(Player = paste(nameLast, nameFirst, sep = ", ")) %>% select(Player, yearID, teamID, HR, AB, SO, SF) %>% mutate(HR_rate = round((HR/(AB+SF-SO)*600),1)) %>% filter(AB >= 400) %>% mutate(Season_next = yearID + 1) %>% left_join(., ., by = c("Season_next" = "yearID", "Player" = "Player")) %>% filter(!is.na(HR.y)) %>% mutate(HR_rate_change = (HR_rate.y - HR_rate.x)) %>% arrange(desc(HR_rate_change)) %>% select(Player, yearID, teamID.x, HR.x, HR_rate.x, Season_next, teamID.y, HR.y, HR_rate.y, HR_rate_change)

# XML

require(pacman)
p_load(dplyr, XML, reshape2)

## scrape the Standings by Date page to see which elements correspond to which divisions

dat <- readHTMLTable("http://www.baseball-reference.com/games/standings.cgi?year=2015&month=08&day=31&submit=Submit+Date")

## here are the divisions and corresponding elements in the list

# 2 AL EAST
# 3 AL CENTRAL
# 4 AL WEST
# 5 NL EAST
# 6 NL CENTRAL
# 7 NL WEST

dat[5]

# create a function for scraping the data given a specific date
date_scrape <- function(y,m,d) {
  url <- paste0("http://www.baseball-reference.com/games/standings.cgi?year=",y,"&month=",m, "&day=",d,"&submit=Submit+Date")
  d <- readHTMLTable(url, stringsAsFactors = FALSE)
  d <- as.data.frame(d[5])
  d
}

# create a complete sequence of dates you want to scrape data for
dates <- as.data.frame(seq(as.Date("2015/04/05"), as.Date("2015/08/31"), by = "days"))
names(dates) <- "dates" 

# split the dates so that there are three separate inputs to feed the function
dates <- colsplit(dates$dates, "-", c("y", "m", "d"))

# use the do() function to iterate the scrape function over all the dates

out <- dates %>% group_by(y,m,d) %>% do(date_scrape(.$y, .$m, .$d))

# view the first 10 rows

head(out, 10)

# Visualizing

require(ggplot2, RColorBrewer)

# pair down the data set and create a single column with the date of the standings
nle_standings_2015 <- ungroup(out) %>% mutate(Date = paste0(y, sep = "-", m, sep = "-", d)) %>% select(Date, Tm, GB) 

# change the data type for the three columns
nle_standings_2015$GB <- as.numeric(nle_standings_2015$GB) 
nle_standings_2015$Date <- as.Date(nle_standings_2015$Date)
nle_standings_2015$Tm <- as.factor(nle_standings_2015$Tm)

# make sure when a team is in first it has a 0 for the games back value
nle_standings_2015$GB <- ifelse(is.na(nle_standings_2015$GB), 0, nle_standings_2015$GB)

# set the color scheme for the teams
team_colors = c("ATL" = "#01487E", "MIA" = "#0482CC", "NYM" = "#F7742C", "PHI" = "#CA1F2C", "WSN" = "#575959")

# plot the data using ggplot2
plot <- ggplot(nle_standings_2015, aes(Date, GB, colour = factor(Tm), group = Tm)) + geom_line(size = 1.25, alpha = .75) + scale_colour_manual(values = team_colors, name = "Team") + scale_y_reverse(breaks = 0:25) + scale_x_date() + labs(title = "NLE East Race through August 2015") + geom_text(aes(label=ifelse(Date == "2015-08-31", as.character(GB),'')),hjust=-.5,just=0, size = 4, show_guide = FALSE) + theme(legend.title = element_text(size = 12)) + theme(legend.text = element_text(size = 12)) + theme(axis.text = element_text(size = 13, face = "bold"), axis.title = element_text(size = 18, color = "grey50", face = "bold"), plot.title = element_text(size = 35, face = "bold", vjust = 1))

# view the graphic
plot

# save the plot as a png

png(filename="NLE_Race_greyTheme.png", 
    units="in", 
    width=12, 
    height=8, 
    res=72)
plot
dev.off()

# plot PITCHf/x data

# subset the data, keeping all rows but only columns number 1 through 5 and 13

deGrom <- data[,c(1:5, 13)]

# filter for swinging strikes

deGrom_swing <- filter(deGrom, grepl("Swinging", des)) 

# plot the pitches, coloring them by velocity

p <- ggplot(deGrom_swing, aes(px, pz, color = start_speed))

# add in customized axis and legend formatting and labels

p <- p + scale_x_continuous(limits = c(-3,3)) + scale_y_continuous(limits = c(0,5)) + annotate("rect", xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5, color = "black", alpha = 0) + labs(title = "Jacob deGrom: Swinging Strikes, 5/21/2015") + ylab("Horizontal Location (ft.)") + xlab("Vertical Location (ft): Catcher's View") + labs(color = "Velocity (mph)")

# format the points

p <- p + geom_point(size = 10, alpha = .65)

# finish formatting

p <- p + theme(axis.title = element_text(size = 15, color = "black", face = "bold")) + theme(plot.title = element_text(size = 30, face = "bold", vjust = 1)) + theme(axis.text = element_text(size = 13, face = "bold", color = "black")) + theme(legend.title = element_text(size = 12)) + theme(legend.text = element_text(size = 12))

# view the plot

p
   
# save the plot as a png

png(filename="deGrom.png", 
    units="in", 
    width=12, 
    height=8, 
    res=72)
p
dev.off()                                                                      
p2 <- ggplot(deGrom_swing, aes(px, pz, color = pitch_type))
p2 <- p2 + scale_x_continuous(limits = c(-3,3)) + scale_y_continuous(limits = c(0,5)) + annotate("rect", xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5, color = "black", alpha = 0) + labs(title = "Jacob deGrom: Swinging Strikes, 5/21/2015") + ylab("Horizontal Location (ft.)") + xlab("Vertical Location (ft): Catcher's View") + labs(color = "Pitch Type")
p2 <- p2 + geom_point(size = 10, alpha = .65)
p2 <- p2 + theme(axis.title = element_text(size = 15, color = "black", face = "bold")) + theme(plot.title = element_text(size = 30, face = "bold", vjust = 1)) + theme(axis.text = element_text(size = 13, face = "bold", color = "black")) + theme(legend.title = element_text(size = 12)) + theme(legend.text = element_text(size = 12))
p2

# save the plot as a png

png(filename="deGrom2.png", 
    units="in", 
    width=12, 
    height=8, 
    res=72)
p2
dev.off()    

