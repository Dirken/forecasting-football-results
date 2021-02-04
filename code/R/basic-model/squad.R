#Animation: squad position
#author: "Donyoe"

# Here we can see the most common squad positions.

library(RSQLite)
library(dplyr)
library(XML)
library(xml2)
library("methods")
library(stringdist)
library(plyr)
library(DT)
library(gridExtra)
library(animation)

con <- dbConnect(SQLite(), dbname="../../../datasets/european-football-db/database.sqlite")

country <- tbl_df(dbGetQuery( con,"SELECT * FROM COUNTRY"))
league <- tbl_df(dbGetQuery( con,"SELECT * FROM LEAGUE"))
match <- tbl_df(dbGetQuery( con,"SELECT * FROM MATCH"))
dbDisconnect(con)

ind1 <- grep("player_X",x = colnames(match))
ind2 <- grep("player_Y",x = colnames(match))
ind3 <- grep("country_id",x = colnames(match))
ind4 <- grep("season",x = colnames(match))
ind5 <- grep("league_id",x = colnames(match))

ind <- c(ind1,ind2,ind3,ind4,ind5)
position <- match[,ind]
position <- as.data.frame(position)
position$home_player_X1[position$home_player_X1==1] <- 5L
position$away_player_X1[position$away_player_X1==1] <- 5L
###############
position <- position[-unique(which(is.na(position),arr.ind = T)[,1]),]
print(position)
##############

equipo <- function(position,year) {
  
  indhpx <- grep("home_player_X",x = colnames(position))
  indapx <- grep("away_player_X",x = colnames(position))
  indhpy <- grep("home_player_Y",x = colnames(position))
  indapy <- grep("away_player_Y",x = colnames(position))
  
  c1 <- NULL
  loc <- list()
  for (index in 1:nrow(position)) {
    for (i in 1:11) { 
      c1 <- c(c1,paste(position[index,indhpx[i]],position[index,indhpy[i]],sep = "-",collapse = "-"))
    } 
    loc[[index]] <- c1
    c1 <- NULL
  }
  
  j <- NULL
  j[1] <- names(which.max(table(as.character(lapply(loc,"[[",1)))))
  j[2] <- names(which.max(table(as.character(lapply(loc,"[[",2)))))
  j[3] <- names(which.max(table(as.character(lapply(loc,"[[",3)))))
  j[4] <- names(which.max(table(as.character(lapply(loc,"[[",4)))))
  j[5] <- names(which.max(table(as.character(lapply(loc,"[[",5)))))
  j[6] <- names(which.max(table(as.character(lapply(loc,"[[",6)))))
  j[7] <- names(which.max(table(as.character(lapply(loc,"[[",7)))))
  j[8] <- names(which.max(table(as.character(lapply(loc,"[[",8)))))
  j[9] <- names(which.max(table(as.character(lapply(loc,"[[",9)))))
  j[10] <- names(which.max(table(as.character(lapply(loc,"[[",10)))))
  j[11] <- names(which.max(table(as.character(lapply(loc,"[[",11)))))
  
  
  jp <- (strsplit(j,split = "-"))
  xpos_home <- as.numeric(lapply(jp,"[[",1))
  ypos_home <- as.numeric(lapply(jp,"[[",2))
  #######
  c1 <- NULL
  loc <- list()
  for (index in 1:nrow(position)) {
    for (i in 1:11) { 
      c1 <- c(c1,paste(position[index,indapx[i]],position[index,indapy[i]],sep = "-",collapse = "-"))
    } 
    loc[[index]] <- c1
    c1 <- NULL
  }
  
  j <- NULL
  j[1] <- names(which.max(table(as.character(lapply(loc,"[[",1)))))
  j[2] <- names(which.max(table(as.character(lapply(loc,"[[",2)))))
  j[3] <- names(which.max(table(as.character(lapply(loc,"[[",3)))))
  j[4] <- names(which.max(table(as.character(lapply(loc,"[[",4)))))
  j[5] <- names(which.max(table(as.character(lapply(loc,"[[",5)))))
  j[6] <- names(which.max(table(as.character(lapply(loc,"[[",6)))))
  j[7] <- names(which.max(table(as.character(lapply(loc,"[[",7)))))
  j[8] <- names(which.max(table(as.character(lapply(loc,"[[",8)))))
  j[9] <- names(which.max(table(as.character(lapply(loc,"[[",9)))))
  j[10] <- names(which.max(table(as.character(lapply(loc,"[[",10)))))
  j[11] <- names(which.max(table(as.character(lapply(loc,"[[",11)))))
  
  
  jp <- (strsplit(j,split = "-"))
  xpos_away <- as.numeric(lapply(jp,"[[",1))
  ypos_away <- as.numeric(lapply(jp,"[[",2))
  positionAway <- NULL
  positionAway$lon <- as.data.frame(xpos_away)
  positionAway$lat <- as.data.frame(ypos_away)
  positionAway <- as.data.frame(positionAway)
  print(positionAway)
  library(ggplot2)
  plot1 <- (qplot(xpos_home,ypos_home, size=I(4))+ggtitle(paste("Home",year))+
              theme(panel.background = element_rect(fill = "#05890b",
                                                    size = 1, linetype = "solid" ),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank()))
  
  plot2 <- (qplot(xpos_away,ypos_away, size=I(4))+ggtitle(paste("Away",year))+
              theme(panel.background = element_rect(fill = "#05890b",
                                                    size = 1, linetype = "solid" ),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank()))
  
  grid.arrange(plot1, plot2, ncol=2)
}
seas <- unique(position$season)
leag <- unique(position$league_id)
leadnom <- league$name

for (i in 1:8) {
  position1 <- position[position$season==seas[i],]
  equipo(position = position1,year = paste("Year: ",seas[i]))
}

for (i in 1:11) {
  position1 <- position[position$league_id==leag[i],]
  equipo(position = position1,year = paste("League:",leadnom[i]))
}


pitch.l = 105
pitch.w = 68

add_rectangle <- function(orig, miny, maxy, width) { 
  mid <- pitch.w / 2 
  halfwidth <- width / 2 
  rbind(orig,
        c(x1 = mid - halfwidth, y1 = miny, x2 = mid + halfwidth, y2 = miny),
        c(x1 = mid - halfwidth, y1 = miny, x2 = mid - halfwidth, y2 = maxy),
        c(x1 = mid - halfwidth, y1 = maxy, x2 = mid + halfwidth, y2 = maxy),
        c(x1 = mid + halfwidth, y1 = maxy, x2 = mid + halfwidth, y2 = miny)
  )
}

field <- tibble(x1 = 0, y1 = pitch.l / 2, x2 = pitch.w, y2 = pitch.l / 2) %>%
  add_rectangle(0, pitch.l, pitch.w) %>%
  add_rectangle(0, -2.44, 7.32) %>%
  add_rectangle(pitch.l, pitch.l + 2.44, 7.32) %>%
  add_rectangle(0, 5.5, 18.32) %>%
  add_rectangle(pitch.l, pitch.l - 5.5, 18.32) %>%
  add_rectangle(0, 16.5, 40.32) %>%
  add_rectangle(pitch.l, pitch.l - 16.5, 40.32) * (2/3)


penalty_curve_bottom  <- tibble(x1 = (pitch.w / 2) - 7.312, y1 = 16.5,
                                x2 = (pitch.w / 2) + 7.312, y2 = 16.5) * (2/3)
penalty_curve_top  <- tibble(x1 = (pitch.w / 2) - 7.312, y1 = pitch.l - 16.5,
                             x2 = (pitch.w / 2) + 7.312, y2 = pitch.l - 16.5) * (2/3)
corner_bottom <- tibble(x1 = c(0, pitch.w - 1), y1 = c(1, 0),
                        x2 = c(1, pitch.w)    , y2 = c(0, 1)) * (2/3)
corner_top <- tibble(x1 = c(0, pitch.w - 1), y1 = c(pitch.l -1, pitch.l    ),
                     x2 = c(1, pitch.w    ), y2 = c(pitch.l   , pitch.l - 1)) * (2/3)
circle  <- tibble(x1 = (pitch.w / 2) - 9.15, y1 = pitch.l / 2,
                  x2 = (pitch.w / 2) + 9.15, y2 = pitch.l / 2) * (2/3)


pitch.l = pitch.l * (2/3)
pitch.w = pitch.w * (2/3)
pitch.aes  <- aes(x = x1, y = y1, xend = x2, yend = y2)
stripes.aes <- aes(x = 10, xend=20)

pitch  <- ggplot(mapping = aes(x = lon, y = lat)) +
  field                   %>% geom_segment(mapping = pitch.aes, colour = 'white', size = 1.25) +
  circle                  %>% geom_curve(  mapping = pitch.aes, colour = 'white', curvature = 1, ncp = 20, size = 1.25) +
  circle                  %>% geom_curve(  mapping = pitch.aes, colour = 'white', curvature = -1, ncp = 20, size = 1.25) +
  penalty_curve_bottom    %>% geom_curve(  mapping = pitch.aes, colour = 'white', curvature = +0.4, size = 1.25) +
  penalty_curve_top       %>% geom_curve(  mapping = pitch.aes, colour = 'white', curvature = -0.4, size = 1.25) +
  corner_bottom           %>% geom_curve(  mapping = pitch.aes, colour = 'white', curvature = -0.5, size = 1.25) +
  corner_top              %>% geom_curve(  mapping = pitch.aes, colour = 'white', curvature = 0.5, size = 1.25) +
  coord_fixed((pitch.l/pitch.w)/1.546) +
  scale_x_continuous(breaks = 1:10 / 10 * pitch.w, labels = NULL) +
  scale_y_continuous(breaks = 1:22 / 22 * pitch.l, labels = NULL) +
  theme(panel.background = element_rect(fill = "#05890b",
                                        size = 1, linetype = "solid" ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + coord_flip()
