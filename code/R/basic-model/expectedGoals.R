con =  dbConnect(SQLite(), dbname="../../../datasets/european-football-db/database.sqlite")

# Read matches from 2015/2016, leaving out betting odds
matches = dbGetQuery(con, "SELECT * FROM Match WHERE country_id IN (1729,4769,7809,10257,21518)")[,1:85]


rmatch <- function(x, name) {
  pos <- match(name, names(x))
  if (!is.na(pos)) return(x[[pos]])
  for (el in x) {
    if (class(el) == "list") {
      out <- Recall(el, name)
      if (!is.null(out)) return(out)
    }
  }
}

# Make a data.frame of shots on target
# Coordinates will be pulled seperately from other variables because it has two values (x, y)
# This only gives blocked  and saved shots
pullcols = c('subtype','elapsed','player1','id','team')
shots = data.frame(matrix(NA,1,length(pullcols)+3))
names(shots)[1:length(pullcols)] = pullcols
names(shots)[(length(pullcols)+1):length(shots)] = c('X', 'Y','MatchID')
xDF.init = shots 

for (i in 1:nrow(matches)){
  # Convert XML data to a vector of data items
  if(!is.na(matches$shoton[i])){
    xD = xmlParse(matches$shoton[i])
    xL = xmlToList(xD)
    xV = unlist(xL)
    names(xV) = gsub('value.','',names(xV))
    # Initialize a data frame to write to
    xDF = xDF.init[rep(1:nrow(xDF.init),length(xL)),]
    # Find the index where each event in our vector begins
    item.bounds = c(0,which(names(xV) == 'id'))
    # For each event, fill out a row of the data frame
    for (j in 1:(length(item.bounds)-1)){
      event = xV[(1+item.bounds[j]):item.bounds[j+1]]
      for (k in 1:length(pullcols)){
        xDF[j,k] = max(0,event[which(names(event) == pullcols[k])])
      }
      xDF$X[j] = max(0,rmatch(xL[[j]], 'coordinates')[1]$value)
      xDF$Y[j] = max(0,rmatch(xL[[j]], 'coordinates')[2]$value)
    }
    xDF$MatchID = matches$id[i]
    shots = rbind(shots,xDF)
  }
}
shots = shots[-1,]



misses = data.frame(matrix(NA,1,length(pullcols)+3))
names(misses)[1:length(pullcols)] = pullcols
names(misses)[(length(pullcols)+1):length(misses)] = c('X', 'Y','MatchID')
xDF.init = misses

for (i in 1:nrow(matches)){
  # Convert XML data to a vector of data items
  if(!is.na(matches$shotoff[i])){
    xD = xmlParse(matches$shotoff[i])
    xL = xmlToList(xD)
    xV = unlist(xL)
    names(xV) = gsub('value.','',names(xV))
    # Initialize a data frame to write to
    xDF = xDF.init[rep(1:nrow(xDF.init),length(xL)),]
    # Find the index where each event in our vector begins
    item.bounds = c(0,which(names(xV) == 'id'))
    # For each event, fill out a row of the data frame
    for (j in 1:(length(item.bounds)-1)){
      event = xV[(1+item.bounds[j]):item.bounds[j+1]]
      for (k in 1:length(pullcols)){
        xDF[j,k] = max(0,event[which(names(event) == pullcols[k])])
      }
      xDF$X[j] = max(0,rmatch(xL[[j]], 'coordinates')[1]$value)
      xDF$Y[j] = max(0,rmatch(xL[[j]], 'coordinates')[2]$value)
    }
    xDF$MatchID = matches$id[i]
    misses = rbind(misses,xDF)
  }
}
misses = misses[-1,]

# Repeat the above process for goals
goals = data.frame(matrix(NA,1,length(pullcols)+3))
names(goals)[1:length(pullcols)] = pullcols
names(goals)[(length(pullcols)+1):length(goals)] = c('X', 'Y','MatchID')
xDF.init = goals
for (i in 1:nrow(matches)){
  # Ignore games with no goals scored
  if(!is.na(matches$goal[i])){
    if(nchar(matches$goal[i]) > 8){
      
      xD = xmlParse(matches$goal[i])
      xL = xmlToList(xD)
      xV = unlist(xL)
      names(xV) = gsub('value.','',names(xV))
      xDF = xDF.init[rep(1:nrow(xDF.init),length(xL)),]
      item.bounds = c(0,which(names(xV) == 'id'))
      for (j in 1:(length(item.bounds)-1)){
        event = xV[(1+item.bounds[j]):item.bounds[j+1]]
        for (k in 1:length(pullcols)){
          xDF[j,k] = max(0,event[which(names(event) == pullcols[k])])
        }
        xDF$X[j] = max(0,rmatch(xL[[j]], 'coordinates')[1]$value)
        xDF$Y[j] = max(0,rmatch(xL[[j]], 'coordinates')[2]$value)
      }
      xDF$MatchID = matches$id[i]
    }
    goals = rbind(goals,xDF)
  }
}
goals = goals[-1,]
goals$X = as.numeric(goals$X)
goals$Y = as.numeric(goals$Y)
shots$X = as.numeric(shots$X)
shots$Y = as.numeric(shots$Y)
misses$X = as.numeric(misses$X)
misses$Y = as.numeric(misses$Y)

# Combine goals, shots, and misses to a single data frame
events = rbind(goals,shots,misses)
events$result = c(rep('G', length(goals[,1])), rep('S', length(shots[,1])), rep('M', length(misses[,1])))

# Remove shots from the penalty spot
events = events[!(events$X == 23 & events$Y == 8),]
events = events[!(events$X == 23 & events$Y == 62),]

# Fold events so that each half overlaps
# Flip events in the folded half so that left overlaps with left
events$X[events$Y > 35] = 45-events$X[events$Y > 35]
events$Y[events$Y > 35] = 70-events$Y[events$Y > 35]

# Remove events with out-of-bounds locations
events = events[events$X > 0 & events$X < 45 & events$Y > 0 & events$Y < 36,]


# Determine the number of attempts and odds of scoring from each cell
chance = matrix(0,44,35)
attempts = matrix(0,44,35)
for (x in 1:44){
  for (y in 1:35){
    # Chance of scoring from this cell
    chance[x,y] = length(events[events$X == x & events$Y == y & events$result=='G',1]) / length(events[events$X == x & events$Y == y,1])
    # Number of attempts from this cell
    attempts[x,y] = length(events[events$X == x & events$Y == y,1])
  }
}

# Catch division by zero errors
chance[is.nan(chance)] = 0
attempts[is.nan(attempts)] = 0

# Attach chance information to events data frame
events$chance = numeric(length(events$subtype))
for(i in 1:length(events$X)){
  events$chance[i] = chance[events$X[i],events$Y[i]]
}
pitch.l = 105
pitch.w = 68

add_rectangle <- function(orig, miny, maxy, width) { 
  mid <- pitch.w / 2 
  halfwidth <- width / 2 
  #rbind(orig,
       # c(x1 = mid - halfwidth, y1 = miny, x2 = mid + halfwidth, y2 = miny),
        #c(x1 = mid - halfwidth, y1 = miny, x2 = mid - halfwidth, y2 = maxy),
        #c(x1 = mid - halfwidth, y1 = maxy, x2 = mid + halfwidth, y2 = maxy),
        #c(x1 = mid + halfwidth, y1 = maxy, x2 = mid + halfwidth, y2 = miny)
  rbind(orig,
    c(x1 = mid - halfwidth, y1 = miny, x2 = mid + halfwidth, y2 = miny),
    c(x1 = mid - halfwidth, y1 = miny, x2 = mid - halfwidth, y2 = maxy),
    c(x1 = mid - halfwidth, y1 = maxy, x2 = mid + halfwidth, y2 = maxy),
    c(x1 = mid + halfwidth, y1 = maxy, x2 = mid + halfwidth, y2 = miny)
  )
}
library(tibble)
library(dplyr)
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
        panel.grid.minor = element_blank())


# Remove areas with less than 5 attempts
chance[attempts < 5] = 0

# Plot the chance of scoring from each grid cell
p.chance = cbind(melt(data.frame(chance)),attempts=melt(data.frame(attempts))$value)
p.chance$X = rep(1:44,35)
p.chance$Y = rep(1:35,each=44)
p1 = ggplot(p.chance) +
  geom_raster(aes(x=X, y=Y, fill=value, fill=attempts)) +
  theme_minimal() +
  scale_fill_gradient(low = "blue", high = "red") + labs(fill = "xG", title = "Shots with my xG") +
  scale_alpha_continuous(guide=FALSE) +
  +
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
  theme(panel.background = element_rect(fill = "grey",
                                        size = 1, linetype = "solid" ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position = "bottom")  +scale_x_reverse()
p1


a1 = p + geom_raster(data = shots_valid, aes(x = location.x, y = location.y, fill = my.xG)) +
  scale_fill_gradient(low = "blue", high = "red") + labs(fill = "xG", title = "Shots with my xG") + theme_void() + theme(plot.title = element_text(size=13,lineheight=.8, vjust=1,family="serif"))
