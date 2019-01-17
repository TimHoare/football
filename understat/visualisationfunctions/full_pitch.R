library(tidyverse)

full_pitch <- function(theme = "light") {

if(theme == "light"){
  grass_colour <- "white"
  line_colour <- "darkgrey"
  background_colour <- "white"
  goal_colour <- "black"
}
else if(theme == "dark") {
  grass_colour <- "gray25"
  line_colour <- "white"
  background_colour <- "gray25"
  goal_colour <- "black"
}
else if(theme == "grass"){
  grass_colour <- "green4"
  line_colour <- "white"
  background_colour <- "green4"
  goal_colour <- "black"
}


#Edges
#Width dims

ymin <- 0
ymax <- 68

#Length dims 

xmin <- 0
xmax <- 105

#Width

penaltyBoxLeft <- 54.06
penaltyBoxRight <- 13.94
sixYardLeft <- 43.86
sixYardRight <- 24.14
goalPostLeft <- 38.08
goalPostRight <- 29.92
centreSpot <- 34

#Length

sixYardDef <- 5.985
sixYardOff <- 99.015
penaltyBoxDef <- 18.06
penaltyBoxOff <- 86.94
penaltySpotDef <- 12.075
penaltySpotOff <- 92.925
halfwayLine <- 52.5

#Other

centreCircleD <- 17

theme_blank <- function() {
  theme(
    axis.text = element_blank(),
    axis.ticks.length = unit(0, "lines"),
    axis.title = element_blank(),
    legend.key = element_rect(fill = background_colour,
                              colour = background_colour),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12,
                                face = "bold"),
    legend.background = element_rect(fill = background_colour),
    panel.background = element_rect(fill = background_colour),
    panel.grid = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "lines"),
    panel.spacing = element_blank()
  )
}

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

centreCircle <- circleFun(c(halfwayLine, centreSpot), centreCircleD, npoints = 1000)

dDef <- circleFun(c(penaltySpotDef, centreSpot), centreCircleD, npoints = 1000)

dDef <- dDef[which(dDef$x >= penaltyBoxDef),]


dOff <- circleFun(c(penaltySpotOff, centreSpot), centreCircleD, npoints = 1000)
## remove part that is in the box
dOff <- dOff[which(dOff$x <= (xmax - penaltyBoxDef)),]


p <- ggplot() +
  xlim(c(xmin - 5, xmax + 5)) +
  ylim(c(ymin - 5, ymax + 5)) +
  theme_blank() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = grass_colour, colour = line_colour) +
  geom_rect(aes(xmin = xmin, xmax = penaltyBoxDef, ymin = penaltyBoxRight, ymax = penaltyBoxLeft), fill = grass_colour, colour = line_colour) +
  geom_rect(aes(xmin = penaltyBoxOff, xmax = xmax, ymin = penaltyBoxRight, ymax = penaltyBoxLeft), fill = grass_colour, colour = line_colour) +
  geom_rect(aes(xmin = xmin, xmax = sixYardDef, ymin = sixYardRight, ymax = sixYardLeft), fill = grass_colour, colour = line_colour) +
  geom_rect(aes(xmin = sixYardOff, xmax = xmax, ymin = sixYardRight, ymax = sixYardLeft), fill = grass_colour, colour = line_colour) +
  geom_segment(aes(x = xmin, y = goalPostRight, xend = xmin, yend = goalPostLeft), colour = goal_colour, size = 1) +
  geom_segment(aes(x = xmax, y = goalPostRight, xend = xmax, yend = goalPostLeft), colour = goal_colour, size = 1) +
  geom_segment(aes(x = halfwayLine, y = ymin, xend = halfwayLine, yend = ymax), colour = line_colour) +
  geom_path(data = centreCircle, aes(x = x, y = y), colour = line_colour) +
  geom_point(aes(x = penaltySpotDef, y = centreSpot), colour = line_colour) +
  geom_point(aes(x = penaltySpotOff, y = centreSpot), colour = line_colour) +
  geom_point(aes(x = halfwayLine, y = centreSpot), colour = line_colour) +
  geom_path(data = dDef, aes(x = x, y = y), colour = line_colour) +
  geom_path(data = dOff, aes(x = x, y = y), colour = line_colour)
  
return(p)
}
