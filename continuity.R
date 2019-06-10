# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Continuity: Elliptical hearts
# Author: Dr Ord (@ordiology)
# Date: 27 April 2019
# License: GPL-3
#
# To demonstrate the gestalt principle of continuity I plot two 
# ellipses of dots at 90 degrees to each other that I describe using 
# elliptical equations in an x-y plane. I colour some of the dots 
# in each ellipse blue and some orange so that the arrangement 
# creates the illusion that there are two hearts, one the right way 
# round and one upside-down. 
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Setup ----

# The limits (range) of the axes in the x-y plane
lims <- c(-2.75, 2.75) 

# Background colour
bg = "#000000" # black

# 2. Plot function ----
createPlot <- function(col_cut = 8){

  # Set up the graphical parameters...
  # bg: background colour
  # mar: all margins (bottom, left, top right) are set to 0... i.e. no margins
  # bty: the type of box to draw around the boundaries of the graph... set to "n" -- no box
  par(bg = bg, mar=c(0,0,0,0), bty="n")
  
  
  # 2.1. Create the plot area ----
  # x: 0 -- no data
  # type: "n" -- no plot
  # xlim: x-axis limits (range)
  # ylim: y-axis limits (range)
  # axes: F -- no axes
  # asp: x-y aspect ratio = 1:1
  plot(0, type = "n", xlim = lims, ylim = lims, axes = F, asp=1)
  
  
  # 2.2. Parameters for the elliptical equation ----
  # Semi-minor axis length
  a = 1 
  # Semi-major axis length
  b = 2
  # This is the shift in initial angle position from zero required to ensure that the
  # top and bottom points of the heart line up
  shift = 20*pi/128
  # Angle steps between each point
  step = pi/16
  # Colours of the dots -- orange and blue
  cols <- c("#ff4019", "#1bd1d1")
  
  # 2.3. Loop over angles from shift to 2*pi + shift in steps of step ----
  for (t in seq(shift, 2*pi + shift, step)) {

    # 2.4. x and y position for the first ellipse current dot ----
    x1 = a*cos(t) - b*sin(t)
    y1 = b*sin(t) + a*cos(t)

    # 2.5. x and y position for the second ellipse current dot ----
    x2 = b*cos(t) - a*sin(t)
    y2 = a*sin(t) + b*cos(t)
    

    # 2.6. Dot colour indeces ---- 
    # Colour of current dot depends on x position -- once past the cut point, the colour changes
    # The second ellipse dot is the opposite colour from the first ellipse dot
    # i is the index of cols that will be selected for the first ellipse
    # j is the index of cols that will be selected for the second ellipse
    if (x1 <= col_cut) {
      i = 1
      j = 2
    } else {
      i = 2
      j = 1
    }

    # 2.7. Plot the first and second ellipse dots ----   
    points(x1, y1, col = cols[i], pch = 19)
    points(x2, y2, col = cols[j], pch = 19)

  }
  
}


# 3. Output a png of width 1800px by 1800px and resolution 360 pixels per inch ----
# Note that if the resolution is changed the dot size will need to change (add cex to the points function)
png(file=paste0("gestalt_continuity.png"), width = 1800, height = 1800, units="px", res = 360)
  createPlot()
dev.off()
