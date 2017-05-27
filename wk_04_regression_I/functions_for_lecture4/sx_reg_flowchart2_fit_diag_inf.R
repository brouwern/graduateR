#This script using the diagram package to make a flowchart


# Original workding director: setwd("C:/Users/lisanjie2/Desktop/TEACHING/1_STATS_CalU/1_STAT_CalU_2016_by_NLB/1_STAT_Duq_2017/1_Lectures_presentations/Class_4_Feb3_2017_regression/Lecture_4_RMD/scripts_Lecture4")

library(diagram)



#Function to make individul elements
# Make boxes/cirecls and Label 1st plot elements
## ellipse
### radx = "horizondal radius of the box"
#   rady = vert
makebox <- function(i.elpos = 1, 
                    lab. = "label",
                    radx. = 0.1, 
                    cex. = 1.5,
                    pos. = NULL,
                    lcol. = 1){
  par(mfrow = c(1,1))
  textrect(elpos[i.elpos,], 
           radx = radx., 
           lab = lab., 
           shadow.size = 0.00, 
           cex = cex.,
           pos = pos.,
           lcol = lcol.)
}




# Set up plot
##Set margins
par(mar = c(1, 1, 1, 1))

##create empty plot window
openplotmat()

#coordinates estimtes location of plot elemens
## pos = is a vector of the number of elements PER ROW
elpos <- coordinates(pos = c(1, 3,3))

#..
fromto <- matrix(ncol = 2, byrow = TRUE,
                 data = c(1, 2,
                          1, 3,
                          1, 4,
                          2, 5,
                          3, 6,
                          4, 7)) 

#number of rows
nr <- nrow(fromto)

#matrix to hold arrow positions
arrpos <- matrix(ncol = 2, nrow = nr)


#loop to calculate arrow positions and plot them
for (i in 1:nr){
  arrpos[i, ] <- straightarrow(to   = elpos[fromto[i, 2], ],
                               from = elpos[fromto[i, 1], ],
                               lwd = 3, 
                               #arr.pos: 
                               ### where along line arrow
                               arr.pos = 0.615, 
                               arr.type = "triangle",
                               #endhead = T,
                               arr.length = 0.75) 
}





#Top
makebox(i.elpos = 1, lab. = "Regression\nModelling")

#Middle row
makebox(i.elpos = 2, lab. = "Model\nfitting")
makebox(i.elpos = 3, lab. = "Model\nDiagnostics")
makebox(i.elpos = 4, lab. = "Inference")

#Bottom row
makebox(i.elpos = 5, lab. = "*Least squares (LS)\n*Maximum likelihood\n*Bayesian", lcol. = 0, pos. = NULL)
makebox(i.elpos = 6, lab. = "*Normality\n*Constant variance\n*Independence\n*etc...", lcol. = 0, pos. = NULL)

makebox(i.elpos = 7, lab. = "*Frequentist (t, F, p, CIs)\n*IT-AIC\n*Bayesian", lcol. = 0, pos. = NULL)