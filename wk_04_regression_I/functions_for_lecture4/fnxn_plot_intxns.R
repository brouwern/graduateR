

#makes interaction plot of simple regression
#models w/1 predictor

plot.interactions <- function(model,
                              x.cont,
                              x.cat,
                              y,
                              cols = c(1,2)){
  
  dat <- model$model
  rng <- range(dat[,x.cont])
  levs <- levels(dat[,x.cat])
   
  newdat <- expand.grid(x.cont = rng,
                        x.cat = levs)
  names(newdat) <- c(x.cont, x.cat)
  
  newdat$yhat <- predict(model,newdat)
  

  i.lev1 <- which(dat[,x.cat] == levs[1])
  
  xlims <- range(dat[,x.cont])
  ylims <- range(dat[,y])
  
  plot(dat[i.lev1,y] ~ dat[i.lev1,x.cont], 
       xlab = x.cont,
       ylab = y,
       xlim = xlims,
       ylim = ylims,col=cols[1])
  points(dat[-i.lev1,y] ~ dat[-i.lev1,x.cont], 
       xlab = x.cont,
       ylab = y,
       col = cols[2])
  

  
  #subset regression lines
  i.lev1 <- newdat[,x.cat] == levs[1]
  i.lev2 <- newdat[,x.cat] == levs[2]
  
  points(newdat$yhat ~ newdat[,x.cont], 
         subset = i.lev1,
         col = cols[1],
         type = "l")
  
  points(newdat$yhat ~ newdat[,x.cont], 
         subset = i.lev2,
         col = cols[2],
         type = "l")
  
  
}




