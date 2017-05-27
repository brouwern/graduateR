#Function to plot the coefficients from linear regresion with a single predictor

m.coefs <- function(mod,plot.p = T){
  coeftable <- round(summary(mod)$coefficients,2)
  
  
  space40 <- paste(rep(" ",40),collapse = "")
  estimate.SE <- paste(space40,"Estimate (SE)")
  estimate.int <- paste(space40,coeftable[1,1]," (",coeftable[1,2],")",sep = "")
  estimate.slo <- paste(space40,coeftable[2,1]," (",coeftable[2,2],")",sep = "")
  
  
  mtext(text = "Coefficients:", side = 3,
        #at = 4,
        adj= 0,
        line = 3)
  mtext(text = estimate.SE,
        side = 3,adj= 0, line = 2)
  
  mtext(text = "(Intercept)", side = 3,adj= 0,line = 1)
  mtext(text = rownames(coeftable)[2], side = 3,adj= 0,line = 0)
  mtext(text = estimate.int, side = 3,adj= 0,line = 1)
  mtext(text = estimate.slo, side = 3,adj= 0,line = 0)
  
  if(plot.p == T){
    mtext(text = "Pr(>|t|)", side = 3,line = 2)
    mtext(text = coeftable[1,4], side = 3,line = 1)
    mtext(text = coeftable[2,4], side = 3,line = 0)
    
  }
  
  
  
}
