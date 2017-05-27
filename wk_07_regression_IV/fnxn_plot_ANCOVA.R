

#makes interaction plot of simple regression
#models w/1 numeric and 1 categorical predictor



#makes interaction plot of simple regression
#models w/1 numeric and 1 categorical predictor


plot.ANCOVA <- function(model,
                        raw.data = NULL,
                        x.axis = NULL,
                        cols = c(1,2)){
  
  #Data
  ##extract data used in model
  dat <- model$model
  
  ##if x.axis arguement contains data, add to dataframe
  if(is.character(x.axis) == TRUE){
    dim.orig <- dim(dat)[2]
    dat <- cbind(dat,raw.data[,x.axis])
    names(dat)[-c(1:dim.orig)] <-  x.axis
  }
  
  
  
  #extract formula from model object
  ##get formula
  form <- as.character(formula(model))[-1]
  
  ##split into vector
  model.terms <- strsplit(form[2],"[\\+\\*]")
  
  ##name list returned by strsplit()
  names(model.terms) <- "x.names"
  
  ##get rid of spaces
  model.terms$x.names <- gsub(" ","" ,model.terms$x.names)
  
  ##count number of parameters
  i.terms <- length(model.terms$x.names)
  
  #create new entry in list (this will be overwritten)
  model.terms$x.types <- model.terms$x.names
  
  #Loop over each term in the model to determine
  ##whether it is categorical or continous
  for(i in 1:i.terms){
    term.i <- model.terms$x.names[i]
    
    if(any(strsplit(term.i,"")[[1]] == "*") == TRUE){
      model.terms$x.types[i] <- "interactions"
      next
    }
    
    
    model.terms$x.types[i] <- ifelse(is.numeric(dat[,term.i]) == TRUE, 
                                     "numeric",
                                     "categorical")
  }
  
  
  
  ## y axis
  model.terms$y <- form[1]
  
  
  
 
  
  #Define continous variable
  ## If the model doesn't contain a continuous, 
  ## get it from x.axis arguement
  if(is.null(x.axis)== FALSE){
    x.cont <- x.axis
  }
  
  ##If model contains a continous variable...
  if(is.null(x.axis)== TRUE){
    i.num <- which(model.terms$x.types == "numeric")
    x.cont <- model.terms$x.names[i.num]
    
  }

  
  #Define categorical variable
  i.cat <- which(model.terms$x.types == "categorical")
  x.cat <- model.terms$x.names[i.cat]
  
  if(length(x.cat) > 1){
    x.cat <- x.cat[1]
    
  }
  
  
  #Define y variable
  y <- model.terms$y
  
  

  
  # range of observed numeric data
  ## if calling from dataframe
  if(is.character(x.cont) == TRUE){
    rng <- range(dat[,x.cont])
    
  }
  
  ## if calling from x.var arguement
  if(is.numeric(x.cont) == TRUE){
    rng <- range(x.cont)
    
  }
  
  ## levels of the categorical variable
  levs <- levels(dat[,x.cat])
  
  
  ## generate new data with expand.grid() 
  newdat <- expand.grid(x.cont = rng,
                        x.cat = levs)
  

  ## label columns

  #if calling from x.axis
  if(is.numeric(x.cont) == TRUE){
    names(newdat) <- c("x.axis", x.cat)
    
  }
 
  #if calling from model datat
  if(is.character(x.cont) == TRUE){
    names(newdat) <- c(x.cont, x.cat)
    
  }
  
  # generate predictions from fitted model
  newdat$yhat <- predict(model,newdat)
  
  #determine baseline level of fitted model
  i.lev1 <- which(dat[,x.cat] == levs[1])
  
  #determine range of values
  ## range of observed x values
  #if calling from x.axis
  if(is.numeric(x.cont) == TRUE){
    xlims <- range(x.cont)
    
    
  }
  
  #if calling from model datat
  if(is.character(x.cont) == TRUE){
    xlims <- range(dat[,x.cont])
    
    
  }
  
 
  
  
  
  ## range of observed y values
  ylims <- range(dat[,y])
  
  
  #Plot raw data
  ## Plot level 1 of model (intercept)
  plot(dat[i.lev1,y] ~ dat[i.lev1,x.cont], 
       xlab = x.cont,
       ylab = y,
       xlim = xlims,
       ylim = ylims,col=cols[1])
  
  ## Plot level 2 of model (intercept+effect)
  points(dat[-i.lev1,y] ~ dat[-i.lev1,x.cont], 
         xlab = x.cont,
         ylab = y,
         col = cols[2])
  
  
  #Plot regession lines
  ##subset data for regression lines
  i.lev1 <- newdat[,x.cat] == levs[1]
  i.lev2 <- newdat[,x.cat] == levs[2]
  
  ##plot regression lines
  points(newdat$yhat ~ newdat[,x.cont], 
         subset = i.lev1,
         col = cols[1],
         type = "l")
  
  points(newdat$yhat ~ newdat[,x.cont], 
         subset = i.lev2,
         col = cols[2],
         type = "l")
  
  
}




