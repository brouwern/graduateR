#plot model equations in margin

# sx_mtext_plot_equations_in_margin


mod <- lm.mass


as.formula(lm.mass$call)[3]

m.equations <- function(mod){
  coefs.out <- coef(mod)
  coefs.out <- round(coefs.out,2)
  y <- as.character(as.formula(lm.mass$call)[2])
  x <- as.character(as.formula(lm.mass$call)[3])
  mod.gen <- "y  = m*x + b"
  mod.a <- paste(y," = slope*",x," + intercept", sep = "")
  mod.b <- paste(y," = ",coefs.out[2],
               "*",names(coefs.out)[2]," + ",
               coefs.out[1],sep = "")
  
  mtext(text = "Regression equation:",
        side = 3,adj= 0, line = 3)
  mtext(text = mod.gen,
        side = 3,adj= 0, line = 2)
  mtext(text = mod.a,side = 3,adj= 0, line = 1)
  mtext(text = mod.b,side = 3,adj= 0, line = 0)
}
