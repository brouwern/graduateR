# A typical R script


## Introduction

#We will re-make the figures that appeared in the following publications.
#Figure 1b, Yi P and Melton DA.  2014.  Perspectives on the Activities
#of ANGPTL8/Betatrophin.  Cell 159:467-468.

#Figure 5b, Yi P, Park JS, Melton DA (2013) Betatrophin:a hormone that controls pancreatic beta cell proliferation. Cell 153:747-758.

#These figures present the same data but in somewhat different forms.  The original publication (Yi et al 2013) presented the raw data; the follow up (Yi et al 2014) presented the raw data in a dot plot.


  
  
# Learning objectives
  
##   Become familiar with an R script
## Become familiar with RMarkdown documents
## Load R packages
## Load data
## Make a barplot.


#The data
  
##**Data from:**
##Figure 1b, Yi P and Melton DA.  2014.  Perspectives on the Activities
##of ANGPTL8/Betatrophin.  Cell 159:467-468.

##**Data Originally presented:**
##Figure 5b, Yi P, Park JS, Melton DA (2013) Betatrophin:a hormone that controls pancreatic beta cell proliferation. Cell 153:747-758.


## Load the data

## Normally we load data into R from a spreadsheet saved as .csv file.  
## To make things super easy, and b/c the dataset is tiny, we'll load it by hand

### Make "vectors" to hold the data


#Control: "beta-cell proliferation ratio" (Ki67/insulin) from gfp-treated animals
gfp <- c(0.56,0.32,0.24,0.32,0.56)

#Treatment:"beta-cell proliferation ratio" (Ki67/insulin) from cells treated with betatropin
betat <- c(0.71,0.79,1.43,2.14,7.70,8.65,8.73)

mean(betat)


#We can queary R about how long each of these "vectors" are using the **length()** command

length(gfp)
length(betat)




### Make "dataframe" to hold the data

#**"dataframes"**" are the principal way we work with data in R.

#This code is a bit complex for a beginner - don't worry about what its doing.


beta.df <- data.frame(trt = c(rep("gfp",5),
                              rep("beta.T",7)),
                      response = c(gfp,betat))




#The data now looks like this

beta.df


#Data in R almost alywas is organized in "long" format with each row being a seperate observation, organism, etc.


  
  
#We can see how big our dataframe is uing the **dim()** command.

dim(beta.df)

