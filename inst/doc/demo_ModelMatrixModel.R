## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
#devtools::install_github("xinyongtian/R_ModelMatrixModel") #install from github
rm(list=ls())
library(ModelMatrixModel)
set.seed(10)
traindf= data.frame(x1 = sample(LETTERS[1:5], replace = T, 20),
                 x2 = rnorm(20, 100, 5),
                 x3 = factor(sample(c("U","L","P"), replace = T, 20)),
                 y = rnorm(20, 10, 2))
set.seed(20)
newdf=data.frame(x1 = sample(LETTERS[1:5], replace = T, 3),
                 x2 = rnorm(3, 100, 5),
                 x3 = sample(c("U","L","P"), replace = T, 3)) 

head(traindf)
sapply(traindf,class) #input categorical variable can be either character or factor

## -----------------------------------------------------------------------------
f1=formula("~x1+x2")
head(model.matrix(f1, traindf),2)
head(model.matrix(f1, newdf),2)

## -----------------------------------------------------------------------------
f2=formula("~ 1+x1+x2") # "1" is need in order to output intercept column
mm=ModelMatrixModel( f2,traindf,remove_1st_dummy =T,sparse = F)

## -----------------------------------------------------------------------------
class(mm)
head(mm$x,2) #note "_Intercept_" is intercept column

## -----------------------------------------------------------------------------
mm_pred=predict(mm,newdf)
head(mm_pred$x,2)

## -----------------------------------------------------------------------------
mm=ModelMatrixModel(~x1+x2+x3,traindf,remove_1st_dummy = F) 

## -----------------------------------------------------------------------------
data.frame(as.matrix(head(mm$x,2)))
mm_pred=predict(mm,newdf)
data.frame(as.matrix(head(mm_pred$x,2)))

## -----------------------------------------------------------------------------
mm=ModelMatrixModel(~x2+x3+x2:x3,traindf) 
data.frame(as.matrix(head(mm$x,2))) # ':' in column name  is replaced with '_X_'
mm_pred=predict(mm,newdf)
data.frame(as.matrix(head(mm_pred$x,2)))

## -----------------------------------------------------------------------------
mm=ModelMatrixModel(~x2*x3,traindf,remove_1st_dummy = T) 
data.frame(as.matrix(head(mm$x,2)))
mm_pred=predict(mm,newdf)
data.frame(as.matrix(head(mm_pred$x,2)))

## -----------------------------------------------------------------------------
mm=ModelMatrixModel(~x2+x3,traindf) 
data.frame(as.matrix(head(mm$x,2)))
newdf2=newdf
newdf2[1,'x3']='z'  #create invalid level
mm_pred=predict(mm,newdf2,handleInvalid = "keep") 

## -----------------------------------------------------------------------------
data.frame(as.matrix(head(mm_pred$x,2))) 

## -----------------------------------------------------------------------------
mm=ModelMatrixModel(~poly(x2,3)+x3,traindf) 
data.frame(as.matrix(head(mm$x,2)))
mm_pred=predict(mm,newdf)
data.frame(as.matrix(head(mm_pred$x,2)))

## -----------------------------------------------------------------------------
mm=ModelMatrixModel(~poly(x2,3,raw=T)+x3, traindf) 
data.frame(as.matrix(head(mm$x,2)))
mm_pred=predict(mm,newdf)
data.frame(as.matrix(head(mm_pred$x,2)))

## -----------------------------------------------------------------------------
mm=ModelMatrixModel(~x2+x3,traindf,scale = T,center = T) 
data.frame(as.matrix(head(mm$x,2)))
mm_pred=predict(mm,newdf)
data.frame(as.matrix(head(mm_pred$x,2)))

