setwd("/mapr/pmr1/user/pfmapr1/r_test")
#source("additional functions.r")

cr <- read.csv("./cart_rf_smpl.csv")
cr$ADOPT_SUB_CLASS <- factor(cr$ADOPT_SUB_CLASS)
summary(cr)

make.holdout <- function(data, build.pct=0.7) {
  s <- sample(nrow(data), round(build.pct*nrow(data)))
  list(build = data[s,], validate = data[-s,])
}

cr1 <- make.holdout(cr, 0.75)
names(cr1)

# function to auto generate a formula so we don't always have to type it out, or know what variables are in the data frame
make.formula <- function(all.vars, depvar, leaveout=c(), to.formula=TRUE) {
  all.vars <- setdiff(all.vars, c(depvar,leaveout))
  stringf <- paste(depvar,"~",paste(all.vars,sep="",collapse="+"),sep="")
  if (to.formula) as.formula(stringf) else stringf
}

fullformula <- make.formula(names(cr),"ADOPT_SUB_CLASS",c("presc_num"))
fullformula

binary.validate <- function(model, newdata, depvar, response.category, type="class") {
  predicted <- predict(model,newdata,type=type)
  if (type=="prob") predicted<-predicted[,2]
  actual <- (newdata[[depvar]] ==  response.category)
  compute.decilemetrics(actual,predicted)
}
compute.decilemetrics <- function(actual,predicted) {
  n<-length(actual)
  rn <- runif(n)
  newdata <- data.frame(predicted=predicted, actual = actual, rn=rn )
  newdata <- subset(newdata,!is.na(predicted) & !is.na(actual))
  newdata <- sort.data.frame(newdata,~-predicted+rn)  # sort it by the predicted value top to bottom and break ties by the random number we put on before
  newdata$decile <- ceiling(10 * (1:nrow(newdata)) / nrow(newdata))  # assign deciles
  tapply(newdata$actual, newdata$decile, mean)  # response percentage by predicted decile, for use in lift charts
}

sort.data.frame <- function(x, by){
  # x: A data.frame
  # by: A one-sided formula using + for ascending and - for descending
  #     Sorting is left to right in the formula
  
  # Useage is:
  # library(nlme);
  # data(Oats)
  # sort(Oats, by= ~nitro-Variety)
  
  if(by[[1]] != "~")
    stop("Argument 'by' must be a one-sided formula.")
  
  # Make the formula into character and remove spaces
  formc <- as.character(by[2]) 
  formc <- gsub(" ", "", formc) 
  # If the first character is not + or -, add +
  if(!is.element(substring(formc, 1, 1), c("+", "-")))
    formc <- paste("+", formc, sep = "")
  
  # Extract the variables from the formula
  vars <- unlist(strsplit(formc, "[\\+\\-]"))    
  vars <- vars[vars != ""] # Remove any extra "" terms
  
  # Build a list of arguments to pass to "order" function
  calllist <- list()
  pos <- 1 # Position of + or -
  for(i in 1:length(vars)){
    varsign <- substring(formc, pos, pos)
    pos <- pos + 1 + nchar(vars[i])
    if(is.factor(x[, vars[i]])){
      if(varsign == "-") {
        calllist[[i]] <- -rank(x[, vars[i]])
      } else {
        calllist[[i]] <- rank(x[, vars[i]])
      }
    } else {
      if(varsign == "-") {
        calllist[[i]] <- -x[, vars[i]]
      } else {
        calllist[[i]] <- x[,vars[i]]
      }
    }
  }
  return(x[do.call("order", calllist), ])
}
#### TREES
# 
# RPART (CART-like tree building)
# 

library(rpart)
treeformula <- make.formula(names(cr),"ADOPT_SUB_CLASS",c("state","HCP_Primary_Specialty","presc_num"))
treeformula
my.tree <- rpart(treeformula, data=cr1$build, na.action=na.exclude, method="class")

# evaluation of the resulting tree
my.tree # indented text view of the tree
summary(my.tree) # overview of the building process
plot(my.tree, margin=.05, branch=.8, uniform=TRUE)
text(my.tree, use.n=TRUE, cex=.75)
my.tree.predict <- predict(my.tree, cr1$validate, type="prob")[,2]
head(my.tree.predict)
quantile(my.tree.predict)

binary.validate(my.tree, cr1$validate, "ADOPT_SUB_CLASS", "Y", "prob")


##output components
# where
# frame (one row per node in tree for further analysis)
# splits

names(my.tree)
table(my.tree$where)
my.tree$frame


help(rpart)
# options method='anova', 'class', 'exp', 'poisson'
# cp (stopping criterion complexity parameter)
# parms=list(split="gini") or parms=list(split="information")
# minsplit, minbucket=minsplit/3
# surrogate


# "cp" to be more or less aggresssive with tree building
my.tree2 <- rpart(treeformula, data=cr1$build, na.action=na.exclude, method="class", cp=.0001, minbucket=3)  # lower cp and minbucket to possibly get bigger tree
my.tree2
plot(my.tree2, margin=.05, branch=.8, uniform=TRUE)
text(my.tree2, use.n=TRUE, cex=.75)
binary.validate(my.tree2, cr1$validate, "ADOPT_SUB_CLASS", "Y", "prob")  # not nearly as strong as the smaller tree

# do some analysis to see a good value of cp to use
# this shows us the results of rpart's internal cross-validation
plotcp(my.tree2)
printcp(my.tree2)
# let's use cp=0.00846 since this minimizes xerror
optimal.cp <- 0.00846

# finalize tree by 'pruning' it by 'cp' value
my.tree.final <- prune(my.tree2, cp=optimal.cp)
my.tree.final
binary.validate(my.tree.final, cr1$validate, "ADOPT_SUB_CLASS", "Y", "prob")  # not nearly as strong as the smaller tree


#note my.tree2$cptable component can be used to automate this instead of manually inspecting the cp table
my.tree2$cptable # just a matrix
optimal.cp <- my.tree2$cptable[,"CP"][which.min(my.tree2$cptable[,"xerror"])]  
# get the value from the CP column that corresponds to the minimum value from the xerror column


### Find interaction terms (a random forest-like technique)
rpart.ctrl <- rpart.control(minsplit = 60, minbucket = 25, cp = 0.0001, maxdepth = 2)

rpart.tree.cycle <- function(data, s) {
  r <- apply(s, 1, function(x) {
    thissubset <- data[,x]
    thissubset$ADOPT_SUB_CLASS <- data$ADOPT_SUB_CLASS
    thisformula <- make.formula(names(thissubset), "ADOPT_SUB_CLASS", c("ADOPT_SUB_CLASS","state","HCP_Primary_Specialty","presc_num"))
    rpart(thisformula, data = thissubset, method = "class", control = rpart.ctrl)
  })
  r
}


sim.samples <- t(replicate(20, sample(ncol(cr1$build), round(.3*ncol(cr1$build)), replace = FALSE)))
sim.samples

my.trees <- rpart.tree.cycle(cr1$build, sim.samples)
my.trees[[1]]
my.trees[[12]]

# function to parse through these trees and find terms used in first two splits
get.interacting.vars <- function(trees, return.unique=TRUE, return.sorted=TRUE) {
  interactions <- data.frame(x1=character(),x2=character(),stringsAsFactors=FALSE)
  
  for (tr in 1:length(trees)) {
    vs <- character()
    a <- trees[[tr]]$splits
    if (!is.null(a) && nrow(a)>0) {
      f <- TRUE
      for (i in 1:nrow(a)) {
        if(f && a[i,1] > 0) {
          vs <- c(vs, row.names(a)[i])
          f <- FALSE
        } else if (a[i,1]==0) {
          f <- TRUE
        }
      }
    }
    vs <- unique(vs)
    if (length(vs)>1) {
      for (j in 2:length(vs)) {
        thisv <- sort(c(vs[1],vs[j]))
        interactions <- rbind(interactions, data.frame(x1=thisv[1],x2=thisv[2],stringsAsFactors=FALSE))
      }
    }
  }
  
  if (return.unique) interactions <- unique(interactions)
  if (return.sorted) interactions <- sort.data.frame(interactions, by = ~x1+x2)
  interactions
}  

my.interactions <- get.interacting.vars(my.trees)
my.interactions
my.newterms <- paste(my.interactions$x1,":", my.interactions$x2, sep="", collapse="+")
my.newterms


##### RANDOM FOREST
library(randomForest)

my.rf <- randomForest(ADOPT_SUB_CLASS~HCP_Yr_Experience+Eigenvector_Centrality+HCP_Affiliation_Count+Degree+In_Degree+Out_Degree,
                      data=cr1$build, na.action=na.exclude, ntree=500, nodesize=10, ntry=4, keep.forest=TRUE, importance=TRUE)
my.rf

predicted <- predict(my.rf,cr1$validate,type="prob")[,2]
actual <- (cr1$validate$ADOPT_SUB_CLASS=="Y")
compute.decilemetrics(actual,predicted)


## options
# ntree = 500 #number of trees to grow
# mtry # number of variables sampled at each split
# nodesize # minimum size for leaf nodes (default value is very small 1 or 5)
# information to get returned: keep.forest, importance

my.rf2 <- randomForest(ADOPT_SUB_CLASS ~ HCP_Yr_Experience+Eigenvector_Centrality+HCP_Affiliation_Count+Degree+In_Degree+Out_Degree,
                       data=cr1$build, na.action=na.exclude, ntree=500, nodesize=10, ntry=4, keep.forest=TRUE, importance=TRUE)
my.rf2$importance
# my.rf2$forest # holds details of the entire forest of trees

# view ranked importance of variables - easiset way to see the role each variable played across the trees
sort.data.frame(data.frame(x=row.names(my.rf2$importance),importance=my.rf2$importance[,4],stringsAsFactors=FALSE),~-importance)
varImpPlot(my.rf2)

predicted <- predict(my.rf2,cr1$validate,type="prob")[,2]
actual <- (cr1$validate$ADOPT_SUB_CLASS=="Y")
compute.decilemetrics(actual,predicted)

head(actual,n=1000)
head(predicted,n=100)

