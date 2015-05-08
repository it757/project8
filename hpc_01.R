# First part runs quickly - it takes 10 mins on laptop
setwd("/mapr/pmr1/user/pfmapr1/r_test") # change the path as needed unix needs "/" not "\"
cr <- read.csv("./cart_kn_pn.csv")
cr$KN_ADPT_CLS <- factor(cr$KN_ADPT_CLS)
summary(cr)
nrow(cr)
make.holdout <- function(data, build.pct=0.7) {
  s <- sample(nrow(data), round(build.pct*nrow(data)))
  list(build = data[s,], validate = data[-s,])
}

cr1 <- make.holdout(cr, 0.75)
names(cr1)

# Auto generate a formula  
make.formula <- function(all.vars, depvar, leaveout=c(), to.formula=TRUE) {
  all.vars <- setdiff(all.vars, c(depvar,leaveout))
  stringf <- paste(depvar,"~",paste(all.vars,sep="",collapse="+"),sep="")
  if (to.formula) as.formula(stringf) else stringf
}

fullformula <- make.formula(names(cr),"KN_ADPT_CLS",c("presc_num"))
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
  
  if(by[[1]] != "~")
    stop("Argument 'by' must be a one-sided formula.") 
  
  formc <- as.character(by[2]) 
  formc <- gsub(" ", "", formc)    
  if(!is.element(substring(formc, 1, 1), c("+", "-")))
    formc <- paste("+", formc, sep = "") 
  
  vars <- unlist(strsplit(formc, "[\\+\\-]"))    
  vars <- vars[vars != ""] # Remove any extra "" terms 
  
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

# RPART (CART-like tree building)
# install.packages("rpart") Install the package

library(rpart)
treeformula <- make.formula(names(cr),"KN_ADPT_CLS",c("ADPT_CLS","presc_id"))
treeformula
my.tree <- rpart(treeformula, data=cr1$build, na.action=na.exclude, method="class")
save(my.tree, file='tree_model.RData')


my.tree # text view of the tree
summary(my.tree)  
pdf("./my.tree1.pdf", width=8.5, height=10)
plot(my.tree, margin=.05, branch=.8, uniform=TRUE)
text(my.tree, use.n=TRUE, cex=.75)
dev.off()
my.tree.predict <- predict(my.tree, cr1$validate, type="prob")[,2]
head(my.tree.predict)
quantile(my.tree.predict)

binary.validate(my.tree, cr1$validate, "KN_ADPT_CLS", "Y", "prob")

names(my.tree)
table(my.tree$where)
my.tree$frame


help(rpart)


my.tree2 <- rpart(treeformula, data=cr1$build, na.action=na.exclude, method="class", cp=.0001, minbucket=3)  
my.tree2
pdf("./my.tree2.pdf", width=8.5, height=10)
plot(my.tree2, margin=.05, branch=.8, uniform=TRUE)
text(my.tree2, use.n=TRUE, cex=.75)
dev.off()
binary.validate(my.tree2, cr1$validate, "KN_ADPT_CLS", "Y", "prob") 

pdf("./cp.tree2.pdf", width=8.5, height=10)
cp.tree2 <- plotcp(my.tree2)
printcp(my.tree2)
dev.off() 
optimal.cp <- 0.00846

my.tree.final <- prune(my.tree2, cp=optimal.cp)
my.tree.final
binary.validate(my.tree.final, cr1$validate, "KN_ADPT_CLS", "Y", "prob")   

my.tree2$cptable # just a matrix
optimal.cp <- my.tree2$cptable[,"CP"][which.min(my.tree2$cptable[,"xerror"])]   

rpart.ctrl <- rpart.control(minsplit = 60, minbucket = 25, cp = 0.0001, maxdepth = 2)

rpart.tree.cycle <- function(data, s) {
  r <- apply(s, 1, function(x) {
    thissubset <- data[,x]
    thissubset$KN_ADPT_CLS <- data$KN_ADPT_CLS
    thisformula <- make.formula(names(thissubset), "KN_ADPT_CLS", c("ADPT_CLS","presc_id"))
    rpart(thisformula, data = thissubset, method = "class", control = rpart.ctrl)
  })
  r
}


sim.samples <- t(replicate(20, sample(ncol(cr1$build), round(.3*ncol(cr1$build)), replace = FALSE)))
sim.samples

my.trees <- rpart.tree.cycle(cr1$build, sim.samples)
my.trees[[1]]
my.trees[[12]]

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


# RANDOM FOREST - It takes more than a day to run

#install.packages("randomForest")
library(randomForest)

rf.formula <- make.formula(names(cr), "KN_ADPT_CLS", c("ADPT_CLS","presc_id"))
my.rf <- randomForest(rf.formula, data=cr1$build, na.action=na.exclude, ntree=500, nodesize=10, ntry=4, keep.forest=TRUE, importance=TRUE)
my.rf

save(my.rf, file='my.rf.RData')

predicted <- predict(my.rf,cr1$validate,type="prob")[,2]
actual <- (cr1$validate$KN_ADPT_CLS=="Y")
compute.decilemetrics(actual,predicted)

my.rf2 <- randomForest(rf.formula, data=cr1$build, na.action=na.exclude, ntree=500, nodesize=10, ntry=4, keep.forest=TRUE, importance=TRUE)
my.rf2$importance


sort.data.frame(data.frame(x=row.names(my.rf2$importance),importance=my.rf2$importance[,4],stringsAsFactors=FALSE),~-importance)
varImpPlot(my.rf2)

predicted <- predict(my.rf2,cr1$validate,type="prob")[,2]
actual <- (cr1$validate$KN_ADPT_CLS=="Y")
compute.decilemetrics(actual,predicted)

head(actual,n=1000)
head(predicted,n=100)

save(my.rf2, file='my.rf2.RData')

