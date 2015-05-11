# Read Text File
mytxt = read.table("/mapr/pmr1/user/pfmapr1/r_test/test_01.txt",header=TRUE)
system.time(read.table("/mapr/pmr1/user/pfmapr1/r_test/test_01.txt"))
nrow(mytxt)

mytxt2 = read.table("/mapr/pmr1/user/pfmapr1/r_test/retail_prod_FI.txt",sep="|",header=TRUE)
mytxt2
nrow(mytxt2)


# Load package for Excel file and read it
library(xlsx)
myxlsx = read.xlsx("/mapr/pmr1/user/pfmapr1/r_test/TEST_DATA1.xlsx",1)
system.time(read.xlsx("/mapr/pmr1/user/pfmapr1/r_test/TEST_DATA1.xlsx",1))
nrow(myxlsx)

# Read CSV File
mycsv = read.csv("/mapr/pmr1/user/pfmapr1/r_test/test_01.csv")
system.time(read.csv("/mapr/pmr1/user/pfmapr1/r_test/test_01.csv"))
nrow(mycsv)

# Load package to read SAS Dataset 
library(Hmisc)
mydata <- sasxport.get("/mapr/pmr1/user/pfmapr1/r_test/test_01.xpt")
system.time(sasxport.get("/mapr/pmr1/user/pfmapr1/r_test/test_01.xpt"))
nrow(mydata)

# Write a table to Text File
write.table(mydata,file="/mapr/pmr1/user/pfmapr1/r_test/rtotxt.txt")
system.time(write.table(mydata,file="/mapr/pmr1/user/pfmapr1/r_test/rtotxt.txt"))
nrow(mydata)

# Write a table to CSV File
write.csv(mydata,"/mapr/pmr1/user/pfmapr1/r_test/test_02.csv")
system.time(write.csv(mydata,"/mapr/pmr1/user/pfmapr1/r_test/test_02.csv"))
nrow(mydata)


#R Procedures
summary(mydata)
system.time(summary(mydata))
nrow(mydata)
system.time(nrow(mydata))
sum(mydata$quintile)
system.time(sum(mydata$quintile))
min(mydata$quintile)
system.time(min(mydata$quintile))
max(mydata$quintile)
system.time(max(mydata$quintile))
mean(mydata$quintile)
system.time(mean(mydata$quintile))
table(mydata$state)
system.time(table(mydata$state))
table(mydata$RBU)
system.time(table(mydata$rbu))
subset(mydata,mydata$state == "AK")
system.time(sum(mydata$state == "AK"))
subset(mydata,mydata$state != "AK")
system.time(sum(mydata$state != "AK"))


# Load Teradata Package
library("teradataR")

# Define the paths where Teradata JDBC Drivers are saved
path1="/home/mohami11/tdgssconfig.jar"
path2="/home/mohami11/terajdbc4.jar"

# rJava code to direct to classpath
.jinit()
.jaddClassPath(path1)
.jaddClassPath(path2)

# Passing Teradata log in credentials
tdConnect("170.116.30.163", uid = "MOHAMI11", pwd = "q_12qF78", database = "TRGTADHOC", dType = c("jdbc"))

# Create a string for select statement
query="select * from TRGTADHOC.sas2td_t2"

df <- tdQuery(query)
head(df)

summary(df)

# Create an object (tdf) with the table X from database TRGTADHOC
tdf <- td.data.frame("sas2td_t2", "TRGTADHOC")
tdf
str(tdf)

# Convert TD Dataframe into local data frame
td = as.data.frame.td.data.frame(tdf)
head(td)
summary(td)

# Write TD table into Text File
write.table(td,file="tdtotxt.txt",row.names=FALSE)

# Run Linux commands on R Editor
system("cat tdtotxt.txt")
system("ls -ltr")

#push new code to github
system("git add .")
system("git commit -m 'Program is saved'")
system("git push -u origin master")
system("git status")
system("pwd")


