mytxt2 = read.table("/mapr/pmr1/user/pfmapr1/r_test/retail_prod_FI.txt",sep="|",header=TRUE)
mytxt2
nrow(mytxt2)

# Read Text File
mytxt = read.table("/mapr/pmr1/user/pfmapr1/r_test/test_01.txt",header=TRUE)
system.time(read.table("/mapr/pmr1/user/pfmapr1/r_test/test_01.txt"))
nrow(mytxt)
