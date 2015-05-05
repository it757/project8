mytxt2 = read.table("/mapr/pmr1/user/pfmapr1/r_test/retail_prod_FI.txt",sep="|",header=TRUE)
mytxt2
nrow(mytxt2)

# Read Text File
mytxt = read.table("/mapr/pmr1/user/pfmapr1/r_test/test_01.txt",header=TRUE)
system.time(read.table("/mapr/pmr1/user/pfmapr1/r_test/test_01.txt"))
nrow(mytxt)


bash-4.1$ git add .
bash-4.1$ git commit -m "file saved"
bash-4.1$ git remote add origin git@github.com:it757/project8.git
bash-4.1$ git push -u origin master
bash-4.1$ git status
