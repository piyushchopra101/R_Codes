#
# Example R code to install packages

###########################################################
# Update this line with the R packages to install:

my_packages = c("plyr,stringi,lattice,hexbin,ggplot2,stringr")

###########################################################

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  else {
    cat(paste("Skipping already installed package:", p, "\n"))
  }
}
invisible(sapply(my_packages, install_if_missing))

myfunc<-function(){
  food<-read.csv("Food_Inspections.csv")
  mydata<-food
  
  food$Facility.Type<-sub("^$","Not_Defined",food$Facility.Type)
  food$Facility.Type<-as.factor(food$Facility.Type)
  
  food$Risk<-sub("^$","Risk 0",food$Risk)
  food$Risk<-as.factor(food$Risk)
  food$City<-mydata$City
  food$City<-sub("","CHICAGO",food$City)
  food$City<-sub("CCHICAGO","CHICAGO",food$City)
  food$City<-sub("CHCHICAGO","CHICAGO",food$City)
  food$City<-sub("Chicago","CHICAGO",food$City)
  food$City<-sub("chicago","CHICAGO",food$City)
  food$City<-sub("CHICAGO","CHICAGO",food$City)
  food$City<-sub("CHCICAGO","CHICAGO",food$City)
  food$City<-sub("alsip","ALSIP",food$City)
  food$Violations<-sub("^$","0. No violation",food$Violations)
  food$City<-as.factor(food$City)
  food$Zip<-as.factor(food$Zip)
  
  library(stringr)
  food$Num_Violations<-str_count(food$Violations,"[:digit:][:digit:]\\.")
  return (food)
  
}
myfood<-myfunc()

library(ggplot2)

#install.packages("hexbin")
library("hexbin")

library(lattice)

#x11()
mysample <- myfood[sample(1:nrow(food), 80010,replace=FALSE),]
#x11()
#plot(density(mysample$Num_Violations))

#x11()
#histogram(mysample$Num_Violations)
#dotplot(mysample$Num_Violations~mysample$Zip)
#x11()
dotplot(mysample$Facility.Type~mysample$Num_Violations,pch=5,cex.lab=3,width=3000,height=4000)

#install.packages("hexbin")
library("hexbin")

library(lattice)

#x11()
mysample <- myfood[sample(1:nrow(food), 80010,replace=FALSE),]
#x11()
#plot(density(mysample$Num_Violations))

#x11()
#histogram(mysample$Num_Violations)
#dotplot(mysample$Num_Violations~mysample$Zip)
#x11()
dotplot(mysample$Zip~mysample$Num_Violations,pch=5,cex.lab=3,width=3000,height=4000)

