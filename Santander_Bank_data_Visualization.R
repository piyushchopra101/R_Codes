library(glmnet)
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(dtplyr)
library(xgboost)

rm(list=ls())

setwd("C:/Users/S K Khare/Desktop/data mining 1/project")

# read train and test data

set.seed(1)
train_data<- fread("train_ver2.csv",nrows=-1)

test_data <- fread("test_ver2.csv",nrows=-1)




# Doing stuff
train_data[train_data$ncodpers==15889,]

### Trying stuff given in forums

# Data cleaning and visualisation

#  cool trick to avoid repetitive code in ggplot2 is to save/reuse your own theme

my_theme <- theme_bw() +
    theme(axis.title=element_text(size=24),
          plot.title=element_text(size=36),
          axis.text =element_text(size=16))

my_theme_dark <- theme_dark() +
    theme(axis.title=element_text(size=24),
          plot.title=element_text(size=36),
          axis.text =element_text(size=16))

# for checking out trainng data(pretty useless)
unique.id    <- unique(train_data$ncodpers)
limit.people <- 3.5e5
unique.id    <- unique.id[sample(length(unique.id),limit.people)]

train_data           <- train_data[train_data$ncodpers %in% unique.id,]
str(train_data1)

# for getting a validation set 

train_data$fecha_dato <- as.POSIXct(strptime(train_data$fecha_dato,format="%Y-%m-%d"))
train_data$fecha_alta <- as.POSIXct(strptime(train_data$fecha_alta,format="%Y-%m-%d"))
unique(train_data$fecha_dato)

# Gettig a month column
train_data$month <- month(train_data$fecha_dato)

# if Missing values in any columns
sapply(train_data,function(x)any(is.na(x)))

#Age distribution
ggplot(data=train_data1,aes(x=age)) + 
    geom_bar(alpha=0.75,fill="tomato",color="black") +
    ggtitle("Age Distribution") + 
    my_theme

# removing outliers and missing values

train_data$age[(train_data$age < 18)]  <- mean(train_data$age[(train_data$age >= 18) & (train_data$age <=30)],na.rm=TRUE)
train_data$age[(train_data$age > 100)] <- mean(train_data$age[(train_data$age >= 30) & (train_data$age <=100)],na.rm=TRUE)
train_data$age[is.na(train_data$age)]  <- median(train_data$age,na.rm=TRUE)
train_data$age <- round(train_data$age)

# Missing values in if customer is new or not?

sum(is.na(train_data$ind_nuevo))

# Finding how many months these missing customers have been active

months.active <- train_data[is.na(train_data$ind_nuevo),] %>%
    group_by(ncodpers) %>%
    summarise(months.active=n())  %>%
    select(months.active)

# seeing how distribution of active months
ggplot(data=months.active,aes(x=months.active)) + 
    +     geom_bar(alpha=0.75,fill="tomato",color="black")

# Conclusion- mostly new
# so giving ind_nuevo values "1" to missing values
train_data$ind_nuevo[is.na(train_data$ind_nuevo)] <- 1 

# Now, antiguedad

sum(is.na(train_data$antiguedad))

#Checking if same data causing issue

summary(train_data[is.na(train_data$antiguedad),]%>%select(ind_nuevo))

# Givivng least seiority to these people

train_data$antiguedad[is.na(train_data$antiguedad)] <- min(train_data$antiguedad,na.rm=TRUE)
train_data$antiguedad[train_data$antiguedad<0]      <- 0


# Givivng missing enteries in joining date a median value

train_data$fecha_alta[is.na(train_data$fecha_alta)] <- median(train_data$fecha_alta,na.rm=TRUE)

# Working on "indrel" the primary status of the 

table(train_data$indrel)

# Filling in missing values with the more common status

train_data$indrel[is.na(train_data$indrel)] <- 1

# Removing un-important features " cod_prov " &  "tipodom" 

train_date <- train_data %>% select(-tipodom,-cod_prov)

# Woring on "ind_actividad_cliente" 

sum(is.na(train_data$ind_actividad_cliente)
    
# Seting to median value

train_data$ind_actividad_cliente[is.na(train_data$ind_actividad_cliente)] <- median(train_data$ind_actividad_cliente,na.rm=TRUE)

# Working on nomprov - "Names of the city

unique(train_data$nomprov)
sum(is.na(train_data$nomprov))
    
# Replacing "" by "UNKNOWN"

train_data$nomprov[train_data$nomprov==""] <- "UNKNOWN"

# Now for gross income, aka renta

sum(is.na(train_data$renta))

# breaking down reanta by region because the missihg values are too large tobe set to median

train_data %>%
    filter(!is.na(train_data$renta)) %>%
    group_by(train_data$nomprov) %>%
    summarise(med.income = median(train_data$renta)) %>%
    arrange(med.income) %>%
    mutate(city=factor(train_data$nomprov,levels=train_data$nomprov)) %>% # the factor() call prevents reordering the names
    ggplot(aes(x=city,y=med.income)) + 
    geom_point(color="#c60b1e") + 
    guides(color=FALSE) + 
    xlab("City") +
    ylab("Median Income") +  
    my_theme + 
    theme(axis.text.x=element_blank(), axis.ticks = element_blank()) + 
    geom_text(aes(x=city,y=med.income,label=city),angle=90,hjust=-.25) +
    theme(plot.background=element_rect(fill="#c60b1e"),
          panel.background=element_rect(fill="#ffc400"),
          panel.grid =element_blank(),
          axis.title =element_text(color="#ffc400"),
          axis.text  =element_text(color="#ffc400"),
          plot.title =element_text(color="#ffc400",size=32)) +
    ylim(c(50000,200000)) +
    ggtitle("Income Distribution by City")

#There's a lot of variation, so I think assigning missing incomes by providence is a good idea. This code gets kind of confusing in a nested SQL statement kind of way, 
#but the idea is to first group the data by city, and reduce to get the median. 
#This intermediate data frame is joined by the original city names to expand the aggregated median incomes, ordered so that there is a 1-to-1 mapping between the rows, and finally the missing values are replaced.

new.incomes <-train_data %>%
    select(nomprov) %>%
    merge(train_data %>%
    group_by(nomprov) %>%
    summarise(med.income=median(renta,na.rm=TRUE)),by="nomprov") %>%
    select(nomprov,med.income) %>%
    arrange(nomprov)
train_data <- arrange(train_data,nomprov)
train_data$renta[is.na(train_data$renta)] <- new.incomes$med.income[is.na(train_data$renta)]
rm(new.incomes)

train_data$renta[is.na(train_data$renta)] <- median(train_data$renta,na.rm=TRUE)
train_data <- arrange(train_data,fecha_dato)

# Last of missing  values

sum(is.na(train_data$ind_nomina_ult1))

# Because such a small number, I assume each to be "0"
train_data[is.na(train_data$ind_nomina_ult1)] <- 0

#To find empty strings
char.cols <- names(train_data)[sapply(train_data,is.character)]
for (name in char.cols){
    print(sprintf("Unique values for %s:", name))
    print(unique(train_data[[name]]))
    cat('\n')
}

# based on that and the definitions of each variable, I will fill the empty strings 
#either with the most common value or create an unknown category based on what I think makes more sense.

train_data$indfall[train_data$indfall==""]                 <- "N"
train_data$tiprel_1mes[train_data$tiprel_1mes==""]         <- "A"
train_data$indrel_1mes[train_data$indrel_1mes==""]         <- "1"
train_data$indrel_1mes[train_data$indrel_1mes=="P"]        <- "5" # change to just numbers because it currently contains letters and numbers
train_data$indrel_1mes                             <- as.factor(as.integer(train_data$indrel_1mes))
train_data$pais_residencia[train_data$pais_residencia==""] <- "UNKNOWN"
train_data$sexo[train_data$sexo==""]                       <- "UNKNOWN"
train_data$ult_fec_cli_1t[train_data$ult_fec_cli_1t==""]   <- "UNKNOWN"
train_data$ind_empleado[train_data$ind_empleado==""]       <- "UNKNOWN"
train_data$indext[train_data$indext==""]                   <- "UNKNOWN"
train_data$indresi[train_data$indresi==""]                 <- "UNKNOWN"
train_data$conyuemp[train_data$conyuemp==""]               <- "UNKNOWN"
train_data$segmento[train_data$segmento==""]               <- "UNKNOWN"

train_data <- data.frame(train_data)

# Getting features

features <- grep("ind_+.*ult.*",names(train_data))

# Convert all the features to numeric dummy indicators & add new feature total variables

train_data[,features]     <- lapply(train_data[,features],function(x)as.integer(round(x)))
train_data1 <- train_data[,features]
train_data$total.services <- rowSums(train_data1,na.rm=TRUE)

# To study trends in customers adding or removing services

train_data   <- train_data %>% arrange(fecha_dato)
train_data$month.id      <- as.numeric(factor((train_data$fecha_dato)))
train_data$month.next.id <- train_data$month.id + 1

status.change <- function(x){
    if ( length(x) == 1 ) { # if only one entry exists, I'll assume they are a new customer and therefore are adding services
        label = ifelse(x==1,"Added","Maintained")
    } else {
        diffs <- diff(x) # difference month-by-month
        diffs <- c(0,diffs) # first occurrence will be considered Maintained, which is a little lazy. A better way would be to check if the earliest date was the same as the earliest we have in the dataset and consider those separately. Entries with earliest dates later than that have joined and should be labeled as "Added"
        label <- rep("Maintained", length(x))
        label <- ifelse(diffs==1,"Added",
                        ifelse(diffs==-1,"Dropped",
                               "Maintained"))
    }
    label
}

train_data[,features] <- lapply(train_data[,features], function(x) return(ave(x,train_data$ncodpers, FUN=status.change)))

interesting <- rowSums(train_data[,features]!="Maintained")
train_data  <- train_data[interesting>0,]
train_data  <- train_data %>%
    gather(key=feature,
           value=status,
           ind_ahor_fin_ult1:ind_recibo_ult1)
train_data  <- filter(train_data,status!="Maintained")


train_data2 <- train_data[train_data$status=="Added",]


train_data2 <- train_data2[train_data2$fecha_dato =="2015-05-28" | train_data2$fecha_dato =="2016-05-28",]

cross_validation <- sample_frac(train_data[train_data$fecha_dato=="2016-05-28",],size=0.75)

train_data3<-anti_join(train_data2,cross_validation)


# Result


