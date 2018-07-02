###### PHANEROZOIC MAMMALIAN PHYLOGENETIC CLUSTERING R CODE ######
## Written by GREGORY J. SMITH ##

rm(list=ls())

library(ggplot2) #plotting
library(dplyr) #data wrangling
library(stats) #to calculate the correlation coefficient - Rcl
# library(tibble) #I don't remember what this is for
# library(MASS) #I don't remember what this is for
library(paleobioDB) #to read in fossil occurrence data for Invertebrate Classes

# setwd("D:/School/Projects/Phylogenetic Clustering (new directions)/Phylogenetic Clustering Directory") #set wd to project folder (Surface Pro)
setwd("E:/School/Projects/Phylogenetic Clustering (new directions)/Phylogenetic Clustering Directory") #set wd to project folder (office desktop)

data <- read.csv('GENERA.csv',header=TRUE,stringsAsFactors = FALSE) #Read in data (MIOMAP and FAUNMAP Mammal Occurrence Data)
data <- filter(data, !order %in% c("Cetacea","Sirenia")) #Remove aquatic genera
data<-data[,1:17] #Remove extra columns in this csv file
data <- as.data.frame(data)

data$ma_max[data$ma_max == ""] <- NA
data$ma_min[data$ma_min == ""] <- NA
data$ma_min <- as.numeric(data$ma_min)
data$ma_max <- as.numeric(data$ma_max)

#### USE OCCURRENCE DATA TO FIND FAD, LAD AND DURATIONS FOR ALL TAXA ####
taxa<-data$genus #What are all occurrences appearing in this Period?
taxa.names<-unique(taxa) #What are the specific species names appearing in this Period?
num.taxa<-length(taxa.names) #How many species are there? (will be number of rows in the results matrix)
outTable<-matrix(NA,nrow=num.taxa,ncol=4)
colnames(outTable)<-c("FAD","LAD","Duration","Family")
rownames(outTable)<-taxa.names

t1<-Sys.time()
for(i in 1:nrow(data)){
  
  this.taxon <- data$genus[i] #Define the taxa
  all.occ.this.taxon <- filter(data, genus == this.taxon) #Select all rows of that taxon
  FAD <- max(all.occ.this.taxon$ma_max, na.rm = T) #Find the max value (FAD) of that taxon...
  outTable[this.taxon,1] <- FAD #...and insert it into column 1 (FAD)
  LAD <- min(all.occ.this.taxon$ma_min, na.rm = T) #Find the min value (LAD) of that taxon...
  outTable[this.taxon,2] <- LAD #...and insert it into column 2
  longevity <- FAD - LAD #Calulate the species longevity...
  outTable[this.taxon,3] <- longevity #...and insert it into column 3
  Family <- all.occ.this.taxon$family[1] #Find the family that genus belongs to (the first one in case two are assigned)
  outTable[this.taxon,4] <- Family #...and insert the Family name into column 4
  outTable<-outTable
}
df <- as.data.frame(outTable)
df <- df[is.finite(rowSums(df)),]
t2<-Sys.time()
t2-t1
write.csv(outTable, "Mammalia_longevities.csv", row.names = T) #For the entire Phanerozoic

#### FUNCTION TO CALCULATE RCL ####
normalize <- function(x){ #Function to normalize, needed for Rcl calculation
  (x - mean(x))
}

Rcl.calc <- function(x,w){ #Function to Calculate relative clustering index; takes two matrices to calculate

  n <- nrow(x) #x is Mext or Morg
  So <- sum(w) #w is Mtax
  sigma <- c()
  sig.z <- c() #set up a vector 
  z <- normalize(x) #normalize Mext or Morg matrix

  for (i in 1:n){

    yi <- sum(z[i,]^2)
    sig.z <- c(sig.z, yi)

    for (j in 1:n){
      yj <- sum(z[i,]*z[,j]*w[i,j])
      sigma <- c(sigma, yj)
    }

    sum(sigma)

  }

  sum(sigma)/sum(sig.z) *(n/So) #compute Rcl

}

# Rcl.calc <- function(x,w){ #Calculate relative clustering index; takes two matrices to calculate
#   
#   n <- nrow(x) #x is Mext or Morg
#   So <- sum(w) #w is Mtax
#   z <- normalize(x)
#   sig.z <- sum(rowSums(z^2))
#   sigma <- sum(z[1:n,]*z[,1:n]*w[1:n,1:n])
#   
#   round(sigma/sig.z *(n/So),3) #compute Rcl
# }


#### BIN BY NALMA AND CALCULATE EXTINCTION AND ORIGINATION RATES AND RCL VALUES ####
df <- read.csv("Mammalia_longevities.csv",header=TRUE,row.names=1,na.strings=c("","NA")) #read dataset back in, remove blank cells
df <- df[Reduce(`&`, lapply(df, function(x) !is.na(x)  & is.finite(x))),] #remove infinity values

n.NALMAs<-c("Puercan","Torrejonian","Tiffanian","Clarkforkian", #Names of the NALMAs
            "Wasatchian","Bridgerian","Uintan","Duchesnean",
            "Chadronian","Orellan","Whitneyan","Arikareean",
            "Hemingfordian","Barstovian","Clarendonian","Hemphillian",
            "Blancan","Irvingtonian","Rancholabrean","Holocene","Survivors")

l.Mtax <- vector("list", length(n.NALMAs)) #Intialize a list for Mtax to iterate over
l.Morig <- vector("list", length(n.NALMAs)) #Initialize a list for Morig to iterate over
l.Mext <- vector("list", length(n.NALMAs)) #Initialize a list for Mext to iterate over

res.matrix <- matrix(NA, nrow = length(n.NALMAs), ncol=13) #Set up a results matrix for extinction rate calculation
rownames(res.matrix) <- n.NALMAs
colnames(res.matrix) <- c("Min Age", "Max Age", "Midpoint","Duration",
                          "Ni","Nfl","Nb","Nt","Nbt","p","q", "Rcl.O","Rcl.E")
res.matrix[,1] <- c(63.3,61.7,56.8,55.8,50.3,46.2,40.4,37.2,33.9,33.3,30.8,20.43,15.97,13.6,10.3,4.9,1.8,0.3,0.012,0,0) #Interval minimum ages, following Woodburne, 2004
res.matrix[,2] <- c(66,63.3,61.7,56.8,55.8,50.3,46.2,40.4,37.2,33.9,33.3,30.8,20.43,15.97,13.6,10.3,4.9,1.8,0.3,0.012,0) #Interval maximum ages, following Woodburne, 2004
res.matrix[,3] <- ((res.matrix[,2]+res.matrix[,1])/2) #Interval Midpoints
res.matrix[,4] <- res.matrix[,2] - res.matrix[,1] #Duration of Interval

t1<-Sys.time()
for(i in seq_along(n.NALMAs)){
  
  Nfl <- df %>% rownames_to_column('genus') %>% #Nfl = taxa that are singletons; i.e., occur in this interval only
    filter(LAD >= res.matrix[i,1] & FAD <= res.matrix[i,2]) %>% 
    column_to_rownames('genus') #Identify those taxa
  res.matrix[i,6] <- nrow(Nfl) #Insert that number into the results matrix
  
  Nb <- df %>% rownames_to_column('genus') %>% #Nb = taxa crossing the bottom of the interval; i.e., exist both in this interval and the previous one
    filter(LAD <= res.matrix[i,1] & FAD <= res.matrix[i,2] & FAD >= res.matrix[i,1]) %>% 
    column_to_rownames('genus') #Identify those taxa
  res.matrix[i,7] <- nrow(Nb) #Insert that number into the results matrix
  
  Nt <- df %>% rownames_to_column('genus') %>% #Nt = taxa crossing the top of the interval; i.e., exist both in this interval and the next one
    filter(LAD >= res.matrix[i,1] & FAD <= res.matrix[i,2] & FAD >= res.matrix[i,1]) %>% 
    column_to_rownames('genus') #Identify those taxa 
  res.matrix[i,8] <- nrow(Nt) #Insert that number into the results matrix
  
  Nbt <- df %>% rownames_to_column('genus') %>% #Nbt = taxa crossing both top and bottom of the interval
    filter(LAD <= res.matrix[i,1] & FAD >= res.matrix[i,2]) %>% 
    column_to_rownames('genus') #Identify those taxa
  res.matrix[i,9] <- nrow(Nbt) #Insert that number into the results matrix
  
  Ni <- rbind(Nfl, Nb, Nt, Nbt) #List of all taxa occurring in focal interval
  res.matrix[i,5] <- nrow(Ni) #Insert that number of taxa into the results matrix
  
  p <- round(-log((nrow(Nbt)+nrow(Nfl))/nrow(Nbt)), digits=3) #Calculate origination rate (p) on a per-interval basis (i.e., time-independent) 
  res.matrix[i,10] <- p #Insert origination rate into the results matrix
  
  q <- round(-log((nrow(Nbt)+nrow(Nb))/nrow(Nbt)), digits=3) #Calculate extinction rate (q) on a per-interval basis (i.e., time-independent) 
  res.matrix[i,11] <- q #Insert extinction rate into the results matrix
  
  extinct.taxa <- rbind(Nfl, Nb) #Identify those taxa which go extinct at the end of the interval
  num.extinct <- nrow(extinct.taxa) #How many taxa are there?
  origin.taxa <- rbind(Nfl, Nt) #Identify those taxa originating at the beginning of the interval
  num.origins <- nrow(origin.taxa) #How many taxa are there?
  
  l.Mtax[[i]] <- crossprod(table(Ni$Family,row.names(Ni))) #Muliply family by genera to create an n x n matrix, with any numeric values that exist being assinged to those genera that belong to the same families
  diag( l.Mtax[[i]]) <- 0 #Assign a 0 to the diagonals 
  l.Mtax[[i]][l.Mtax[[i]] > 0]<-1 #Assign a 1 to anything that returned a value (will only be those genera belonging to same families); keep 0 otherwise
  
  l.Morig[[i]]<-array(0,dim=c(nrow(Ni),nrow(Ni))) #create the origination matrix, assign a default value of 0 (no shared origination)
  rownames(l.Morig[[i]])<-row.names(Ni) #taxa names make the rows
  colnames(l.Morig[[i]])<-row.names(Ni) #taxa names make the columns
  
  l.Mext[[i]]<-array(0,dim=c(nrow(Ni),nrow(Ni))) #create the extinction matrix, assign a default value of 0 (no shared extinction)
  rownames(l.Mext[[i]])<-row.names(Ni) #taxa names make the rows
  colnames(l.Mext[[i]])<-row.names(Ni) #taxa names make the columns
  
  l.Morig[[i]][row.names(origin.taxa), row.names(origin.taxa)] <-1 #add these members to their position in the array made above, give them a value of 1
  l.Mext[[i]][row.names(extinct.taxa), row.names(extinct.taxa)] <-1 #add these members to their position in the array made above, give them a value of 1
  diag(l.Morig[[i]]) <- 0 #Assign a 0 to the diagonals 
  diag(l.Mext[[i]]) <- 0 #Assign a 0 to the diagonals 
  
  Rcl.O <- cor.test(l.Morig[[i]], l.Mtax[[i]], method = "spearman")$estimate #Calculate clustering index for this NALMA
  res.matrix[i,12] <- Rcl.O #And insert it into the results matrix

  Rcl.E <- cor.test(l.Mext[[i]], l.Mtax[[i]], method = "spearman")$estimate #Calculate clustering index for this NALMA
  res.matrix[i,13] <- Rcl.E #And insert it into the results matrix

}
t2<-Sys.time()
t2-t1


res.matrix <- as.data.frame(res.matrix) #Save result as a data.frame
names(l.Morig) <- paste0("Morig",n.NALMAs) #Name each matrix something useful, i.e., its interval name
list2env(l.Morig, env = .GlobalEnv) #Export the matrix to the global environment
names(l.Mext) <- paste0("Mext",n.NALMAs) #Name each matrix something useful, i.e., its interval name
list2env(l.Mext, env = .GlobalEnv) #Export the matrix to the global environment
names(l.Mtax) <- paste0("Mtax",n.NALMAs) #Name each matrix something useful, i.e., its interval name
list2env(l.Mtax, env = .GlobalEnv) #Export the matrix to the global environment
write.csv(res.matrix, "ResultsMatrix.csv", row.names = T) #For the entire Phanerozoic


#### MONTE CARLO SIMULATION OF RCL ####

iterations <- 1000

## Rcl (Extinctions) ##
Rcl.E.matrix <- matrix(NA,nrow=iterations,ncol=length(n.NALMAs))
colnames(Rcl.E.matrix)<-n.NALMAs

t1<-Sys.time()
for(i in seq_along(n.NALMAs)){ #for each interval

    n <- nrow(l.Mtax[[i]]) #Total number of rows and columns for the matrix
    N <- res.matrix[i,6] + res.matrix[i,7] #Number of extinctions in the focal bin (Nfl + Nb)
    ne <- (N^2) - N #Number of 1s to put in matrix
    
  for (iter in 1:iterations){ #for each iteration
    
    outTable <- matrix(0,n,n) #set up the matrix
    outTable[sample(n*n,ne)] <-1 #insert 1s (extinctions) into matrix in random locations
    R.cl <- cor.test(outTable, l.Mtax[[i]], method = "spearman")$estimate #calculate a null Rcl 
    ## CURRENT CONCERN - RCL VALUE CALCULATED IS ABOUT 1/100TH WHAT I EXPECT IT SHOULD BE ##
    Rcl.E.matrix[iter,i] <- R.cl #Insert calculated random value of clustering index into results matrix
    
  } #for each iteration
    
} #for each interval

t2<-Sys.time()
t2-t1

conf.levels <- matrix(NA, nrow = ncol(Rcl.E.matrix), ncol = 2) #Set up confidence interval matrix
for (i in 1:ncol(Rcl.E.matrix)){
  temp <- t.test(Rcl.E.matrix[,i], conf.level = 0.95) #Calculate 95% Confidence Intervals for Each Column
  conf.levels[i,] <- temp$conf.int
}
colnames(conf.levels) <- c("lcl.E","ucl.E") #lower confidence interval, upper confidence interval
res.matrix <- cbind(res.matrix,conf.levels) #bind the confidence intervals to the results matrix

## Rcl (Originations) ##
Rcl.O.matrix <- matrix(NA,nrow=iterations,ncol=length(n.NALMAs))
colnames(Rcl.O.matrix)<-n.NALMAs

t1<-Sys.time()
for(i in seq_along(n.NALMAs)){ #for each interval
  
  n <- nrow(l.Mtax[[i]]) #Total number of rows and columns for the matrix
  N <- res.matrix[i,6] + res.matrix[i,8] #Number of originations in the focal bin (Nfl + Nt)
  no <- (N^2) - N #Number of 1s to put in matrix

  for (iter in 1:iterations){ #for each iteration
    
    outTable <- matrix(0,n,n) #set up the matrix
    outTable[sample(n*n,no)] <-1 #insert 1s (originations) into matrix in random locations
    R.cl <- cor.test(outTable, l.Mtax[[i]], method = "spearman")$estimate #calculate a null Rcl
    Rcl.O.matrix[iter,i] <- R.cl #Insert calculated random value of clustering index into results matrix
    
  } #for each iteration
  
} #for each interval

t2<-Sys.time()
t2-t1

conf.levels <- matrix(NA, nrow = ncol(Rcl.O.matrix), ncol = 2) #Set up confidence interval matrix
for (i in 1:ncol(Rcl.O.matrix)){
  temp <- t.test(Rcl.O.matrix[,i], conf.level = 0.95) #Calculate 95% Confidence Intervals for Each Column
  conf.levels[i,] <- temp$conf.int
}
colnames(conf.levels) <- c("lcl.O","ucl.O") #lower confidence interval, upper confidence interval
res.matrix <- cbind(res.matrix,conf.levels) #bind the confidence intervals to the results matrix

#### PLOTTING ####
## Extinction Rate ##
ggplot(res.matrix, aes(x = Midpoint, y = q))+
  geom_point(color="black", pch = 19, size = 2)+
  geom_line(color = '#2980B9', lty = 2, lwd = 1)+
  geom_smooth(method = lm, color = "black")+
  ggtitle("Mammalian Extinction Rate through Cenozoic")+
  xlab("Time (Ma)")+
  ylab("Extinction Rate")+
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60), label = c(0,10,20,30,40,50,60))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
  ggsave("ExtinctionRate.png",width = 10, height = 5)

## Origination Rate ##
ggplot(res.matrix, aes(x = Midpoint, y = p))+
  geom_point(color="black", pch = 19, size = 2)+
  geom_line(color = '#B96229', lty = 2, lwd = 1)+
  geom_smooth(method = lm, color = "black")+
  ggtitle("Mammalian Origination Rate through Cenozoic")+
  xlab("Time (Ma)")+
  ylab("Origination Rate")+
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60), label = c(0,10,20,30,40,50,60))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
  ggsave("OriginationRate.png",width = 10, height = 5)

## Rcl Extinctions ##
ggplot(res.matrix, aes(x = Midpoint, y = Rcl.E))+
  geom_point(color="black", pch = 19, size = 2)+
  geom_line(color = '#2980B9', lty = 2, lwd = 1)+
  geom_ribbon(aes(ymin = lcl.E, ymax = ucl.E, color = "gray", fill = "gray"))+
  ggtitle("Mammalian Phylogenetic Clustering of Extinctions through Cenozoic")+
  xlab("Time (Ma)")+
  ylab("Rcl")+
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60), label = c(0,10,20,30,40,50,60))+
  guides(fill = FALSE, col = FALSE)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
  ggsave("Rcl.E.png",width = 10, height = 5)

## Rcl Originations ##
ggplot(res.matrix, aes(x = Midpoint, y = Rcl.O))+
  geom_point(color="black", pch = 19, size = 2)+
  geom_line(color = '#2980B9', lty = 2, lwd = 1)+
  geom_ribbon(aes(ymin = lcl, ymax = ucl, color = "gray", fill = "gray"))+
  ggtitle("Mammalian Phylogenetic Clustering of Originations through Cenozoic")+
  xlab("Time (Ma)")+
  ylab("Rcl")+
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60), label = c(0,10,20,30,40,50,60))+
  guides(fill = FALSE, col = FALSE)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
  ggsave("Rcl.O.png",width = 10, height = 5)

#### end ####

#### Rcl Calculation Test ####
x.clust <- matrix(c(rep(0,61),1,1,rep(0,6),1,0,1,rep(0,6),1,1,0),9,9) #the clustered example matrix
x.rando <- matrix(c(rep(0,4),1,rep(0,3),1,rep(0,27),1,rep(0,7),1,rep(0,27),1,rep(0,3),1,rep(0,4)),9,9) #the random example matrix
w <- matrix(c(0,1,1,rep(0,6),1,0,1,rep(0,6),1,1,rep(0,11),1,1,rep(0,6),1,0,1,rep(0,6),1,1,rep(0,11),1,1,rep(0,6),1,0,1,rep(0,6),1,1,0),9,9) #Mtax

normalize <- function(x){ #Function to normalize, needed for Rcl calculation
  (x - mean(x))
}

clust.e <- normalize (x.clust)
rando.e <- normalize (x.rando)

normalize <- function(x){ #Function to normalize, needed for Rcl calculation
  (x - mean(x))
}

Rcl.calc <- function(x,w){ #Function to Calculate relative clustering index; takes two matrices to calculate
  
  n <- nrow(x) #x is Mext or Morg
  So <- sum(w) #w is Mtax
  sigma <- c()
  sig.z <- c() #set up a vector 
  z <- normalize(x) #normalize Mext or Morg matrix
  
  for (i in 1:n){
    
    yi <- sum(z[i,]^2)
    sig.z <- c(sig.z, yi)
    
    for (j in 1:n){
      yj <- sum(z[i,]*z[,j]*w[i,j])
      sigma <- c(sigma, yj)
    }
    
    sum(sigma)
    
  }
  
  sum(sigma)/sum(sig.z) *(n/So) #compute Rcl
  
}

