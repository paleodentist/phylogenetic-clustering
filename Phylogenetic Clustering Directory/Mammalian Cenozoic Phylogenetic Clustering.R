###### PHANEROZOIC MAMMALIAN PHYLOGENETIC CLUSTERING R CODE ######
## Written by GREGORY J. SMITH ##

rm(list=ls())

library(ggplot2) #plotting
library(dplyr) #data wrangling
library(plyr) #for the histogram of extinctions
library(stats) #to calculate the correlation coefficient - Rcl
library(tibble) #for "rownames_to_column function
library(paleobioDB) #to read in fossil occurrence data from pbdb

setwd("C:/Users/Greg/Dropbox/Research/Projects/PhD Dissertation Chapters/Chapter 4 - Mammalian Cenozoic Phylogenetic Clustering/Phylogenetic Clustering Directory")

data <- read.csv('pbdb_mammalia_cenozoic_genus.csv',skip=21, stringsAsFactors = FALSE) #Read in pbdb mammalian data (genus resolution, NOA, 70-0 Ma)
data <- filter(data, !order %in% c("Cetacea","Sirenia","Desmostylia",""), !genus == "", !family == "") #Remove aquatic genera and those without genus ID
data <- filter(data, lat <= 55) #Only keep occurrences below 55 deg latitude
data <- as.data.frame(data)

#### USE OCCURRENCE DATA TO FIND FAD, LAD AND DURATIONS FOR ALL TAXA ####
taxa<-data$genus #What are all occurrences appearing in this Period?
taxa.names<-unique(taxa) #What are the specific genus names appearing in this Period?
num.taxa<-length(taxa.names) #How many genera are there? (will be number of rows in the results matrix)
outTable<-matrix(NA,nrow=num.taxa,ncol=5)
colnames(outTable)<-c("FAD","LAD","Duration","Family", "Order")
rownames(outTable)<-taxa.names

t1<-Sys.time()
for(i in 1:nrow(data)){
  
  this.taxon <- data$genus[i] #Define the taxa
  all.occ.this.taxon <- filter(data, genus == this.taxon) #Select all rows of that taxon
  FAD <- max(all.occ.this.taxon$max_ma, na.rm = T) #Find the max value (FAD) of that taxon...
  outTable[this.taxon,1] <- FAD #...and insert it into column 1 (FAD)
  LAD <- min(all.occ.this.taxon$min_ma, na.rm = T) #Find the min value (LAD) of that taxon...
  outTable[this.taxon,2] <- LAD #...and insert it into column 2
  longevity <- FAD - LAD #Calulate the species longevity...
  outTable[this.taxon,3] <- longevity #...and insert it into column 3
  Family <- all.occ.this.taxon$family[1] #Find the family that genus belongs to (the first one in case two are assigned)
  outTable[this.taxon,4] <- Family #...and insert the Family name into column 4
  Order <- all.occ.this.taxon$order[1] #Find the order that genus belongs to (the first one in case two are assigned)
  outTable[this.taxon,5] <- Order #...and insert the Order name into column 5
  outTable<-outTable
}
df <- as.data.frame(outTable)

t2<-Sys.time()
t2-t1
write.csv(outTable, "Mammalia_longevities.csv", row.names = T) #Save result

#### FUNCTIONS TO CALCULATE RCL AND MORAN'S I ####
Rcl.calc <- function(x,w){ #Function to Calculate relative clustering index; takes two matrices to calculate
  
  lt <- lower.tri(x) #lower triangle (avoiding repeats of a symmetric matrix, pulls out diagonals too)
  rr <- cor(x[lt],w[lt]) #correlation bewteen two matrices; x is Mext/Morg, w is Mtax
  return(rr)
}

moranI<- function(z, w)
{
  # normalize (mean center) z
  zn <- z-mean(z)
  cp <- sum(zn %*% t(zn) * w)  #numerator term of I
  
  w.sum <- sum(w)		   #sum of Mtax (w)
  vi <- length(zn)/sum(zn^2) # variance term
  morI <- cp*vi/w.sum
  
  return(morI)
}

#### BIN BY NALMA AND CALCULATE EXTINCTION AND ORIGINATION RATES AND RCL VALUES ####
df <- read.csv("Mammalia_longevities.csv",header=TRUE,row.names=1,na.strings=c("","NA")) #read dataset back in, remove blank cells
df <- df[Reduce(`&`, lapply(df, function(x) !is.na(x)  & is.finite(x))),] #remove infinity values

n.NALMAs<-c("Lancian","Puercan","Torrejonian","Tiffanian","Clarkforkian", #Names of the NALMAs
            "Wasatchian","Bridgerian","Uintan","Duchesnean",
            "Chadronian","Orellan","Whitneyan","Arikareean",
            "Hemingfordian","Barstovian","Clarendonian","Hemphillian",
            "Blancan","Irvingtonian","Rancholabrean","Recent")


l.Mtax <- vector("list", length(n.NALMAs)) #Intialize a list for Mtax to iterate over
l.Morig <- vector("list", length(n.NALMAs)) #Initialize a list for Morig to iterate over
l.Mext <- vector("list", length(n.NALMAs)) #Initialize a list for Mext to iterate over
l.taxa <- vector("list", length(n.NALMAs)) #List of taxa existing in each NALMA
l.extinct <- vector("list", length(n.NALMAs)) #List of extinct taxa in each NALMA
l.orig <- vector("list", length(n.NALMAs)) #List of originating taxa in each NALMA

res.matrix <- matrix(NA, nrow = length(n.NALMAs), ncol=15) #Set up a results matrix for extinction rate calculation
rownames(res.matrix) <- n.NALMAs
colnames(res.matrix) <- c("MinAge", "MaxAge", "Midpoint","Duration",
                          "Ntot","Nt","Nb","Ne","No",
                          "p","q","Rcl.E","Rcl.O","I.E","I.O")

res.matrix[,1] <- c(66,63.3,61.7,56.8,55.8,50.3,46.2,40.4,37.2,33.9,33.3,30.8,20.43,15.97,13.6,10.3,4.9,1.8,0.3,0.011,0) #Interval minimum ages, following Woodburne, 2004 (including Lancian)
res.matrix[,2] <- c(68,66,63.3,61.7,56.8,55.8,50.3,46.2,40.4,37.2,33.9,33.3,30.8,20.43,15.97,13.6,10.3,4.9,1.8,0.3,0.011) #Interval maximum ages, following Woodburne, 2004 (including Lancian)
res.matrix[,3] <- ((res.matrix[,2]+res.matrix[,1])/2) #Interval Midpoints
res.matrix[,4] <- res.matrix[,2] - res.matrix[,1] #Duration of Interval

t1<-Sys.time()
for(i in seq_along(n.NALMAs)){
  
  singleton <- df %>% rownames_to_column('genus') %>% #NFL = taxa that are singletons; i.e., occur in this interval only
    filter(LAD >= res.matrix[i,1] & FAD <= res.matrix[i,2]) %>% 
    column_to_rownames('genus') #Identify those taxa
  NFL <- nrow(singleton)
  
  bottom <- df %>% rownames_to_column('genus') %>% #NbL = taxa that cross the bottom boundary and make their last appearance during the interval
    filter(LAD >= res.matrix[i,1] & LAD < res.matrix[i,2] & FAD >= res.matrix[i,2]) %>% 
    column_to_rownames('genus') #Identify those taxa 
  NbL <- nrow(bottom)
  
  top <- df %>% rownames_to_column('genus') %>% #NFt = taxa that make their first appearance during the interval and cross the top boundary
    filter(LAD <= res.matrix[i,1] & FAD <= res.matrix[i,2] & FAD > res.matrix[i,1]) %>% 
    column_to_rownames('genus') #Identify those taxa
  NFt <- nrow(top)
  
  bottomtop <- df %>% rownames_to_column('genus') %>% #Nbt = taxa that range through the entire interval, crossing both the top and bottom boundaries
    filter(LAD < res.matrix[i,1] & FAD > res.matrix[i,2]) %>% 
    column_to_rownames('genus') #Identify those taxa
  Nbt <- nrow(bottomtop)
  
  Ntaxa <- rbind(singleton, bottom, top, bottomtop) #List of all taxa occurring in focal interval
  l.taxa[[i]] <- Ntaxa
  Ntot <- NFL + NbL + NFt + Nbt #total diversity
  res.matrix[i,5] <- Ntot #insert into results matrix
  
  Nt <- NFt + Nbt #top-boundary crossers
  res.matrix[i,6] <- Nt #insert into results matrix
  
  Nb <- NbL + Nbt #bottom-boundary crossers
  res.matrix[i,7] <- Nb #insert into results matrix
  
  extinct.taxa <- rbind(singleton, bottom) #identify extinct taxa
  l.extinct[[i]] <- extinct.taxa #insert into list
  Ne <- NFL + NbL #number of extinctions
  res.matrix[i,8] <- Ne #insert into results matrix
  
  origin.taxa <- rbind(singleton, top) #identify originating taxa
  l.orig[[i]] <- origin.taxa #insert into list
  No <- NFL + NFt #number of originations
  res.matrix[i,9] <- No #insert into results matrix
  
  p <- round((-log(Nbt/Nt)/res.matrix[i,4]), digits=3) #Calculate origination rate (p) on a per-interval basis (i.e., time-independent) 
  res.matrix[i,10] <- p #Insert origination rate into the results matrix
  
  q <- round((-log(Nbt/Nb)/res.matrix[i,4]), digits=3) #Calculate extinction rate (q) on a per-interval basis (i.e., time-independent) 
  res.matrix[i,11] <- q #Insert extinction rate into the results matrix
  
  l.Mtax[[i]] <- crossprod(table(Ntaxa$Family,row.names(Ntaxa))) #Muliply family by genera to create an n x n matrix, with any numeric values that exist being assinged to those genera that belong to the same families
  diag( l.Mtax[[i]]) <- 0 #Assign a 0 to the diagonals 
  l.Mtax[[i]][l.Mtax[[i]] > 0] <- 1 #Assign a 1 to anything that returned a value (will only be those genera belonging to same families); keep 0 otherwise
  
  l.Morig[[i]] <- array(0,dim=c(nrow(Ntaxa),nrow(Ntaxa))) #create the origination matrix, assign a default value of 0 (no shared origination)
  rownames(l.Morig[[i]]) <- row.names(Ntaxa) #taxa names make the rows
  colnames(l.Morig[[i]]) <- row.names(Ntaxa) #taxa names make the columns
  
  l.Mext[[i]] <- array(0,dim=c(nrow(Ntaxa),nrow(Ntaxa))) #create the extinction matrix, assign a default value of 0 (no shared extinction)
  rownames(l.Mext[[i]]) <- row.names(Ntaxa) #taxa names make the rows
  colnames(l.Mext[[i]]) <- row.names(Ntaxa) #taxa names make the columns
  
  l.Morig[[i]][row.names(origin.taxa), row.names(origin.taxa)] <-1 #add these members to their position in the array made above, give them a value of 1
  l.Mext[[i]][row.names(extinct.taxa), row.names(extinct.taxa)] <-1 #add these members to their position in the array made above, give them a value of 1
  
  Rcl.E <- Rcl.calc(l.Mext[[i]], l.Mtax[[i]])
  res.matrix[i,12] <- Rcl.E
  
  Rcl.O <- Rcl.calc(l.Morig[[i]], l.Mtax[[i]])
  res.matrix[i,13] <- Rcl.O
  
  z.e <- rep(0, times = Ntot)
  z.e[which(row.names(Ntaxa) %in% row.names(extinct.taxa))] <- 1
  I.E <- moranI(z.e, l.Mtax[[i]])
  res.matrix[i,14] <- I.E
  
  z.o <- rep(0, times = Ntot)
  z.o[which(row.names(Ntaxa) %in% row.names(origin.taxa))] <- 1
  I.O <- moranI(z.o, l.Mtax[[i]])
  res.matrix[i,15] <- I.O
  
} #for each NALMA

t2<-Sys.time()
t2-t1


res.matrix <- as.data.frame(res.matrix) #Save result as a data.frame
names(l.Morig) <- paste0("Morig",n.NALMAs) #Name each matrix something useful, i.e., its interval name
list2env(l.Morig, env = .GlobalEnv) #Export the matrix to the global environment
names(l.Mext) <- paste0("Mext",n.NALMAs) #Name each matrix something useful, i.e., its interval name
list2env(l.Mext, env = .GlobalEnv) #Export the matrix to the global environment
names(l.Mtax) <- paste0("Mtax",n.NALMAs) #Name each matrix something useful, i.e., its interval name
list2env(l.Mtax, env = .GlobalEnv) #Export the matrix to the global environment

write.csv(res.matrix, "ResultsMatrix.csv", row.names = T) #Save result (Table 1)

#### MONTE CARLO SIMULATION OF RCL ####

iterations <- 5000

## Rcl (Extinctions) ##
Rcl.E.matrix <- matrix(NA,nrow=iterations,ncol=length(n.NALMAs))
colnames(Rcl.E.matrix)<-n.NALMAs

t1<-Sys.time()
for(i in seq_along(n.NALMAs)){ #for each interval

  n <- res.matrix[i,5] #Total number of rows and columns for the matrix (number of taxa)
  ne <- res.matrix[i,8] #Number of extinctions in the focal time period
  
  for (iter in 1:iterations){ #for each iteration
    
    z.e <- rep(0,times=n) #Set up a vector of length n, default 0s
    z.e[sample(n,ne)]<-1  #Insert ne 1s in random locations (random extinctions)
    r.Mext <- z.e %o% z.e  #Make the random extinctions matrix 
    Rcl.E <- Rcl.calc(r.Mext, l.Mtax[[i]]) #calculate a null extinction Rcl 
    Rcl.E.matrix[iter,i] <- Rcl.E #Insert calculated random value of clustering index into results matrix
    
  } #for each iteration
    
} #for each interval

#Confidence levels for Rcl.E Calculation
conf.levels <- matrix(NA, nrow = ncol(Rcl.E.matrix), ncol = 2) #Set up confidence interval matrix
for (i in 1:ncol(Rcl.E.matrix)){
  temp <- quantile(Rcl.E.matrix[,i], c(0.025,0.975), na.rm=T) #Calculate 95% Confidence Intervals for Each Column
  conf.levels[i,] <- temp #Insert value into appropriate row
}
colnames(conf.levels) <- c("lcl.Rcl.E","ucl.Rcl.E") #lower confidence interval, upper confidence interval
res.matrix <- cbind(res.matrix,conf.levels) #bind the confidence intervals to the results matrix


## Rcl (Originations) ##
Rcl.O.matrix <- matrix(NA,nrow=iterations,ncol=length(n.NALMAs))
colnames(Rcl.O.matrix)<-n.NALMAs

for(i in seq_along(n.NALMAs)){ #for each interval
  
  n <- res.matrix[i,5] #Total number of rows and columns for the matrix (number of taxa)
  no <- res.matrix[i,9] #Number of originations in the focal time period
  
  for (iter in 1:iterations){ #for each iteration
    
    z.o <- rep(0,times=n) #Set up a vector of length n, default 0s
    z.o[sample(n,no)]<-1  #Insert no 1s in random locations (random originations)
    r.Morig <- z.o %o% z.o  #Make the random originations matrix
    Rcl.O <- Rcl.calc(r.Morig, l.Mtax[[i]]) #calculate a null origination Rcl 
    Rcl.O.matrix[iter,i] <- Rcl.O #Insert calculated random value of clustering index into results matrix
    
  } #for each iteration
  
} #for each interval

#Confidence levels for Rcl.O Calculation
conf.levels <- matrix(NA, nrow = ncol(Rcl.O.matrix), ncol = 2) #Set up confidence interval matrix
for (i in 1:ncol(Rcl.O.matrix)){
  temp <- quantile(Rcl.O.matrix[,i], c(0.025,0.975), na.rm=T) #Calculate 95% Confidence Intervals for Each Column
  conf.levels[i,] <- temp #Insert value into appropriate row
}
colnames(conf.levels) <- c("lcl.Rcl.O","ucl.Rcl.O") #lower confidence interval, upper confidence interval
res.matrix <- cbind(res.matrix,conf.levels) #bind the confidence intervals to the results matrix


## Moran's I (Extinctions) ##
I.E.matrix <- matrix(NA,nrow=iterations,ncol=length(n.NALMAs))
colnames(I.E.matrix)<-n.NALMAs

for(i in seq_along(n.NALMAs)){ #for each interval
  
  n <- res.matrix[i,5] #Total number of rows and columns for the matrix (number of taxa)
  ne <- res.matrix[i,8] #Number of extinctions in the focal time period
  
  for (iter in 1:iterations){ #for each iteration
    
    z.e <- rep(0,times=n) #Set up a vector of length n, default 0s
    z.e[sample(n,ne)]<-1  #Insert ne 1s in random locations (random extinctions)
    I.E <- moranI(z.e, l.Mtax[[i]]) #calculate a null extinction I 
    I.E.matrix[iter,i] <- I.E #Insert calculated random value of clustering index into results matrix
    
  } #for each iteration
  
} #for each interval

#Confidence levels for I.E Calculation
conf.levels <- matrix(NA, nrow = ncol(I.E.matrix), ncol = 2) #Set up confidence interval matrix
for (i in 1:ncol(I.E.matrix)){
  temp <- quantile(I.E.matrix[,i], c(0.025,0.975), na.rm=T) #Calculate 95% Confidence Intervals for Each Column
  conf.levels[i,] <- temp #Insert value into appropriate row
}
colnames(conf.levels) <- c("lcl.I.E","ucl.I.E") #lower confidence interval, upper confidence interval
res.matrix <- cbind(res.matrix,conf.levels) #bind the confidence intervals to the results matrix


## Moran's I (Originations) ##
I.O.matrix <- matrix(NA,nrow=iterations,ncol=length(n.NALMAs))
colnames(I.O.matrix)<-n.NALMAs

for(i in seq_along(n.NALMAs)){ #for each interval
  
  n <- res.matrix[i,5] #Total number of rows and columns for the matrix (number of taxa)
  no <- res.matrix[i,9] #Number of originations in the focal time period
  
  for (iter in 1:iterations){ #for each iteration
    
    z.o <- rep(0,times=n) #Set up a vector of length n, default 0s
    z.o[sample(n,no)]<-1  #Insert ne 1s in random locations (random originations)
    I.O <- moranI(z.o, l.Mtax[[i]]) #calculate a null origination Rcl 
    I.O.matrix[iter,i] <- I.O #Insert calculated random value of clustering index into results matrix
    
  } #for each iteration
  
} #for each interval

#Confidence levels for I.O Calculation
conf.levels <- matrix(NA, nrow = ncol(I.O.matrix), ncol = 2) #Set up confidence interval matrix
for (i in 1:ncol(I.O.matrix)){
  temp <- quantile(I.O.matrix[,i], c(0.025,0.975), na.rm=T) #Calculate 95% Confidence Intervals for Each Column
  conf.levels[i,] <- temp #Insert value into appropriate row
}
colnames(conf.levels) <- c("lcl.I.O","ucl.I.O") #lower confidence interval, upper confidence interval
res.matrix <- cbind(res.matrix,conf.levels) #bind the confidence intervals to the results matrix


t2<-Sys.time()
t2-t1

write.csv(res.matrix, "ResultsMatrixConfidenceLimits.csv", row.names = T) #Save result

#### FIGURE 1 - Rates and Clustering ####
res.matrix <- read.csv("ResultsMatrixConfidenceLimits.csv",header=TRUE) #Full res matrix for plotting

## Extinction Rate ##
ggplot(res.matrix %>% slice(1:20), aes(x = MinAge, y = q))+ #remove Holocene from plot
  geom_smooth(method = lm, color = "grey40")+
  geom_point(color="black", pch = 19, size = 3)+
  geom_line(color = 'black', lty = 1, lwd = 1)+
  ggtitle("A. Extinction Rate")+
  xlab("Time (Ma)")+
  ylab("Extinction Rate")+
  scale_x_reverse(breaks = c(70,60,50,40,30,20,10,0), label = c(70,60,50,40,30,20,10,0), limits = c(70,0))+
  scale_y_continuous(breaks = c(0,0.5,1.0,1.5,2.0), label = c(0,0.5,1.0,1.5,2.0), limits = c(-0.2,2.0))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_classic()
  ggsave("ExtinctionRate.pbdb.pdf",width = 3.5, height = 2.5)

## Origination Rate ##
ggplot(res.matrix %>% slice(1:20), aes(x = MaxAge, y = p))+ #remove Holocene from plot
  geom_smooth(method = lm, color = "grey40")+
  geom_point(color="black", pch = 19, size = 3)+
  geom_line(color = 'black', lty = 1, lwd = 1)+
  ggtitle("B. Origination Rate")+
  xlab("Time (Ma)")+
  ylab("Origination Rate")+
  scale_x_reverse(breaks = c(70,60,50,40,30,20,10,0), label = c(70,60,50,40,30,20,10,0), limits = c(70,0))+
  scale_y_continuous(breaks = c(0,0.4,0.8,1.2), label = c(0,0.4,0.8,1.2), limits = c(-0.1,1.2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_classic()
  ggsave("OriginationRate.pbdb.pdf",width = 3.5, height = 2.5)

## Rcl Extinctions ##
ggplot(res.matrix %>% slice(1:20), aes(x = MinAge, y = Rcl.E))+ #remove Holocene from plot
  geom_ribbon(aes(ymin = lcl.Rcl.E, ymax = ucl.Rcl.E),  fill = "grey80")+
  geom_point(color="black", pch = 19, size = 3)+
  geom_line(color = 'black', lty = 1, lwd = 1)+
  geom_hline(yintercept = 0, lty = 1, lwd = 0.5)+
  ggtitle("C. Extinction Clustering (Rcl)")+
  xlab("Time (Ma)")+
  ylab("Rcl")+
  scale_x_reverse(breaks = c(70,60,50,40,30,20,10,0), label = c(70,60,50,40,30,20,10,0), limits = c(70,0))+
  scale_y_continuous(breaks = c(-0.15,0,0.15), label = c(-0.15,0,0.15), limits = c(-0.2,0.2))+ #for pbdb
  guides(fill = FALSE, col = FALSE)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_classic()
  ggsave("RclE.pbdb.pdf",width = 3.5, height = 2.5)  

## Rcl Originations ##
ggplot(res.matrix %>% slice(1:20), aes(x = MaxAge, y = Rcl.O))+ #remove Holocene from plot
  geom_ribbon(aes(ymin = lcl.Rcl.O, ymax = ucl.Rcl.O),  fill = "grey80")+
  geom_point(color="black", pch = 19, size = 3)+
  geom_line(color = 'black', lty = 1, lwd = 1)+
  geom_hline(yintercept = 0, lty = 1, lwd = 0.5)+
  ggtitle("D. Origination Clustering (Rcl)")+
  xlab("Time (Ma)")+
  ylab("Rcl")+
  scale_x_reverse(breaks = c(70,60,50,40,30,20,10,0), label = c(70,60,50,40,30,20,10,0), limits = c(70,0))+
  scale_y_continuous(breaks = c(-0.15,0,0.15), label = c(-0.15,0,0.15), limits = c(-0.2,0.2))+ #for pbdb
  guides(fill = FALSE, col = FALSE)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_classic()
  ggsave("RclO.pbdb.pdf",width = 3.5, height = 2.5)

## Moran's I Extinctions ##
ggplot(res.matrix %>% slice(1:20), aes(x = MinAge, y = I.E))+ #remove Holocene from plot
  geom_ribbon(aes(ymin = lcl.I.E, ymax = ucl.I.E),  fill = "grey80")+
  geom_point(color="black", pch = 19, size = 3)+
  geom_line(color = 'black', lty = 1, lwd = 1)+
  geom_hline(yintercept = 0, lty = 1, lwd = 0.5)+
  ggtitle("E. Extinction Clustering (Moran's I)")+
  xlab("Time (Ma)")+
  ylab("Moran's I")+
  scale_x_reverse(breaks = c(70,60,50,40,30,20,10,0), label = c(70,60,50,40,30,20,10,0), limits = c(70,0))+
  scale_y_continuous(breaks = c(seq(-0.3,0.3,0.3)), label = c(seq(-0.3,0.3,0.3)), limits = c(-0.4,0.4))+ #for pbdb
  guides(fill = FALSE, col = FALSE)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_classic()
  ggsave("IE.pbdb.pdf",width = 3.5, height = 2.5)  

 ## Moran's I Originations ##
ggplot(res.matrix %>% slice(1:20), aes(x = MaxAge, y = I.O))+ #remove Holocene from plot
  geom_ribbon(aes(ymin = lcl.I.O, ymax = ucl.I.O),  fill = "grey80")+
  geom_point(color="black", pch = 19, size = 3)+
  geom_line(color = 'black', lty = 1, lwd = 1)+
  geom_hline(yintercept = 0, lty = 1, lwd = 0.5)+
  ggtitle("F. Origination Clustering (Moran's I)")+
  xlab("Time (Ma)")+
  ylab("Moran's I")+
  scale_x_reverse(breaks = c(70,60,50,40,30,20,10,0), label = c(70,60,50,40,30,20,10,0), limits = c(70,0))+
  scale_y_continuous(breaks = c(seq(-0.3,0.3,0.3)), label = c(seq(-0.3,0.3,0.3)), limits = c(-0.4,0.4))+ #for pbdb
  guides(fill = FALSE, col = FALSE)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme_classic()
  ggsave("IO.pbdb.pdf",width = 3.5, height = 2.5)

#### FIGURE 2 - Histogram of Group Membership ####
extinct.matrix <- matrix(0, nrow = length(n.NALMAs), ncol=length(unique(df$Order))) #Set up a results matrix for numbers of extinct orders by NALMA
colnames(extinct.matrix) <- levels(df$Order)

for(i in seq_along(n.NALMAs)){
 
  n.extinct <- summary(l.extinct[[i]]$Order)
  extinct.matrix[i,] <- n.extinct
}
extinct.matrix <- as.data.frame(extinct.matrix) #Save result as a data.frame
extinct.matrix <- extinct.matrix %>% slice(1:20) #Remove Holocene
rownames(extinct.matrix) <- head(n.NALMAs, -1)

ggplot(extinct.matrix, aes(row.names(extinct.matrix), fill = colnames(extinct.matrix)))+
  geom_histogram()
  
  
  geom_histogram(aes(x = row.names(extinct.matrix), y = rowSums(extinct.matrix)), fill = "blue", stat = "identity")+
  geom_histogram(aes(x = row.names(extinct.matrix), y = extinct.matrix$Carnivora), fill = "deeppink", stat = "identity")+
  geom_histogram(aes(x = row.names(extinct.matrix), y = extinct.matrix$Artiodactyla), fill = "green", stat = "identity")+
  geom_histogram(aes(x = row.names(extinct.matrix), y = extinct.matrix$Perissodactyla), fill = "red", stat = "identity")+
  geom_histogram(aes(x = row.names(extinct.matrix), y = extinct.matrix$Proboscidea), fill = "pink", stat = "identity")+
  geom_histogram(aes(x = row.names(extinct.matrix), y = extinct.matrix$Rodentia), fill = "orange", stat = "identity")+
  geom_histogram(aes(x = row.names(extinct.matrix), y = extinct.matrix$Multituberculata), fill = "yellow", stat = "identity")+
  geom_histogram(aes(x = row.names(extinct.matrix), y = extinct.matrix$Cimolesta), fill = "brown", stat = "identity")+
  geom_histogram(aes(x = row.names(extinct.matrix), y = extinct.matrix$Creodonta), fill = "black", stat = "identity")

#### STATISICAL COMPARISONS ####
 ## Correlation between Rcl.E and Diversity/Turnover Metrics ##
 cor.test(res.matrix$Ntot, res.matrix$Rcl.E, method = "spearman") #rho = -0.398, p = 0.092    
 cor.test(res.matrix$Ne, res.matrix$Rcl.E, method = "spearman") #rho = 0.084, p = 0.734
 cor.test((res.matrix$Ne/res.matrix$Ntot), res.matrix$Rcl.E, method = "spearman") #rho = 0.409, p = 0.083
 cor.test(res.matrix$q, res.matrix$Rcl.E, method = "spearman") #rho = 0.354, p = 0.137
 
 ## Correlation between Rcl.O and Diversity/Turnover Metrics ##
 cor.test(res.matrix$Ntot, res.matrix$Rcl.O, method = "spearman") #rho = 0.113, p = 0.635    
 cor.test(res.matrix$No, res.matrix$Rcl.O, method = "spearman") #rho = -0.311, p = 0.183
 cor.test((res.matrix$No/res.matrix$Ntot), res.matrix$Rcl.O, method = "spearman") #rho = -0.698, p = 0.001
 cor.test(res.matrix$p, res.matrix$Rcl.O, method = "spearman") #rho = -0.313, p = 0.179
 
 ## Correlation between I.E and Diversity/Turnover Metrics ##
 cor.test(res.matrix$Ntot, res.matrix$I.E, method = "spearman") #rho = -0.486, p = 0.037    
 cor.test(res.matrix$Ne, res.matrix$I.E, method = "spearman") #rho = 0.040, p = 0.872
 cor.test((res.matrix$Ne/res.matrix$Ntot), res.matrix$I.E, method = "spearman") #rho = 0.402, p = 0.089
 cor.test(res.matrix$q, res.matrix$I.E, method = "spearman") #rho = 0.462, p = 0.047
 
 ## Correlation between I.O and Diversity/Turnover Metrics ##
 cor.test(res.matrix$Ntot, res.matrix$I.O, method = "spearman") #rho = -0.096, p = 0.686    
 cor.test(res.matrix$No, res.matrix$I.O, method = "spearman") #rho = -0.290, p = 0.214
 cor.test((res.matrix$No/res.matrix$Ntot), res.matrix$I.O, method = "spearman") #rho = -0.627, p = 0.004
 cor.test(res.matrix$p, res.matrix$I.O, method = "spearman") #rho = -0.663, p = 0.781
 
#### end ####