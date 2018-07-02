
#Read in MTAX matrices
MtaxPuercan<-read.table("MtaxPuercan.txt",header=TRUE)
rownames(MtaxPuercan) <- colnames(MtaxPuercan)
diag(MtaxPuercan) <- 0 #set diagonals to 0
MtaxTorrejonian<-read.table("MtaxTorrejonian",header=TRUE)
rownames(MtaxTorrejonian) <- colnames(MtaxTorrejonian)
diag(MtaxTorrejonian) <- 0 #set diagonals to 0
MtaxTiffanian<-read.table("MtaxTiffanian",header=TRUE)
rownames(MtaxTiffanian) <- colnames(MtaxTiffanian)
diag(MtaxTiffanian) <- 0 #set diagonals to 0
MtaxClarkforkian<-read.table("MtaxClarkforkian",header=TRUE)
rownames(MtaxClarkforkian) <- colnames(MtaxClarkforkian)
diag(MtaxClarkforkian) <- 0 #set diagonals to 0
MtaxWasatchian<-read.table("MtaxWasatchian",header=TRUE)
rownames(MtaxWasatchian) <- colnames(MtaxWasatchian)
diag(MtaxWasatchian) <- 0 #set diagonals to 0
MtaxBridgerian<-read.table("MtaxBridgerian",header=TRUE)
rownames(MtaxBridgerian) <- colnames(MtaxBridgerian)
diag(MtaxBridgerian) <- 0 #set diagonals to 0
MtaxUintan<-read.table("MtaxUintan",header=TRUE)
rownames(MtaxUintan) <- colnames(MtaxUintan)
diag(MtaxUintan) <- 0 #set diagonals to 0
MtaxDuchesnean<-read.table("MtaxDuchesnean",header=TRUE)
rownames(MtaxDuchesnean) <- colnames(MtaxDuchesnean)
diag(MtaxDuchesnean) <- 0 #set diagonals to 0
MtaxChadronian<-read.table("MtaxChadronian",header=TRUE)
rownames(MtaxChadronian) <- colnames(MtaxChadronian)
diag(MtaxChadronian) <- 0 #set diagonals to 0
MtaxOrellan<-read.table("MtaxOrellan",header=TRUE)
rownames(MtaxOrellan) <- colnames(MtaxOrellan)
diag(MtaxOrellan) <- 0 #set diagonals to 0
MtaxWhitneyan<-read.table("MtaxWhitneyan",header=TRUE)
rownames(MtaxWhitneyan) <- colnames(MtaxWhitneyan)
diag(MtaxWhitneyan) <- 0 #set diagonals to 0
MtaxArikareean<-read.table("MtaxArikareean",header=TRUE)
rownames(MtaxArikareean) <- colnames(MtaxArikareean)
diag(MtaxArikareean) <- 0 #set diagonals to 0
MtaxHemingfordian<-read.table("MtaxHemingfordian",header=TRUE)
rownames(MtaxHemingfordian) <- colnames(MtaxHemingfordian)
diag(MtaxHemingfordian) <- 0 #set diagonals to 0
MtaxBarstovian<-read.table("MtaxBarstovian",header=TRUE)
rownames(MtaxBarstovian) <- colnames(MtaxBarstovian)
diag(MtaxBarstovian) <- 0 #set diagonals to 0
MtaxClarendonian<-read.table("MtaxClarendonian",header=TRUE)
rownames(MtaxClarendonian) <- colnames(MtaxClarendonian)
diag(MtaxClarendonian) <- 0 #set diagonals to 0
MtaxHemphillian<-read.table("MtaxHemphillian",header=TRUE)
rownames(MtaxHemphillian) <- colnames(MtaxHemphillian)
diag(MtaxHemphillian) <- 0 #set diagonals to 0
MtaxBlancan<-read.table("MtaxBlancan",header=TRUE)
rownames(MtaxBlancan) <- colnames(MtaxBlancan)
diag(MtaxBlancan) <- 0 #set diagonals to 0
MtaxIrvingtonian<-read.table("MtaxIrvingtonian",header=TRUE)
rownames(MtaxIrvingtonian) <- colnames(MtaxIrvingtonian)
diag(MtaxIrvingtonian) <- 0 #set diagonals to 0
MtaxRancholabrean<-read.table("MtaxRancholabrean",header=TRUE)
rownames(MtaxRancholabrean) <- colnames(MtaxRancholabrean)
diag(MtaxRancholabrean) <- 0 #set diagonals to 0
MtaxHolocene<-read.table("MtaxHolocene",header=TRUE)
rownames(MtaxHolocene) <- colnames(MtaxHolocene)
diag(MtaxHolocene) <- 0 #set diagonals to 0
Survivors<-read.csv("HoloceneExtinctions.csv",header=TRUE,row.names=2)
Survivors<-Survivors[which(Survivors$Status>0),]

#######################################################################
################### EXTINCTION RATE CALCULATIONS ######################
#Calculate Extinction Rate Via Boundary-Crossers Method, Foote 2000
NALMAs <- c("Puercan", "Torrejonian", "Tiffanian", "Clarkforkian",
            "Wasatchian", "Bridgerian", "Uintan", "Duchesnean",
            "Chadronian", "Orellan", "Whitneyan", "Arikareean",
            "Hemingfordian", "Barstovian", "Clarendonian", "Hemphillian",
            "Blancan","Irvingtonian", "Rancholabrean","Holocene","Recent")

l.df<-list(MtaxPuercan,MtaxTorrejonian,MtaxTiffanian,MtaxClarkforkian,
           MtaxWasatchian,MtaxBridgerian,MtaxUintan,MtaxDuchesnean,
           MtaxChadronian,MtaxOrellan,MtaxWhitneyan,MtaxArikareean,
           MtaxHemingfordian,MtaxBarstovian,MtaxClarendonian,MtaxHemphillian,
           MtaxBlancan,MtaxIrvingtonian,MtaxRancholabrean,MtaxHolocene,Survivors)

res.matrix <- matrix(NA, nrow = length(NALMAs), ncol=11)
rownames(res.matrix) <- NALMAs
colnames(res.matrix) <- c("Min Age", "Max Age", "Midpoint","Duration",
                          "Ni","Nfl","Nb","Nt","Nbt","p","q")
res.matrix[,1] <- c(63.3,61.7,56.8,55.8,50.3,46.2,40.4,37.2,33.9,33.3,30.8,20.43,15.97,13.6,10.3,4.9,1.8,0.3,0.012,0,0) #Interval minimum ages, following Veter et al., 2013
res.matrix[,2] <- c(65.5,63.3,61.7,56.8,55.8,50.3,46.2,40.4,37.2,33.9,33.3,30.8,20.43,15.97,13.6,10.3,4.9,1.8,0.3,0.012,0) #Interval maximum ages, following Veter et al., 2013
res.matrix[,3] <- ((res.matrix[,2]+res.matrix[,1])/2) #Interval Midpoints
res.matrix[,4] <- res.matrix[,2] - res.matrix[,1] #Duration of Interval

# Write a for loop that steps through each interval and calculates extinction/origination rate
for (i in seq_along(l.df)){ #For each focal interval
  
  N <- row.names(l.df[[i]]) #Generate a list of all taxa occurring in focal interval
  res.matrix[i,5] <- length(N) #Insert that number of taxa into the results matrix
  
  if((i >= 2)&(i <= 20)){ #For all intervals except the first (oldest) and last (youngest) ones
    
    N.previous <- row.names(l.df[[i-1]]) #What taxa occurred in the previous interval?
    N.next <- row.names(l.df[[i+1]]) #What taxa occur in the next interval?
    
    Nfl <- setdiff(N, union(N.previous, N.next)) #Identify those taxa that are singletons; i.e., occur in this interval only
    res.matrix[i,6] <- length(Nfl) #Insert that number into the results matrix
    
    Nbt <- Reduce(intersect, list(N.previous, N, N.next)) #Identify those taxa that cross both the bottom and top of the interval
    res.matrix[i,9] <- length(Nbt) #Insert that number into the results matrix
    
    Nt <- setdiff(intersect(N,N.next), Nbt) #Identify those taxa that cross the top of the interval; i.e., exist both in this interval and the next one
    res.matrix[i,8] <- length(Nt) #Insert that number into the results matrix
    
    Nb <- setdiff(intersect(N,N.previous), Nbt) #Identify those taxa that cross the bottom of the interval; i.e., exist both in this interval and the previous one
    res.matrix[i,7] <- length(Nb) #Insert that number into the results matrix
    
    p <- round(-log((length(Nbt)+length(Nfl))/length(Nbt)), digits=3) #Calculate origination rate (p) on a per-interval basis (i.e., time-independent) 
    res.matrix[i,10] <- p #Insert origination rate into the results matrix
    
    q <- round(-log((length(Nbt)+length(Nb))/length(Nbt)), digits=3) #Calculate extinction rate (q) on a per-interval basis (i.e., time-independent) 
    res.matrix[i,11] <- q #Insert extinction rate into the results matrix
  }
}

res.matrix <- as.data.frame(res.matrix)

##Plotting
ggplot(res.matrix, aes(x = Midpoint, y = q))+
  geom_point(color="black", pch = 19, size = 2)+
  geom_line(color = '#2980B9', lty = 2, lwd = 1)+
  geom_smooth(method = lm, color = "black")+
  ggtitle("Mammalian Extinction Rate through Cenozoic")+
  xlab("Time (Ma)")+
  ylab("Extinction Rate")+
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60), label = c(0,10,20,30,40,50,60))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  ggsave("ExtinctionRate.png",width = 10, height = 5)

##Plotting
ggplot(res.matrix, aes(x = Midpoint, y = p))+
  geom_point(color="black", pch = 19, size = 2)+
  geom_line(color = '#B96229', lty = 2, lwd = 1)+
  geom_smooth(method = lm, color = "black")+
  ggtitle("Mammalian Origination Rate through Cenozoic")+
  xlab("Time (Ma)")+
  ylab("Origination Rate")+
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60), label = c(0,10,20,30,40,50,60))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  ggsave("OriginationRate.png",width = 10, height = 5)
