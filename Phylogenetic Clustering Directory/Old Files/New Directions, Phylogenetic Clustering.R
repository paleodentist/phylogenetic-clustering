rm(list=ls())
data<-read.csv('RODENT_GENERA.csv',header=TRUE)
library(maps)
library(vegan)
library(compare)

####Bin by NALMA		
Holocene<-data[which(data$ma_max<=0.012),]
Rancholabrean<- data[which((data$ma_max<=0.3)&(data$ma_min>0.012)),]
Rancholabrean.1<-data[which((data$ma_max<=0.3)&(data$ma_max>0.012)&(data$ma_min>=0)),]
Rancholabrean<-rbind(Rancholabrean,Rancholabrean.1)

Irvingtonian2<-data[which((data$ma_max<=0.85)&(data$ma_min>0.3)),]
Irvingtonian2.1<-data[which((data$ma_max<=0.85)&(data$ma_max>0.3)&(data$ma_min>0.012)),]
Irvingtonian1<-data[which((data$ma_max<=1.72)&(data$ma_min>0.85)),]
Irvingtonian1.1<-data[which((data$ma_max<=1.72)&(data$ma_max>0.85)&(data$ma_min>0.3)),]

LateBlancan<-data[which((data$ma_max<=2.6)&(data$ma_min>1.6)),]
LateBlancan.1<-data[which((data$ma_max<=2.6)&(data$ma_max>1.6)&(data$ma_min>0.85)),]
EarlyBlancan<-data[which((data$ma_max<=4.75)&(data$ma_min>2.6)),]
EarlyBlancan.1<-data[which((data$ma_max<=4.75)&(data$ma_max>2.6)&(data$ma_min>1.6)),]

LateHemphillian<-data[which((data$ma_max<=6.8)&(data$ma_min >4.75)),]
LateHemphillian.1<-data[which((data$ma_max<=6.8)&(data$ma_max>4.75)&(data$ma_min>2.6)),] 
EarlyHemphillian<-data[which((data$ma_max<=9)&(data$ma_min >6.8)),]
EarlyHemphillian.1<-data[which((data$ma_max<=9)&(data$ma_max>6.8)&(data$ma_min>4.75)),] 

Clarendonian3<-data[which((data$ma_max<=10)&(data$ma_min>9)),]
Clarendonian3.1<-data[which((data$ma_max<=10)&(data$ma_max>9)&(data$ma_min>6.8)),]
Clarendonian2<-data[which((data$ma_max<=12)&(data$ma_min>10)),]
Clarendonian2.1<-data[which((data$ma_max<=12)&(data$ma_max>10)&(data$ma_min>9)),]
Clarendonian1<-data[which((data$ma_max<=12.5)&(data$ma_min>12)),]
Clarendonian1.1<-data[which((data$ma_max<=12.5)&(data$ma_max>12)&(data$ma_min>10)),]

Barstovian2<-data[which((data$ma_max<=14.8)&(data$ma_min >12.5)),]
Barstovian2.1<-data[which((data$ma_max<=14.8)&(data$ma_max>12.5)&(data$ma_min>12)),]
Barstovian1<-data[which((data$ma_max<=15.9)&(data$ma_min >14.8)),]
Barstovian1.1<-data[which((data$ma_max<=15.9)&(data$ma_max>14.8)&(data$ma_min>12.5)),]

Hemingfordian2<-data[which((data$ma_max<=17.5)&(data$ma_min>15.9)),]
Hemingfordian2.1<-data[which((data$ma_max<=17.5)&(data$ma_max>15.9)&(data$ma_min>14.8)),]
Hemingfordian1<-data[which((data$ma_max<=18.9)&(data$ma_min>17.5)),]
Hemingfordian1.1<-data[which((data$ma_max<=18.9)&(data$ma_max>17.5)&(data$ma_min>15.9)),]


Rancholabrean<-rbind(Rancholabrean,Rancholabrean.1)
Irvingtonian2<-rbind(Irvingtonian2,Irvingtonian2.1)
Irvingtonian1<-rbind(Irvingtonian1,Irvingtonian1.1)
EarlyBlancan<-rbind(EarlyBlancan,EarlyBlancan.1)
LateBlancan<-rbind(LateBlancan,LateBlancan.1)
EarlyHemphillian<-rbind(EarlyHemphillian,EarlyHemphillian.1)
LateHemphillian<-rbind(LateHemphillian,LateHemphillian.1)
Clarendonian3<-rbind(Clarendonian3,Clarendonian3.1)
Clarendonian2<-rbind(Clarendonian2,Clarendonian2.1)
Clarendonian1<-rbind(Clarendonian1,Clarendonian1.1)
Barstovian2<-rbind(Barstovian2,Barstovian2.1)
Barstovian1<-rbind(Barstovian1,Barstovian1.1)
Hemingfordian2<-rbind(Hemingfordian2,Hemingfordian2.1)
Hemingfordian1<-rbind(Hemingfordian1,Hemingfordian1.1)
####2 Divisions of Hemphillian
rodentiaData<-rbind(Holocene,Rancholabrean,Irvingtonian2,Irvingtonian1,LateBlancan,EarlyBlancan,LateHemphillian,EarlyHemphillian,Clarendonian3,Clarendonian2,Clarendonian1,Barstovian2,Barstovian1,Hemingfordian2,Hemingfordian1)
####Clean Up Environment
rm(Barstovian1.1,Barstovian2.1,EarlyBlancan.1,LateBlancan.1,Clarendonian1.1,Clarendonian2.1,Clarendonian3.1,Hemingfordian1.1,Hemingfordian2.1,EarlyHemphillian.1,LateHemphillian.1,Irvingtonian1.1,Irvingtonian2.1,Rancholabrean.1)

####Re-Load Tables to remove "hidden" genera
write.table(Hemingfordian1,"Hemingfordian1.txt",sep="\t")
Hemingfordian1<-read.table("Hemingfordian1.txt",header=TRUE)
write.table(Hemingfordian2,"Hemingfordian2.txt",sep="\t")
Hemingfordian2<-read.table("Hemingfordian2.txt",header=TRUE)
write.table(Barstovian1,"Barstovian1.txt",sep="\t")
Barstovian1<-read.table("Barstovian1.txt",header=TRUE)
write.table(Barstovian2,"Barstovian2.txt",sep="\t")
Barstovian2<-read.table("Barstovian2.txt",header=TRUE)
write.table(Clarendonian1,"Clarendonian1.txt",sep="\t")
Clarendonian1<-read.table("Clarendonian1.txt",header=TRUE)
write.table(Clarendonian2,"Clarendonian2.txt",sep="\t")
Clarendonian2<-read.table("Clarendonian2.txt",header=TRUE)
write.table(Clarendonian3,"Clarendonian3.txt",sep="\t")
Clarendonian3<-read.table("Clarendonian3.txt",header=TRUE)
write.table(LateHemphillian,"Late Hemphillian.txt",sep="\t")
LateHemphillian<-read.table("Late Hemphillian.txt",header=TRUE)
write.table(EarlyHemphillian,"Early Hemphillian.txt",sep="\t")
EarlyHemphillian<-read.table("Early Hemphillian.txt",header=TRUE)
write.table(LateBlancan,"Late Blancan.txt",sep="\t")
LateBlancan<-read.table("Late Blancan.txt",header=TRUE)
write.table(EarlyBlancan,"Early Blancan.txt",sep="\t")
EarlyBlancan<-read.table("Early Blancan.txt",header=TRUE)
write.table(Irvingtonian1,"Irvingtonian1.txt",sep="\t")
Irvingtonian1<-read.table("Irvingtonian1.txt",header=TRUE)
write.table(Irvingtonian2,"Irvingtonian2.txt",sep="\t")
Irvingtonian2<-read.table("Irvingtonian2.txt",header=TRUE)
write.table(Rancholabrean,"Rancholabrean.txt",sep="\t")
Rancholabrean<-read.table("Rancholabrean.txt",header=TRUE)
write.table(Holocene,"Holocene.txt",sep="\t")
Holocene<-read.table("Holocene.txt",header=TRUE)

#Create Mtax Matrices
Mtax<-function(x){
    for(i in 1:nrow(x))
    outTable<-crossprod(table(x$family_name,x$occurrence.genus_name))
    # outTable[lower.tri(outTable, diag=TRUE)] <- NA
    outTable[outTable>0]<-1
    outTable<-outTable
}

MtaxHemingfordian1<-Mtax(Hemingfordian1)  
MtaxHemingfordian2<-Mtax(Hemingfordian2)
MtaxBarstovian1<-Mtax(Barstovian1)
MtaxBarstovian2<-Mtax(Barstovian2)
MtaxClarendonian1<-Mtax(Clarendonian1)
MtaxClarendonian2<-Mtax(Clarendonian2)
MtaxClarendonian3<-Mtax(Clarendonian3)
MtaxEarlyHemphillian<-Mtax(EarlyHemphillian)
MtaxLateHemphillian<-Mtax(LateHemphillian)
MtaxEarlyBlancan<-Mtax(EarlyBlancan)
MtaxLateBlancan<-Mtax(LateBlancan)
MtaxIrvingtonian1<-Mtax(Irvingtonian1)
MtaxIrvingtonian2<-Mtax(Irvingtonian2)
MtaxRancholabrean<-Mtax(Rancholabrean)
MtaxHolocene<-Mtax(Holocene)

#Create Mext Matrices
Mext<-function(x,y,z){
  for(i in 1:nrow(x))
  taxa<-x$occurrence.genus_name
  taxa.names<-levels(taxa)
  numtaxa<-length(taxa.names)
  outTable<-array(0,dim=c(numtaxa,numtaxa))
  rownames(outTable)<-taxa.names
  colnames(outTable)<-taxa.names
  for (i in 1:numtaxa)
  extincttaxa<-which(row.names(y) %in%  row.names(z))
  extincttaxa<-row.names(y[-extincttaxa, ])
  outTable[extincttaxa,extincttaxa]<-1
  # outTable[lower.tri(outTable, diag=TRUE)]<-NA
  outTable<-outTable
}

MextHemingfordian1<-Mext(Hemingfordian1,MtaxHemingfordian1,MtaxHemingfordian2)
MextHemingfordian2<-Mext(Hemingfordian2,MtaxHemingfordian2,MtaxBarstovian1)
MextBarstovian1<-Mext(Barstovian1,MtaxBarstovian1,MtaxBarstovian2)
MextBarstovian2<-Mext(Barstovian2,MtaxBarstovian2,MtaxClarendonian1)
MextClarendonian1<-Mext(Clarendonian1,MtaxClarendonian1,MtaxClarendonian2)
MextClarendonian2<-Mext(Clarendonian2,MtaxClarendonian2,MtaxClarendonian3)
MextClarendonian3<-Mext(Clarendonian3,MtaxClarendonian3,MtaxEarlyHemphillian)
MextEarlyHemphillian<-Mext(EarlyHemphillian,MtaxEarlyHemphillian,MtaxLateHemphillian)
MextLateHemphillian<-Mext(LateHemphillian,MtaxLateHemphillian,MtaxEarlyBlancan)
MextEarlyBlancan<-Mext(EarlyBlancan,MtaxEarlyBlancan,MtaxLateBlancan)
MextLateBlancan<-Mext(LateBlancan,MtaxLateBlancan,MtaxIrvingtonian1)
MextIrvingtonian1<-Mext(Irvingtonian1,MtaxIrvingtonian1,MtaxIrvingtonian2)
MextIrvingtonian2<-Mext(Irvingtonian2,MtaxIrvingtonian2,MtaxRancholabrean)
MextRancholabrean<-Mext(Rancholabrean,MtaxRancholabrean,MtaxHolocene)

#Create Morg Matrices
Morig<-function(x,y,z){
  for(i in 1:nrow(x))
  taxa<-x$occurrence.genus_name
  taxa.names<-levels(taxa)
  numtaxa<-length(taxa.names)
  outTable<-array(0,dim=c(numtaxa,numtaxa))
  rownames(outTable)<-taxa.names
  colnames(outTable)<-taxa.names
  for (i in 1:numtaxa)
  newtaxa<-which(row.names(y) %in%  row.names(z))
  newtaxa<-row.names(y[-newtaxa, ])
  outTable[newtaxa,newtaxa]<-1
  # outTable[lower.tri(outTable, diag=TRUE)]<-NA
  outTable<-outTable
}

MorigHemingfordian2<-Morig(Hemingfordian2,MtaxHemingfordian2,MtaxHemingfordian1)
MorigBarstovian1<-Morig(Barstovian1,MtaxBarstovian1,MtaxHemingfordian2)
MorigBarstovian2<-Morig(Barstovian2,MtaxBarstovian2,MtaxBarstovian1)
MorigClarendonian1<-Morig(Clarendonian1,MtaxClarendonian1,MtaxBarstovian2)
MorigClarendonian2<-Morig(Clarendonian2,MtaxClarendonian2,MtaxClarendonian1)
MorigClarendonian3<-Morig(Clarendonian3,MtaxClarendonian3,MtaxClarendonian2)
MorigEarlyHemphillian<-Morig(EarlyHemphillian,MtaxEarlyHemphillian,MtaxClarendonian3)
MorigLateHemphillian<-Morig(LateHemphillian,MtaxLateHemphillian,MtaxEarlyHemphillian)
MorigEarlyBlancan<-Morig(EarlyBlancan,MtaxEarlyBlancan,MtaxLateHemphillian)
MorigLateBlancan<-Morig(LateBlancan,MtaxLateBlancan,MtaxEarlyBlancan)
MorigIrvingtonian1<-Morig(Irvingtonian1,MtaxIrvingtonian1,MtaxLateBlancan)
MorigIrvingtonian2<-Morig(Irvingtonian2,MtaxIrvingtonian2,MtaxIrvingtonian1)
MorigRancholabrean<-Morig(Rancholabrean,MtaxRancholabrean,MtaxIrvingtonian2)
MorigHolocene<-Morig(Holocene,MtaxHolocene,MtaxRancholabrean)

#Matrix Correlations
MEXT<-c(mean(cor(MextHemingfordian1,MtaxHemingfordian1,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextHemingfordian2,MtaxHemingfordian2,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextBarstovian1,MtaxBarstovian1,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextBarstovian2,MtaxBarstovian2,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextClarendonian1,MtaxClarendonian1,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextClarendonian2,MtaxClarendonian2,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextClarendonian3,MtaxClarendonian3,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextEarlyHemphillian,MtaxEarlyHemphillian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextLateHemphillian,MtaxLateHemphillian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextEarlyBlancan,MtaxEarlyBlancan,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextLateBlancan,MtaxLateBlancan,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextIrvingtonian1,MtaxIrvingtonian1,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextIrvingtonian1,MtaxIrvingtonian1,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextRancholabrean,MtaxRancholabrean,use="pairwise.complete.obs"),na.rm=TRUE))

MORIG<-c(mean(cor(MorigHemingfordian2,MtaxHemingfordian2,use="pairwise.complete.obs"),na.rm=TRUE),
         mean(cor(MorigBarstovian1,MtaxBarstovian1,use="pairwise.complete.obs"),na.rm=TRUE),
         mean(cor(MorigBarstovian2,MtaxBarstovian2,use="pairwise.complete.obs"),na.rm=TRUE),
         mean(cor(MorigClarendonian1,MtaxClarendonian1,use="pairwise.complete.obs"),na.rm=TRUE),
         mean(cor(MorigClarendonian2,MtaxClarendonian2,use="pairwise.complete.obs"),na.rm=TRUE),
         mean(cor(MorigClarendonian3,MtaxClarendonian3,use="pairwise.complete.obs"),na.rm=TRUE),
         mean(cor(MorigEarlyHemphillian,MtaxEarlyHemphillian,use="pairwise.complete.obs"),na.rm=TRUE),
         mean(cor(MorigLateHemphillian,MtaxLateHemphillian,use="pairwise.complete.obs"),na.rm=TRUE),
         mean(cor(MorigEarlyBlancan,MtaxEarlyBlancan,use="pairwise.complete.obs"),na.rm=TRUE),
         mean(cor(MorigLateBlancan,MtaxLateBlancan,use="pairwise.complete.obs"),na.rm=TRUE),
         mean(cor(MorigIrvingtonian1,MtaxIrvingtonian1,use="pairwise.complete.obs"),na.rm=TRUE),
         mean(cor(MorigIrvingtonian2,MtaxIrvingtonian2,use="pairwise.complete.obs"),na.rm=TRUE),
         mean(cor(MorigRancholabrean,MtaxRancholabrean,use="pairwise.complete.obs"),na.rm=TRUE),
         mean(cor(MorigHolocene,MtaxHolocene,use="pairwise.complete.obs"),na.rm=TRUE))

#Plotting
x<-1
y<-1
NALMA<-c("Early Hemingfordian","Late Hemingfordian","Early Barstovian","Late Barstovian","Early Clarendonian","Middle Clarendonian","Late Clarendonian","Early Hemphillian","Late Hemphillian","Early Blancan","Late Blancan","Early Irvingtonian","Late Irvingtonian","Rancholabrean","Holocene")
Mid<-c(18.2,16.7,15.35,13.65,12.25,11,9.5,7.9,5.775,3.675,2.1,1.285,0.575,0.156,0.006)
MidEXT<-c(18.2,16.7,15.35,13.65,12.25,11,9.5,7.9,5.775,3.675,2.1,1.285,0.575,0.156)
MidORIG<-c(16.7,15.35,13.65,12.25,11,9.5,7.9,5.775,3.675,2.1,1.285,0.575,0.156,0.006)
#MEXT
par(mar=c(3,5,5,2))
plot(x,y,main="EXTINCTION",type="n",xlab=NA,,xaxt="n",yaxt="n",ylab=expression('R'[CL]),xlim=c(20,0),ylim=c(-0.15,0.15),cex.lab=1.5)
axis(side=1,at=c(20,16,12,8,4,0),labels=c(20,16,12,8,4,0))
axis(side=2,at=c(-0.15,0,0.15),labels=c(-0.15,0,0.15))
points(MidEXT,MEXT,cex=1.2,pch=19,col="black")
lines(MidEXT,MEXT,lwd=2)
abline(h=0)
abline(v=0.012,lty=3)
abline(v=0.85,lty=3)
abline(v=1.6,lty=3)
abline(v=4.75,lty=3)
abline(v=6.8,lty=3)
abline(v=9,lty=3)
#MORIG
par(mar=c(3,5,5,2))
plot(x,y,main="ORIGINATION",type="n",xlab=NA,,xaxt="n",yaxt="n",ylab=expression('R'[CL]),xlim=c(20,0),ylim=c(-0.15,0.15),cex.lab=1.5)
axis(side=1,at=c(20,16,12,8,4,0),labels=c(20,16,12,8,4,0))
axis(side=2,at=c(-0.15,0,0.15),labels=c(-0.15,0,0.15))
points(MidORIG,MORIG,cex=1.2,pch=19,col="black")
lines(MidORIG,MORIG,lwd=2)
abline(h=-0.3,lwd=2)
abline(h=0)
abline(v=0.012,lty=3)
abline(v=0.85,lty=3)
abline(v=1.6,lty=3)
abline(v=4.75,lty=3)
abline(v=6.8,lty=3)
abline(v=9,lty=3)



The significance of both positive and negative
excursions of RCL was determined by randomizing the genera within each interval 
that originated or went extinct, recalculating RCL, and repeating 1000 times.