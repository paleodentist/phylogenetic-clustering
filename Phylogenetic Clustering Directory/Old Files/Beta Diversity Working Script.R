rm(list=ls())
data<-read.csv('RODENT_SPECIES.csv', header=TRUE)
library(maps)
library(vegan)
letstrythis<-function(x){
for(i in 1:nrow(x)){
	if((x$paleolatdec[i]>=0)&(x$paleolatdec[i]<=5)){x$paleolat[i]<-1}
	else
	if((x$paleolatdec[i]>5)&(x$paleolatdec[i]<=10)){x$paleolat[i]<-2}
	else
	if((x$paleolatdec[i]>10)&(x$paleolatdec[i]<=15)){x$paleolat[i]<-3}
	else
	if((x$paleolatdec[i]>15)&(x$paleolatdec[i]<=20)){x$paleolat[i]<-4}
	else
	if((x$paleolatdec[i]>20)&(x$paleolatdec[i]<=25)){x$paleolat[i]<-5}
	else
	if((x$paleolatdec[i]>25)&(x$paleolatdec[i]<=30)){x$paleolat[i]<-6}
	else
	if((x$paleolatdec[i]>30)&(x$paleolatdec[i]<=35)){x$paleolat[i]<-7}
	else
	if((x$paleolatdec[i]>35)&(x$paleolatdec[i]<=40)){x$paleolat[i]<-8}
	else
	if((x$paleolatdec[i]>40)&(x$paleolatdec[i]<=45)){x$paleolat[i]<-9}
	else
	if((x$paleolatdec[i]>45)&(x$paleolatdec[i]<=50)){x$paleolat[i]<-10}
	else
	if((x$paleolatdec[i]>50)&(x$paleolatdec[i]<=55)){x$paleolat[i]<-11}
	else
	if((x$paleolatdec[i]>55)&(x$paleolatdec[i]<=60)){x$paleolat[i]<-12}
  else
  if((x$paleolatdec[i]>60)&(x$paleolatdec[i]<=65)){x$paleolat[i]<-13}
  else
  if((x$paleolatdec[i]>65)&(x$paleolatdec[i]<=70)){x$paleolat[i]<-14}
  else
  if((x$paleolatdec[i]>70)&(x$paleolatdec[i]<=75)){x$paleolat[i]<-15}
  else
  if((x$paleolatdec[i]>75)&(x$paleolatdec[i]<=80)){x$paleolat[i]<-16}
  else
  if((x$paleolatdec[i]>80)&(x$paleolatdec[i]<=85)){x$paleolat[i]<-17}
  else
  if((x$paleolatdec[i]>85)&(x$paleolatdec[i]<=90)){x$paleolat[i]<-18}
  }
taxa<-x$full_name
taxa.names<-levels(taxa)
latitudes<-x$paleolat
latitudes.names<-unique(latitudes)
numtaxa<-length(taxa.names)
numlats<-length(latitudes.names)

outTable<-array(0, dim=c(numlats,numtaxa))
rownames(outTable)<-latitudes.names
colnames(outTable)<-taxa.names

for (i in 1:numlats){
	subtaxa<-as.character(taxa[latitudes==latitudes.names[i]])
	present<-which(taxa.names %in% subtaxa)
	outTable[i,present]<-1}
outTable<-outTable
outTable<-outTable[,colSums(outTable^2) !=0]
}

####Bin by NALMA		
		
Holocene<-data[which(data$ma_max<=0.012),]
Rancholabrean<- data[which((data$ma_max<=0.3)&(data$ma_min>0.012)),]
Rancholabrean1<-data[which((data$ma_max<=0.3)&(data$ma_max>0.012)&(data$ma_min>=0)),]

Irvingtonian<-data[which((data$ma_max<=1.8)&(data$ma_min>0.3)),]
Irvingtonian1<-data[which((data$ma_max<=1.8)&(data$ma_max>0.3)&(data$ma_min>0.012)),]

Blancan<-data[which((data$ma_max<=4.9)&(data$ma_min>1.8)),]
Blancan1<-data[which((data$ma_max<=4.9)&(data$ma_max>1.8)&(data$ma_min>0.3)),]

Hemphillian<-data[which((data$ma_max<=10.3)&(data$ma_min >4.9)),]
Hemphillian1<-data[which((data$ma_max<=10.3)&(data$ma_max>4.9)&(data$ma_min>1.8)),]

Clarendonian<-data[which((data$ma_max<=13.6)&(data$ma_min>10.3)),]
Clarendonian1<-data[which((data$ma_max<=13.6)&(data$ma_max>10.3)&(data$ma_min>4.9)),]

Barstovian<-data[which((data$ma_max<=15.97)&(data$ma_min >13.6)),]
Barstovian1<-data[which((data$ma_max<=15.97)&(data$ma_max>13.6)&(data$ma_min>10.3)),]

Hemingfordian<-data[which((data$ma_max<=20.43)&(data$ma_min>15.97)),]
Hemingfordian1<-data[which((data$ma_max<=20.43)&(data$ma_max>15.97)&(data$ma_min>13.6)),]
Rancholabrean<-rbind(Rancholabrean,Rancholabrean1)
Irvingtonian<-rbind(Irvingtonian,Irvingtonian1)
Blancan<-rbind(Blancan,Blancan1)
Hemphillian<-rbind(Hemphillian,Hemphillian1)
Clarendonian<-rbind(Clarendonian,Clarendonian1)
Barstovian<-rbind(Barstovian,Barstovian1)
Hemingfordian<-rbind(Hemingfordian,Hemingfordian1)
rodentiaData<-rbind(Holocene,Rancholabrean,Irvingtonian,Blancan,Hemphillian,Clarendonian,Barstovian,Hemingfordian)

Holo<-letstrythis(Holocene)
#write.table(Holo,"Holocene.txt",sep="\t")
Rancho<-letstrythis(Rancholabrean)
#write.table(Rancho,"Rancholabrean.txt",sep="\t")
Irving<-letstrythis(Irvingtonian)
#write.table(Irving,"Irvingtonian.txt",sep="\t")
Blanc<-letstrythis(Blancan)
#write.table(Blanc,"Blanc.txt",sep="\t")
Hemph<-letstrythis(Hemphillian)
#write.table(Hemph,"Hemphillian.txt",sep="\t")
Clare<-letstrythis(Clarendonian)
#write.table(Clare,"Clarendonian.txt",sep="\t")
Barst<-letstrythis(Barstovian)
#write.table(Barst,"Barstovian.txt",sep="\t")
Hemi<-letstrythis(Hemingfordian)
#write.table(Hemi,"Hemingfordian.txt",sep="\t")

#Calculate Sorensen indices of dissimilarity for each NALMA
BetaHolo<-betadiver(Holo,method=1)
BetaRanch<-betadiver(Rancho,method=1)
BetaIrv<-betadiver(Irving,method=1)
BetaBlanc<-betadiver(Blanc,method=1)
BetaHemp<-betadiver(Hemph,method=1)
BetaClar<-betadiver(Clare,method=1)
BetaBar<-betadiver(Barst,method=1)
BetaHeming<-betadiver(Hemi,method=1)

BetaHolo<-as.data.frame(BetaHolo)

#Set groups for betadisper function
groupsHolo<-as.factor(c(1:9),labels=c("30 - 40","20 - 30","40 - 50","0 - 10","50 - 60","60 - 70","70 - 80","10 - 20","80 - 90"))
groupsRanch<-factor(c(1:8),labels=c("30 - 40","20 - 30","40 - 50","50 - 60","60 - 70","0 - 10","10 - 20","70 - 80"))

BetaHolo<-betadiver(Holocene,method=1)
mod<-betadisper(BetaHolo,groupsHolo,type="centroid")
mod

median<-c(median(BetaHolo),median(BetaRanch),median(BetaIrv),median(BetaBlanc),median(BetaHemp),median(BetaClar),median(BetaBar),median(BetaHeming))
mean<-c(mean(BetaHolo),mean(BetaRanch),mean(BetaIrv),mean(BetaBlanc),mean(BetaHemp),mean(BetaClar),mean(BetaBar),mean(BetaHeming))
age<-c(0.006,0.021,1.05,3.3,7.8,11.9,14.75,18.2)

#Average Beta Divrsity through Time
Mean<-c(mean(BetaHeming),mean(BetaBar),mean(BetaClar),mean(BetaHemp),mean(BetaBlanc),mean(BetaIrv),mean(BetaRanch),mean(BetaHolo))
Time<-c(1:8)
plot(Time,Mean,type="l",ylim=c(0,1),ylab="Beta Diversity")


Holo<-read.table("Holo.txt")
Holo<-Holo[2:8, ]
Ranch<-read.table("Ranch.txt")
Ranch<-Ranch[3:9, ]
Irving<-read.table("Irving.txt")
Irving<-Irving[2:6, ]
Blanc<-read.table("Blanc.txt")
Blanc<-Blanc[2:5, ]
Hemph<-read.table("Hemph.txt")
Hemph<-Hemph[2:5, ]
Barst<-read.table("Barst.txt")
Clare<-read.table("Clare.txt")
Hemi<-read.table("Hemi.txt")

labelsHolo<-c("30","40","50","60")
#Latitudinal Beta Diversity, separate plots
par(mar=c(2,5,5,2))
plot(Holo$V2,Holo$V1,type="n",xaxt="n",yaxt="n",xlab=NA,ylab=NA,xlim=c(0,1),ylim=c(6,12))
axis(side=2,at=c(6,8,10,12),labels=labelsHolo,las=2)
axis(side=3,xlim=c(0,1))
grid(ny=NA,nx=NULL,lty="dotted")
mtext(text=expression(paste("BETA DIVERSITY (", beta ,")" )),side=3,line=3,outer=FALSE)
mtext(text="LATITUDE (\u00B0 N)",side=2,line=3)
#Holocene
points(Holo$V2,Holo$V1,col="green",pch=18,cex=2)
abline(lsfit(Holo$V2,Holo$V1,intercept = FALSE),col="green")
cor(Holo$V2,Holo$V1)
text(x=0.7,y=8,labels="r = 0.614")
mtext(side=1,"HOLOCENE",cex=1.6,line=1)
#Rancholabrean
points(Ranch$V2,Ranch$V1,col="cyan",pch=18,cex=2)
abline(lsfit(Ranch$V2,Ranch$V1,intercept = FALSE),col="cyan")
cor(Ranch$V2,Ranch$V1)
text(x=0.7,y=8,labels="r = 0.696")
mtext(side=1,"RANCHOLABREAN",cex=1.6,line=1)
#Irvingtonian
points(Irving$V2,Irving$V1,col="blue",pch=18,cex=2)
abline(lsfit(Irving$V2,Irving$V1,intercept = FALSE),col="blue")
cor(Irving$V2,Irving$V1)
text(x=0.5,y=9,labels="r = 0.216")
mtext(side=1,"Irvingtonian",cex=1.6,line=1)
#Blancan
points(Blanc$V2,Blanc$V1,col="purple",pch=18,cex=2)
abline(lsfit(Blanc$V2,Blanc$V1,intercept = FALSE),col="purple")
cor(Blanc$V2,Blanc$V1)
text(x=0.7,y=10,labels="r = -0.556")
mtext(side=1,"Blancan",cex=1.6,line=1)
#Hemphillian
points(Hemph$V2,Hemph$V1,col="deeppink",pch=18,cex=2)
abline(lsfit(Hemph$V2,Hemph$V1,intercept = FALSE),col="deeppink")
cor(Hemph$V2,Hemph$V1)
text(x=0.5,y=7,labels="r = -0.941")
mtext(side=1,"Hemphillian",cex=1.6,line=1)
#Barstovian
points(Barst$V2,Barst$V1,col="red",pch=18,cex=2)
abline(lsfit(Barst$V2,Barst$V1,intercept = FALSE),col="red")
cor(Barst$V2,Barst$V1)
text(x=0.4,y=8,labels="r = -0.308")
mtext(side=1,"Barstovian",cex=1.6,line=1)
#Clarendonian
points(Clare$V2,Clare$V1,col="orange",pch=18,cex=2)
abline(lsfit(Clare$V2,Clare$V1,intercept = FALSE),col="orange")
cor(Clare$V2,Clare$V1)
text(x=0.7,y=9,labels="r = -0.031")
mtext(side=1,"Clarendonian",cex=1.6,line=1)
#Hemingfordian
points(Hemi$V2,Hemi$V1,col="brown",pch=18,cex=2)
abline(lsfit(Hemi$V2,Hemi$V1,intercept = FALSE),col="brown")
cor(Hemi$V2,Hemi$V1)
text(x=0.7,y=9,labels="r = 0.725")
mtext(side=1,"Hemingfordian",cex=1.6,line=1)

#Latitudinal Beta Diversity, all plots in 1
par(mar=c(2,4,4,2),mfrow=c(2,4),oma=c(2,5,5,2))
#Hemingfordian
plot(Holo$V2,Holo$V1,type="n",xaxt="n",yaxt="n",xlab=NA,ylab=NA,xlim=c(0,1),ylim=c(6,12))
axis(side=2,at=c(6,8,10,12),labels=labelsHolo,las=2)
axis(side=3,xlim=c(0,1))
grid(ny=NA,nx=NULL,lty="dotted")
points(Hemi$V2,Hemi$V1,col="brown",pch=18,cex=2)
abline(lsfit(Hemi$V2,Hemi$V1,intercept = FALSE),col="brown")
cor(Hemi$V2,Hemi$V1)
text(x=0.2,y=12,labels="n = 75")
text(x=0.7,y=9,labels="r = 0.725")
mtext(side=1,"Hemingfordian",line=1)
#Barstovian
plot(Holo$V2,Holo$V1,type="n",xaxt="n",yaxt="n",xlab=NA,ylab=NA,xlim=c(0,1),ylim=c(6,12))
axis(side=2,at=c(6,8,10,12),labels=labelsHolo,las=2)
axis(side=3,xlim=c(0,1))
grid(ny=NA,nx=NULL,lty="dotted")
points(Barst$V2,Barst$V1,col="red",pch=18,cex=2)
abline(lsfit(Barst$V2,Barst$V1,intercept = TRUE),col="red")
cor(Barst$V2,Barst$V1)
text(x=0.2,y=12,labels="n = 133")
text(x=0.4,y=8,labels="r = -0.308")
mtext(side=1,"Barstovian",line=1)
#Clarendonian
plot(Holo$V2,Holo$V1,type="n",xaxt="n",yaxt="n",xlab=NA,ylab=NA,xlim=c(0,1),ylim=c(6,12))
axis(side=2,at=c(6,8,10,12),labels=labelsHolo,las=2)
axis(side=3,xlim=c(0,1))
grid(ny=NA,nx=NULL,lty="dotted")
points(Clare$V2,Clare$V1,col="orange",pch=18,cex=2)
abline(lsfit(Clare$V2,Clare$V1,intercept = TRUE),col="orange")
cor(Clare$V2,Clare$V1)
text(x=0.2,y=12,labels="n = 99")
text(x=0.7,y=9,labels="r = -0.031")
mtext(side=1,"Clarendonian",line=1)
#Hemphillian
plot(Holo$V2,Holo$V1,type="n",xaxt="n",yaxt="n",xlab=NA,ylab=NA,xlim=c(0,1),ylim=c(6,12))
axis(side=2,at=c(6,8,10,12),labels=labelsHolo,las=2)
axis(side=3,xlim=c(0,1))
grid(ny=NA,nx=NULL,lty="dotted")
points(Hemph$V2,Hemph$V1,col="deeppink",pch=18,cex=2)
abline(lsfit(Hemph$V2,Hemph$V1,intercept = TRUE),col="deeppink")
cor(Hemph$V2,Hemph$V1)
text(x=0.2,y=12,labels="n = 130")
text(x=0.5,y=7,labels="r = -0.941")
mtext(side=1,"Hemphillian",line=1)
#Blancan
plot(Holo$V2,Holo$V1,type="n",xaxt="n",yaxt="n",xlab=NA,ylab=NA,xlim=c(0,1),ylim=c(6,12))
axis(side=2,at=c(6,8,10,12),labels=labelsHolo,las=2)
axis(side=3,xlim=c(0,1))
grid(ny=NA,nx=NULL,lty="dotted")
points(Blanc$V2,Blanc$V1,col="purple",pch=18,cex=2)
abline(lsfit(Blanc$V2,Blanc$V1,intercept = TRUE),col="purple")
cor(Blanc$V2,Blanc$V1)
text(x=0.2,y=12,labels="n = 196")
text(x=0.7,y=10,labels="r = -0.556")
mtext(side=1,"Blancan",line=1)
#Irvingtonian
plot(Holo$V2,Holo$V1,type="n",xaxt="n",yaxt="n",xlab=NA,ylab=NA,xlim=c(0,1),ylim=c(6,12))
axis(side=2,at=c(6,8,10,12),labels=labelsHolo,las=2)
axis(side=3,xlim=c(0,1))
grid(ny=NA,nx=NULL,lty="dotted")
points(Irving$V2,Irving$V1,col="blue",pch=18,cex=2)
abline(lsfit(Irving$V2,Irving$V1,intercept = FALSE),col="blue")
cor(Irving$V2,Irving$V1)
text(x=0.2,y=12,labels="n = 131")
text(x=0.5,y=9,labels="r = 0.216")
mtext(side=1,"Irvingtonian",line=1)
#Rancholabrean
plot(Holo$V2,Holo$V1,type="n",xaxt="n",yaxt="n",xlab=NA,ylab=NA,xlim=c(0,1),ylim=c(6,12))
axis(side=2,at=c(6,8,10,12),labels=labelsHolo,las=2)
axis(side=3,xlim=c(0,1))
grid(ny=NA,nx=NULL,lty="dotted")
points(Ranch$V2,Ranch$V1,col="cyan",pch=18,cex=2)
abline(lsfit(Ranch$V2,Ranch$V1,intercept = FALSE),col="cyan")
cor(Ranch$V2,Ranch$V1)
text(x=0.2,y=12,labels="n = 169")
text(x=0.7,y=8,labels="r = 0.696")
mtext(side=1,"Rancholabrean",line=1)
#Holocene
plot(Holo$V2,Holo$V1,type="n",xaxt="n",yaxt="n",xlab=NA,ylab=NA,xlim=c(0,1),ylim=c(6,12))
axis(side=2,at=c(6,8,10,12),labels=labelsHolo,las=2)
axis(side=3,xlim=c(0,1))
grid(ny=NA,nx=NULL,lty="dotted")
points(Holo$V2,Holo$V1,col="green",pch=18,cex=2)
abline(lsfit(Holo$V2,Holo$V1,intercept = FALSE),col="green")
cor(Holo$V2,Holo$V1)
text(x=0.2,y=12,labels="n = 148")
text(x=0.7,y=8,labels="r = 0.614")
mtext(side=1,"Holocene",line=1)

mtext(text=expression(paste("BETA DIVERSITY (", beta ,")" )),side=3,line=0,cex=1.2,outer=TRUE)
mtext(text="LATITUDE (\u00B0 N)",side=2,line=1,cex=1.2,outer=TRUE)

