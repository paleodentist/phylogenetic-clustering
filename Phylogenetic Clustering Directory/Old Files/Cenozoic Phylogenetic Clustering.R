rm(list=ls())
data<-read.csv('GENERA.csv',header=TRUE,stringsAsFactors = FALSE)
library(maps)
library(vegan)
library(MASS)
library(ggplot2)
Cetacea<-which(data$order=="Cetacea")
Sirenia<-which(data$order=="Sirenia")
aquatic<-c(Cetacea,Sirenia)
data<-data[-aquatic,]
data<-data[,1:17]

####Compartmentalize the Data by Order####
Perissodactyla<-data[which(data$order=="Perissodactyla"),]
Proboscidea<-data[which(data$order=="Proboscidea"),]
Artiodactyla<-data[which(data$order=="Artiodactyla"),]

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
Arikareean<-data[which((data$ma_max<=30.8)&(data$ma_min>20.43)),]
Arikareean1<-data[which((data$ma_max<=30.8)&(data$ma_max>20.43)&(data$ma_min>15.97)),]
Whitneyan<-data[which((data$ma_max<=33.3)&(data$ma_min>30.8)),]
Whitneyan1<-data[which((data$ma_max<=33.3)&(data$ma_max>30.8)&(data$ma_min>20.43)),]
Orellan<-data[which((data$ma_max<=33.9)&(data$ma_min>33.3)),]
Orellan1<-data[which((data$ma_max<=33.9)&(data$ma_max>33.3)&(data$ma_min>30.8)),]
Chadronian<-data[which((data$ma_max<=37.2)&(data$ma_min>33.9)),]
Chadronian1<-data[which((data$ma_max<=37.2)&(data$ma_max>33.9)&(data$ma_min>33.3)),]
Duchesnean<-data[which((data$ma_max<=40.4)&(data$ma_min>37.2)),]
Duchesnean1<-data[which((data$ma_max<=40.4)&(data$ma_max>37.2)&(data$ma_min>33.9)),]
Uintan<-data[which((data$ma_max<=46.2)&(data$ma_min>40.4)),]
Uintan1<-data[which((data$ma_max<=46.2)&(data$ma_max>40.4)&(data$ma_min>37.2)),]
Bridgerian<-data[which((data$ma_max<=50.3)&(data$ma_min>46.2)),]
Bridgerian1<-data[which((data$ma_max<=50.3)&(data$ma_max>46.2)&(data$ma_min>40.4)),]
Wasatchian<-data[which((data$ma_max<=55.8)&(data$ma_min>50.3)),]
Wasatchian1<-data[which((data$ma_max<=55.8)&(data$ma_max>50.3)&(data$ma_min>46.2)),]
Clarkforkian<-data[which((data$ma_max<=56.8)&(data$ma_min>55.8)),]
Clarkforkian1<-data[which((data$ma_max<=56.8)&(data$ma_max>55.8)&(data$ma_min>50.3)),]
Tiffanian<-data[which((data$ma_max<=61.7)&(data$ma_min>56.8)),]
Tiffanian1<-data[which((data$ma_max<=61.7)&(data$ma_max>56.8)&(data$ma_min>55.8)),]
Torrejonian<-data[which((data$ma_max<=63.3)&(data$ma_min>61.7)),]
Torrejonian1<-data[which((data$ma_max<=63.3)&(data$ma_max>61.7)&(data$ma_min>56.8)),]
Puercan<-data[which((data$ma_max<=66)&(data$ma_min>63.3)),]
Puercan1<-data[which((data$ma_max<=66)&(data$ma_max>63.3)&(data$ma_min>61.7)),]

Rancholabrean<-rbind(Rancholabrean,Rancholabrean1)
Irvingtonian<-rbind(Irvingtonian,Irvingtonian1)
Blancan<-rbind(Blancan,Blancan1)
Hemphillian<-rbind(Hemphillian,Hemphillian1)
Clarendonian<-rbind(Clarendonian,Clarendonian1)
Barstovian<-rbind(Barstovian,Barstovian1)
Hemingfordian<-rbind(Hemingfordian,Hemingfordian1)
Arikareean<-rbind(Arikareean,Arikareean1)
Whitneyan<-rbind(Whitneyan,Whitneyan1)
Orellan<-rbind(Orellan,Orellan1)
Chadronian<-rbind(Chadronian,Chadronian1)
Duchesnean<-rbind(Duchesnean,Duchesnean1)
Uintan<-rbind(Uintan,Uintan1)
Bridgerian<-rbind(Bridgerian,Bridgerian1)
Wasatchian<-rbind(Wasatchian,Wasatchian1)
Clarkforkian<-rbind(Clarkforkian,Clarkforkian1)
Tiffanian<-rbind(Tiffanian,Tiffanian1)
Torrejonian<-rbind(Torrejonian,Torrejonian1)
Puercan<-rbind(Puercan,Puercan1)
Data<-rbind(Holocene,Rancholabrean,Irvingtonian,Blancan,Hemphillian,Clarendonian,Barstovian,Hemingfordian,Arikareean,Whitneyan,Orellan,Chadronian,Duchesnean,Uintan,Bridgerian,Wasatchian,Clarkforkian,Tiffanian,Torrejonian,Puercan)
####Clean Up Environment
rm(Barstovian1,Blancan1,Clarendonian1,Hemingfordian1,Hemphillian1,Irvingtonian1,Rancholabrean1,Arikareean1,Whitneyan1,Orellan1,Chadronian1,Duchesnean1,Uintan1,Bridgerian1,Wasatchian1,Clarkforkian1,Tiffanian1,Torrejonian1,Puercan1)

####Re-Load Tables to remove "hidden" genera
write.table(Puercan,"Puercan.txt",sep="\t")
Puercan<-read.table("Puercan.txt",header=TRUE)
write.table(Torrejonian,"Torrejonian.txt",sep="\t")
Torrejonian<-read.table("Torrejonian.txt",header=TRUE)
write.table(Tiffanian,"Tiffanian.txt",sep="\t")
Tiffanian<-read.table("Tiffanian.txt",header=TRUE)
write.table(Clarkforkian,"Clarkforkian.txt",sep="\t")
Clarkforkian<-read.table("Clarkforkian.txt",header=TRUE)
write.table(Wasatchian,"Wasatchian.txt",sep="\t")
Wasatchian<-read.table("Wasatchian.txt",header=TRUE)
write.table(Bridgerian,"Bridgerian.txt",sep="\t")
Bridgerian<-read.table("Bridgerian.txt",header=TRUE)
write.table(Uintan,"Uintan.txt",sep="\t")
Uintan<-read.table("Uintan.txt",header=TRUE)
write.table(Duchesnean,"Duchesnean.txt",sep="\t")
Duchesnean<-read.table("Duchesnean.txt",header=TRUE)
write.table(Chadronian,"Chadronian.txt",sep="\t")
Chadronian<-read.table("Chadronian.txt",header=TRUE)
write.table(Orellan,"Orellan.txt",sep="\t")
Orellan<-read.table("Orellan.txt",header=TRUE)
write.table(Whitneyan,"Whitneyan.txt",sep="\t")
Whitneyan<-read.table("Whitneyan.txt",header=TRUE)
write.table(Arikareean,"Arikareean.txt",sep="\t")
Arikareean<-read.table("Arikareean.txt",header=TRUE)
write.table(Hemingfordian,"Hemingfordian.txt",sep="\t")
Hemingfordian<-read.table("Hemingfordian.txt",header=TRUE)
write.table(Barstovian,"Barstovian.txt",sep="\t")
Barstovian<-read.table("Barstovian.txt",header=TRUE)
write.table(Clarendonian,"Clarendonian.txt",sep="\t")
Clarendonian<-read.table("Clarendonian.txt",header=TRUE)
write.table(Hemphillian,"Hemphillian.txt",sep="\t")
Hemphillian<-read.table("Hemphillian.txt",header=TRUE)
write.table(Blancan,"Blancan.txt",sep="\t")
Blancan<-read.table("Blancan.txt",header=TRUE)
write.table(Irvingtonian,"Irvingtonian.txt",sep="\t")
Irvingtonian<-read.table("Irvingtonian.txt",header=TRUE)
write.table(Rancholabrean,"Rancholabrean.txt",sep="\t")
Rancholabrean<-read.table("Rancholabrean.txt",header=TRUE)
write.table(Holocene,"Holocene.txt",sep="\t")
Holocene<-read.table("Holocene.txt",header=TRUE)
Survivors<-read.csv("HoloceneExtinctions.csv",header=TRUE,row.names=2)
Survivors<-Survivors[which(Survivors$Status>0),]

#Create Mtax Matrices
Mtax<-function(x){
  for(i in 1:nrow(x))
    outTable<-crossprod(table(x$family,x$genus))
  # outTable[lower.tri(outTable, diag=TRUE)] <- NA
  outTable[outTable>0]<-1
  outTable<-outTable
}

MtaxPuercan<-Mtax(Puercan)
write.matrix(MtaxPuercan,"MtaxPuercan.txt",sep="\t")
MtaxTorrejonian<-Mtax(Torrejonian)
write.matrix(MtaxTorrejonian,"MtaxTorrejonian",sep="\t")
MtaxTiffanian<-Mtax(Tiffanian)
write.matrix(MtaxTiffanian,"MtaxTiffanian",sep="\t")
MtaxClarkforkian<-Mtax(Clarkforkian)
write.matrix(MtaxClarkforkian,"MtaxClarkforkian",sep="\t")
MtaxWasatchian<-Mtax(Wasatchian)
write.matrix(MtaxWasatchian,"MtaxWasatchian",sep="\t")
MtaxBridgerian<-Mtax(Bridgerian)
write.matrix(MtaxBridgerian,"MtaxBridgerian",sep="\t")
MtaxUintan<-Mtax(Uintan)
write.matrix(MtaxUintan,"MtaxUintan",sep="\t")
MtaxDuchesnean<-Mtax(Duchesnean)
write.matrix(MtaxDuchesnean,"MtaxDuchesnean",sep="\t")
MtaxChadronian<-Mtax(Chadronian)
write.matrix(MtaxChadronian,"MtaxChadronian",sep="\t")
MtaxOrellan<-Mtax(Orellan)
write.matrix(MtaxOrellan,"MtaxOrellan",sep="\t")
MtaxWhitneyan<-Mtax(Whitneyan)
write.matrix(MtaxWhitneyan,"MtaxWhitneyan",sep="\t")
MtaxArikareean<-Mtax(Arikareean)
write.matrix(MtaxArikareean,"MtaxArikareean",sep="\t")
MtaxHemingfordian<-Mtax(Hemingfordian)
write.matrix(MtaxHemingfordian,"MtaxHemingfordian",sep="\t")
MtaxBarstovian<-Mtax(Barstovian)
write.matrix(MtaxBarstovian,"MtaxBarstovian",sep="\t")
MtaxClarendonian<-Mtax(Clarendonian)
write.matrix(MtaxClarendonian,"MtaxClarendonian",sep="\t")
MtaxHemphillian<-Mtax(Hemphillian)
write.matrix(MtaxHemphillian,"MtaxHemphillian",sep="\t")
MtaxBlancan<-Mtax(Blancan)
write.matrix(MtaxBlancan,"MtaxBlancan",sep="\t")
MtaxIrvingtonian<-Mtax(Irvingtonian)
write.matrix(MtaxIrvingtonian,"MtaxIrvingtonian",sep="\t")
MtaxRancholabrean<-Mtax(Rancholabrean)
write.matrix(MtaxRancholabrean,"MtaxRancholabrean",sep="\t")
MtaxHolocene<-Mtax(Holocene)
write.matrix(MtaxHolocene,"MtaxHolocene",sep="\t")

#Origin Species Lists (what existed before the focal time bin?)
OrigTorrejonian<-as.character(row.names(MtaxPuercan))
OrigTiffanian<-c(as.character(row.names(MtaxTorrejonian)),as.character(row.names(MtaxPuercan)))
OrigClarkforkian<-c(as.character(row.names(MtaxTiffanian)),as.character(row.names(MtaxTorrejonian)),as.character(row.names(MtaxPuercan)))
OrigWasatchian<-c(as.character(row.names(MtaxClarkforkian)),as.character(row.names(MtaxTiffanian)),as.character(row.names(MtaxTorrejonian)),as.character(row.names(MtaxPuercan)))
OrigBridgerian<-c(as.character(row.names(MtaxWasatchian)),as.character(row.names(MtaxClarkforkian)),as.character(row.names(MtaxTiffanian)),as.character(row.names(MtaxTorrejonian)),as.character(row.names(MtaxPuercan)))
OrigUintan<-c(as.character(row.names(MtaxBridgerian)),as.character(row.names(MtaxWasatchian)),as.character(row.names(MtaxClarkforkian)),as.character(row.names(MtaxTiffanian)),as.character(row.names(MtaxTorrejonian)),as.character(row.names(MtaxPuercan)))
OrigDuchesnean<-c(as.character(row.names(MtaxUintan)),as.character(row.names(MtaxBridgerian)),as.character(row.names(MtaxWasatchian)),as.character(row.names(MtaxClarkforkian)),as.character(row.names(MtaxTiffanian)),as.character(row.names(MtaxTorrejonian)),as.character(row.names(MtaxPuercan)))
OrigChadronian<-c(as.character(row.names(MtaxDuchesnean)),as.character(row.names(MtaxUintan)),as.character(row.names(MtaxBridgerian)),as.character(row.names(MtaxWasatchian)),as.character(row.names(MtaxClarkforkian)),as.character(row.names(MtaxTiffanian)),as.character(row.names(MtaxTorrejonian)),as.character(row.names(MtaxPuercan)))
OrigOrellan<-c(as.character(row.names(MtaxChadronian)),as.character(row.names(MtaxDuchesnean)),as.character(row.names(MtaxUintan)),as.character(row.names(MtaxBridgerian)),as.character(row.names(MtaxWasatchian)),as.character(row.names(MtaxClarkforkian)),as.character(row.names(MtaxTiffanian)),as.character(row.names(MtaxTorrejonian)),as.character(row.names(MtaxPuercan)))
OrigWhitneyan<-c(as.character(row.names(MtaxOrellan)),as.character(row.names(MtaxChadronian)),as.character(row.names(MtaxDuchesnean)),as.character(row.names(MtaxUintan)),as.character(row.names(MtaxBridgerian)),as.character(row.names(MtaxWasatchian)),as.character(row.names(MtaxClarkforkian)),as.character(row.names(MtaxTiffanian)),as.character(row.names(MtaxTorrejonian)),as.character(row.names(MtaxPuercan)))
OrigArikareean<-c(as.character(row.names(MtaxWhitneyan)),as.character(row.names(MtaxOrellan)),as.character(row.names(MtaxChadronian)),as.character(row.names(MtaxDuchesnean)),as.character(row.names(MtaxUintan)),as.character(row.names(MtaxBridgerian)),as.character(row.names(MtaxWasatchian)),as.character(row.names(MtaxClarkforkian)),as.character(row.names(MtaxTiffanian)),as.character(row.names(MtaxTorrejonian)),as.character(row.names(MtaxPuercan)))
OrigHemingfordian<-c(as.character(row.names(MtaxArikareean)),as.character(row.names(MtaxWhitneyan)),as.character(row.names(MtaxOrellan)),as.character(row.names(MtaxChadronian)),as.character(row.names(MtaxDuchesnean)),as.character(row.names(MtaxUintan)),as.character(row.names(MtaxBridgerian)),as.character(row.names(MtaxWasatchian)),as.character(row.names(MtaxClarkforkian)),as.character(row.names(MtaxTiffanian)),as.character(row.names(MtaxTorrejonian)),as.character(row.names(MtaxPuercan)))
OrigBarstovian<-c(as.character(row.names(MtaxHemingfordian)),as.character(row.names(MtaxArikareean)),as.character(row.names(MtaxWhitneyan)),as.character(row.names(MtaxOrellan)),as.character(row.names(MtaxChadronian)),as.character(row.names(MtaxDuchesnean)),as.character(row.names(MtaxUintan)),as.character(row.names(MtaxBridgerian)),as.character(row.names(MtaxWasatchian)),as.character(row.names(MtaxClarkforkian)),as.character(row.names(MtaxTiffanian)),as.character(row.names(MtaxTorrejonian)),as.character(row.names(MtaxPuercan)))
OrigClarendonian<-c(as.character(row.names(MtaxBarstovian)),as.character(row.names(MtaxHemingfordian)),as.character(row.names(MtaxArikareean)),as.character(row.names(MtaxWhitneyan)),as.character(row.names(MtaxOrellan)),as.character(row.names(MtaxChadronian)),as.character(row.names(MtaxDuchesnean)),as.character(row.names(MtaxUintan)),as.character(row.names(MtaxBridgerian)),as.character(row.names(MtaxWasatchian)),as.character(row.names(MtaxClarkforkian)),as.character(row.names(MtaxTiffanian)),as.character(row.names(MtaxTorrejonian)),as.character(row.names(MtaxPuercan)))
OrigHemphillian<-c(as.character(row.names(MtaxClarendonian)),as.character(row.names(MtaxBarstovian)),as.character(row.names(MtaxHemingfordian)),as.character(row.names(MtaxArikareean)),as.character(row.names(MtaxWhitneyan)),as.character(row.names(MtaxOrellan)),as.character(row.names(MtaxChadronian)),as.character(row.names(MtaxDuchesnean)),as.character(row.names(MtaxUintan)),as.character(row.names(MtaxBridgerian)),as.character(row.names(MtaxWasatchian)),as.character(row.names(MtaxClarkforkian)),as.character(row.names(MtaxTiffanian)),as.character(row.names(MtaxTorrejonian)),as.character(row.names(MtaxPuercan)))
OrigBlancan<-c(as.character(row.names(MtaxHemphillian)),as.character(row.names(MtaxClarendonian)),as.character(row.names(MtaxBarstovian)),as.character(row.names(MtaxHemingfordian)),as.character(row.names(MtaxArikareean)),as.character(row.names(MtaxWhitneyan)),as.character(row.names(MtaxOrellan)),as.character(row.names(MtaxChadronian)),as.character(row.names(MtaxDuchesnean)),as.character(row.names(MtaxUintan)),as.character(row.names(MtaxBridgerian)),as.character(row.names(MtaxWasatchian)),as.character(row.names(MtaxClarkforkian)),as.character(row.names(MtaxTiffanian)),as.character(row.names(MtaxTorrejonian)),as.character(row.names(MtaxPuercan)))
OrigIrvingtonian<-c(as.character(row.names(MtaxBlancan)),as.character(row.names(MtaxHemphillian)),as.character(row.names(MtaxClarendonian)),as.character(row.names(MtaxBarstovian)),as.character(row.names(MtaxHemingfordian)),as.character(row.names(MtaxArikareean)),as.character(row.names(MtaxWhitneyan)),as.character(row.names(MtaxOrellan)),as.character(row.names(MtaxChadronian)),as.character(row.names(MtaxDuchesnean)),as.character(row.names(MtaxUintan)),as.character(row.names(MtaxBridgerian)),as.character(row.names(MtaxWasatchian)),as.character(row.names(MtaxClarkforkian)),as.character(row.names(MtaxTiffanian)),as.character(row.names(MtaxTorrejonian)),as.character(row.names(MtaxPuercan)))
OrigRancholabrean<-c(as.character(row.names(MtaxIrvingtonian)),as.character(row.names(MtaxBlancan)),as.character(row.names(MtaxHemphillian)),as.character(row.names(MtaxClarendonian)),as.character(row.names(MtaxBarstovian)),as.character(row.names(MtaxHemingfordian)),as.character(row.names(MtaxArikareean)),as.character(row.names(MtaxWhitneyan)),as.character(row.names(MtaxOrellan)),as.character(row.names(MtaxChadronian)),as.character(row.names(MtaxDuchesnean)),as.character(row.names(MtaxUintan)),as.character(row.names(MtaxBridgerian)),as.character(row.names(MtaxWasatchian)),as.character(row.names(MtaxClarkforkian)),as.character(row.names(MtaxTiffanian)),as.character(row.names(MtaxTorrejonian)),as.character(row.names(MtaxPuercan)))
OrigHolocene<-c(as.character(row.names(MtaxRancholabrean)),as.character(row.names(MtaxIrvingtonian)),as.character(row.names(MtaxBlancan)),as.character(row.names(MtaxHemphillian)),as.character(row.names(MtaxClarendonian)),as.character(row.names(MtaxBarstovian)),as.character(row.names(MtaxHemingfordian)),as.character(row.names(MtaxArikareean)),as.character(row.names(MtaxWhitneyan)),as.character(row.names(MtaxOrellan)),as.character(row.names(MtaxChadronian)),as.character(row.names(MtaxDuchesnean)),as.character(row.names(MtaxUintan)),as.character(row.names(MtaxBridgerian)),as.character(row.names(MtaxWasatchian)),as.character(row.names(MtaxClarkforkian)),as.character(row.names(MtaxTiffanian)),as.character(row.names(MtaxTorrejonian)),as.character(row.names(MtaxPuercan)))

#Magnitude of Speciation
Magnitude<-function(x,y){
  for(i in 1:nrow(x))
      newtaxa<-which(row.names(x) %in%  unique(y))
      newtaxa<-row.names(x[-newtaxa, ])
      length(newtaxa)
}

OrigsTorrejonian<-Magnitude(MtaxTorrejonian,OrigTorrejonian) #Number of originating taxa in the Torrejonian
OrigsTiffanian<-Magnitude(MtaxTiffanian,OrigTiffanian) #Number of originating taxa in the Tiffanian
OrigsClarkforkian<-Magnitude(MtaxClarkforkian,OrigClarkforkian) #And so on...
OrigsWasatchian<-Magnitude(MtaxWasatchian,OrigWasatchian)
OrigsBridgerian<-Magnitude(MtaxBridgerian,OrigBridgerian)
OrigsUintan<-Magnitude(MtaxUintan,OrigUintan)
OrigsDuchesnean<-Magnitude(MtaxDuchesnean,OrigDuchesnean)
OrigsChadronian<-Magnitude(MtaxChadronian,OrigChadronian)
OrigsOrellan<-Magnitude(MtaxOrellan,OrigOrellan)
OrigsWhitneyan<-Magnitude(MtaxWhitneyan,OrigWhitneyan)
OrigsArikareean<-Magnitude(MtaxArikareean,OrigArikareean)
OrigsHemingfordian<-Magnitude(MtaxHemingfordian,OrigHemingfordian)
OrigsBarstovian<-Magnitude(MtaxBarstovian,OrigBarstovian)
OrigsClarendonian<-Magnitude(MtaxClarendonian,OrigClarendonian)
OrigsHemphillian<-Magnitude(MtaxHemphillian,OrigHemphillian)
OrigsBlancan<-Magnitude(MtaxBlancan,OrigBlancan)
OrigsIrvingtonian<-Magnitude(MtaxIrvingtonian,OrigIrvingtonian)
OrigsRancholabrean<-Magnitude(MtaxRancholabrean,OrigRancholabrean)
OrigsHolocene<-Magnitude(MtaxHolocene,OrigHolocene)

Num.Origs<-c(OrigsTorrejonian,OrigsTiffanian,OrigsClarkforkian,OrigsWasatchian,OrigsBridgerian,OrigsUintan, #Vector of number originating taxa throughout the Cenozoic
            OrigsDuchesnean,OrigsChadronian,OrigsOrellan,OrigsWhitneyan,OrigsArikareean,OrigsHemingfordian,
            OrigsBarstovian,OrigsClarendonian,OrigsHemphillian,OrigsBlancan,OrigsIrvingtonian,OrigsRancholabrean,
            OrigsHolocene)

#Extinct Species Lists (what exists after the focal time bin?)
ExtPuercan<-c(as.character(row.names(MtaxTorrejonian)),as.character(row.names(MtaxTiffanian)),as.character(row.names(MtaxClarkforkian)),as.character(row.names(MtaxWasatchian)),as.character(row.names(MtaxBridgerian)),as.character(row.names(MtaxUintan)),as.character(row.names(MtaxDuchesnean)),as.character(row.names(MtaxChadronian)),as.character(row.names(MtaxWhitneyan)),as.character(row.names(MtaxArikareean)),as.character(row.names(MtaxHemingfordian)),as.character(row.names(MtaxBarstovian)),as.character(row.names(MtaxClarendonian)),as.character(row.names(MtaxHemphillian)),as.character(row.names(MtaxBlancan)),as.character(row.names(MtaxIrvingtonian)),as.character(row.names(MtaxRancholabrean)),as.character(row.names(MtaxHolocene)))
ExtTorrejonian<-c(as.character(row.names(MtaxTiffanian)),as.character(row.names(MtaxClarkforkian)),as.character(row.names(MtaxWasatchian)),as.character(row.names(MtaxBridgerian)),as.character(row.names(MtaxUintan)),as.character(row.names(MtaxDuchesnean)),as.character(row.names(MtaxChadronian)),as.character(row.names(MtaxWhitneyan)),as.character(row.names(MtaxArikareean)),as.character(row.names(MtaxHemingfordian)),as.character(row.names(MtaxBarstovian)),as.character(row.names(MtaxClarendonian)),as.character(row.names(MtaxHemphillian)),as.character(row.names(MtaxBlancan)),as.character(row.names(MtaxIrvingtonian)),as.character(row.names(MtaxRancholabrean)),as.character(row.names(MtaxHolocene)))
ExtTiffanian<-c(as.character(row.names(MtaxClarkforkian)),as.character(row.names(MtaxWasatchian)),as.character(row.names(MtaxBridgerian)),as.character(row.names(MtaxUintan)),as.character(row.names(MtaxDuchesnean)),as.character(row.names(MtaxChadronian)),as.character(row.names(MtaxWhitneyan)),as.character(row.names(MtaxArikareean)),as.character(row.names(MtaxHemingfordian)),as.character(row.names(MtaxBarstovian)),as.character(row.names(MtaxClarendonian)),as.character(row.names(MtaxHemphillian)),as.character(row.names(MtaxBlancan)),as.character(row.names(MtaxIrvingtonian)),as.character(row.names(MtaxRancholabrean)),as.character(row.names(MtaxHolocene)))
ExtClarkforkian<-c(as.character(row.names(MtaxWasatchian)),as.character(row.names(MtaxBridgerian)),as.character(row.names(MtaxUintan)),as.character(row.names(MtaxDuchesnean)),as.character(row.names(MtaxChadronian)),as.character(row.names(MtaxWhitneyan)),as.character(row.names(MtaxArikareean)),as.character(row.names(MtaxHemingfordian)),as.character(row.names(MtaxBarstovian)),as.character(row.names(MtaxClarendonian)),as.character(row.names(MtaxHemphillian)),as.character(row.names(MtaxBlancan)),as.character(row.names(MtaxIrvingtonian)),as.character(row.names(MtaxRancholabrean)),as.character(row.names(MtaxHolocene)))
ExtWasatchian<-c(as.character(row.names(MtaxBridgerian)),as.character(row.names(MtaxUintan)),as.character(row.names(MtaxDuchesnean)),as.character(row.names(MtaxChadronian)),as.character(row.names(MtaxWhitneyan)),as.character(row.names(MtaxArikareean)),as.character(row.names(MtaxHemingfordian)),as.character(row.names(MtaxBarstovian)),as.character(row.names(MtaxClarendonian)),as.character(row.names(MtaxHemphillian)),as.character(row.names(MtaxBlancan)),as.character(row.names(MtaxIrvingtonian)),as.character(row.names(MtaxRancholabrean)),as.character(row.names(MtaxHolocene)))
ExtBridgerian<-c(as.character(row.names(MtaxUintan)),as.character(row.names(MtaxDuchesnean)),as.character(row.names(MtaxChadronian)),as.character(row.names(MtaxWhitneyan)),as.character(row.names(MtaxArikareean)),as.character(row.names(MtaxHemingfordian)),as.character(row.names(MtaxBarstovian)),as.character(row.names(MtaxClarendonian)),as.character(row.names(MtaxHemphillian)),as.character(row.names(MtaxBlancan)),as.character(row.names(MtaxIrvingtonian)),as.character(row.names(MtaxRancholabrean)),as.character(row.names(MtaxHolocene)))
ExtUintan<-c(as.character(row.names(MtaxDuchesnean)),as.character(row.names(MtaxChadronian)),as.character(row.names(MtaxWhitneyan)),as.character(row.names(MtaxArikareean)),as.character(row.names(MtaxHemingfordian)),as.character(row.names(MtaxBarstovian)),as.character(row.names(MtaxClarendonian)),as.character(row.names(MtaxHemphillian)),as.character(row.names(MtaxBlancan)),as.character(row.names(MtaxIrvingtonian)),as.character(row.names(MtaxRancholabrean)),as.character(row.names(MtaxHolocene)))
ExtDuchesnean<-c(as.character(row.names(MtaxChadronian)),as.character(row.names(MtaxWhitneyan)),as.character(row.names(MtaxArikareean)),as.character(row.names(MtaxHemingfordian)),as.character(row.names(MtaxBarstovian)),as.character(row.names(MtaxClarendonian)),as.character(row.names(MtaxHemphillian)),as.character(row.names(MtaxBlancan)),as.character(row.names(MtaxIrvingtonian)),as.character(row.names(MtaxRancholabrean)),as.character(row.names(MtaxHolocene)))
ExtChadronian<-c(as.character(row.names(MtaxOrellan)),as.character(row.names(MtaxWhitneyan)),as.character(row.names(MtaxArikareean)),as.character(row.names(MtaxHemingfordian)),as.character(row.names(MtaxBarstovian)),as.character(row.names(MtaxClarendonian)),as.character(row.names(MtaxHemphillian)),as.character(row.names(MtaxBlancan)),as.character(row.names(MtaxIrvingtonian)),as.character(row.names(MtaxRancholabrean)),as.character(row.names(MtaxHolocene)))
ExtOrellan<-c(as.character(row.names(MtaxWhitneyan)),as.character(row.names(MtaxArikareean)),as.character(row.names(MtaxHemingfordian)),as.character(row.names(MtaxBarstovian)),as.character(row.names(MtaxClarendonian)),as.character(row.names(MtaxHemphillian)),as.character(row.names(MtaxBlancan)),as.character(row.names(MtaxIrvingtonian)),as.character(row.names(MtaxRancholabrean)),as.character(row.names(MtaxHolocene)))
ExtWhitneyan<-c(as.character(row.names(MtaxArikareean)),as.character(row.names(MtaxHemingfordian)),as.character(row.names(MtaxBarstovian)),as.character(row.names(MtaxClarendonian)),as.character(row.names(MtaxHemphillian)),as.character(row.names(MtaxBlancan)),as.character(row.names(MtaxIrvingtonian)),as.character(row.names(MtaxRancholabrean)),as.character(row.names(MtaxHolocene)))
ExtArikareean<-c(as.character(row.names(MtaxHemingfordian)),as.character(row.names(MtaxBarstovian)),as.character(row.names(MtaxClarendonian)),as.character(row.names(MtaxHemphillian)),as.character(row.names(MtaxBlancan)),as.character(row.names(MtaxIrvingtonian)),as.character(row.names(MtaxRancholabrean)),as.character(row.names(MtaxHolocene)))
ExtHemingfordian<-c(as.character(row.names(MtaxBarstovian)),as.character(row.names(MtaxClarendonian)),as.character(row.names(MtaxHemphillian)),as.character(row.names(MtaxBlancan)),as.character(row.names(MtaxIrvingtonian)),as.character(row.names(MtaxRancholabrean)),as.character(row.names(MtaxHolocene)))
ExtBarstovian<-c(as.character(row.names(MtaxClarendonian)),as.character(row.names(MtaxHemphillian)),as.character(row.names(MtaxBlancan)),as.character(row.names(MtaxIrvingtonian)),as.character(row.names(MtaxRancholabrean)),as.character(row.names(MtaxHolocene)))
ExtClarendonian<-c(as.character(row.names(MtaxHemphillian)),as.character(row.names(MtaxBlancan)),as.character(row.names(MtaxIrvingtonian)),as.character(row.names(MtaxRancholabrean)),as.character(row.names(MtaxHolocene)))
ExtHemphillian<-c(as.character(row.names(MtaxBlancan)),as.character(row.names(MtaxIrvingtonian)),as.character(row.names(MtaxRancholabrean)),as.character(row.names(MtaxHolocene)))
ExtBlancan<-c(as.character(row.names(MtaxIrvingtonian)),as.character(row.names(MtaxRancholabrean)),as.character(row.names(MtaxHolocene)))
ExtIrvingtonian<-c(as.character(row.names(MtaxRancholabrean)),as.character(row.names(MtaxHolocene)))
ExtRancholabrean<-as.character(row.names(MtaxHolocene))
ExtHolocene<-as.character(Survivors$Genus)

#Extinction Rates
ExtsPuercan<-Magnitude(MtaxPuercan,ExtPuercan)
ExtsTorrejonian<-Magnitude(MtaxTorrejonian,ExtTorrejonian)
ExtsTiffanian<-Magnitude(MtaxTiffanian,ExtTiffanian)
ExtsClarkforkian<-Magnitude(MtaxClarkforkian,ExtClarkforkian)
ExtsWasatchian<-Magnitude(MtaxWasatchian,ExtWasatchian)
ExtsBridgerian<-Magnitude(MtaxBridgerian,ExtBridgerian)
ExtsUintan<-Magnitude(MtaxUintan,ExtUintan)
ExtsDuchesnean<-Magnitude(MtaxDuchesnean,ExtDuchesnean)
ExtsChadronian<-Magnitude(MtaxChadronian,ExtChadronian)
ExtsOrellan<-Magnitude(MtaxOrellan,ExtOrellan)
ExtsWhitneyan<-Magnitude(MtaxWhitneyan,ExtWhitneyan)
ExtsArikareean<-Magnitude(MtaxArikareean,ExtArikareean)
ExtsHemingfordian<-Magnitude(MtaxHemingfordian,ExtHemingfordian)
ExtsBarstovian<-Magnitude(MtaxBarstovian,ExtBarstovian)
ExtsClarendonian<-Magnitude(MtaxClarendonian,ExtClarendonian)
ExtsHemphillian<-Magnitude(MtaxHemphillian,ExtHemphillian)
ExtsBlancan<-Magnitude(MtaxBlancan,ExtBlancan)
ExtsIrvingtonian<-Magnitude(MtaxIrvingtonian,ExtIrvingtonian)
ExtsRancholabrean<-Magnitude(MtaxRancholabrean,ExtRancholabrean)
ExtsHolocene<-Magnitude(MtaxHolocene,ExtHolocene)

Num.Exts<-c(ExtsPuercan,ExtsTorrejonian,ExtsTiffanian,ExtsClarkforkian,ExtsWasatchian,ExtsBridgerian,ExtsUintan, #Vector of number taxa going extinct throughout the Cenozoic
            ExtsDuchesnean,ExtsChadronian,ExtsOrellan,ExtsWhitneyan,ExtsArikareean,ExtsHemingfordian,
            ExtsBarstovian,ExtsClarendonian,ExtsHemphillian,ExtsBlancan,ExtsIrvingtonian,ExtsRancholabrean,
            ExtsHolocene)

#Create Morig Matrices
Morig<-function(x,y,z){

  Setup <- function(x){
    taxa<-x$genus #list of each occurrence's genus from this interval
    taxa.names<-levels(taxa) #what are the unique genera occurring in this interval?
    numtaxa<-length(taxa.names) #how many are there?
    outTable<-array(0,dim=c(numtaxa,numtaxa)) #create an n x n array of these genera, assign a null value of 0
    rownames(outTable)<-taxa.names #taxa names make the rows
    colnames(outTable)<-taxa.names #taxa names make the columns
    outTable<-outTable
  }

  for (i in 1:numtaxa){ #for each genus in the interval
    newtaxa<-which(row.names(y) %in%  unique(z)) #which genera (as row numbers) occurring in the current interval did not exist in the previous interval?
    newtaxa<-row.names(y[-newtaxa, ]) #what are the names of these genera?
    outTable[newtaxa,newtaxa]<-1 #add these members to their position in the array made above, give them a value of 1
   # outTable[lower.tri(outTable, diag=TRUE)]<-NA #remove the lower half of the matrix
    outTable<-outTable #save result
    
  } #for each genus in the interval
}

MorigTorrejonian<-Morig(Torrejonian,MtaxTorrejonian,OrigTorrejonian)
MorigTiffanian<-Morig(Tiffanian,MtaxTiffanian,OrigTiffanian)
MorigClarkforkian<-Morig(Clarkforkian,MtaxClarkforkian,OrigClarkforkian)
MorigWasatchian<-Morig(Wasatchian,MtaxWasatchian,OrigWasatchian)
MorigBridgerian<-Morig(Bridgerian,MtaxBridgerian,OrigBridgerian)
MorigUintan<-Morig(Uintan,MtaxUintan,OrigUintan)
MorigDuchesnean<-Morig(Duchesnean,MtaxDuchesnean,OrigDuchesnean)
MorigChadronian<-Morig(Chadronian,MtaxChadronian,OrigChadronian)
MorigOrellan<-Morig(Orellan,MtaxOrellan,OrigOrellan)
MorigWhitneyan<-Morig(Whitneyan,MtaxWhitneyan,OrigWhitneyan)
MorigArikareean<-Morig(Arikareean,MtaxArikareean,OrigArikareean)
MorigHemingfordian<-Morig(Hemingfordian,MtaxHemingfordian,OrigHemingfordian)
MorigBarstovian<-Morig(Barstovian,MtaxBarstovian,OrigBarstovian)
MorigClarendonian<-Morig(Clarendonian,MtaxClarendonian,OrigClarendonian)
MorigHemphillian<-Morig(Hemphillian,MtaxHemphillian,OrigHemphillian)
MorigBlancan<-Morig(Blancan,MtaxBlancan,OrigBlancan)
MorigIrvingtonian<-Morig(Irvingtonian,MtaxIrvingtonian,OrigIrvingtonian)
MorigRancholabrean<-Morig(Rancholabrean,MtaxRancholabrean,OrigRancholabrean)
MorigHolocene<-Morig(Holocene,MtaxHolocene,OrigHolocene)


#Create Mext Matrices
Mext<-function(x,y,z){
  
  for(i in 1:nrow(x)){ #for each occurrence
    
    taxa<-x$genus #list of each occurrence's genus from this interval
    taxa.names<-levels(taxa) #what are the unique genera occurring in this interval?
    numtaxa<-length(taxa.names) #how many are there?
    outTable<-array(0,dim=c(numtaxa,numtaxa)) #create an n x n array of these genera, assign a null value of 0
    rownames(outTable)<-taxa.names #taxa names make the rows
    colnames(outTable)<-taxa.names #taxa names make the columns
    
  for (i in 1:numtaxa){ #for each genus in the interval
    
    extincttaxa<-which(row.names(y) %in%  unique(z)) #which genera (as row numbers) occurring in the current interval do not exist in the following interval?
    extincttaxa<-row.names(y[-extincttaxa, ]) #what are the names of these genera?
    outTable[extincttaxa,extincttaxa]<-1 #add these members to their position in the array made above, give them a value of 1
    # outTable[lower.tri(outTable, diag=TRUE)]<-NA #remove the lower half of the matrix
    outTable<-outTable #save result
    
  } #for each genus in the interval
  } #for each occurrence
}

MextPuercan<-Mext(Puercan,MtaxPuercan,ExtPuercan)
MextTorrejonian<-Mext(Torrejonian,MtaxTorrejonian,ExtTorrejonian)
MextTiffanian<-Mext(Tiffanian,MtaxTiffanian,ExtTiffanian)
MextClarkforkian<-Mext(Clarkforkian,MtaxClarkforkian,ExtClarkforkian)
MextWasatchian<-Mext(Wasatchian,MtaxWasatchian,ExtWasatchian)
MextBridgerian<-Mext(Bridgerian,MtaxBridgerian,ExtBridgerian)
MextUintan<-Mext(Uintan,MtaxUintan,ExtUintan)
MextDuchesnean<-Mext(Duchesnean,MtaxDuchesnean,ExtDuchesnean)
MextChadronian<-Mext(Chadronian,MtaxChadronian,ExtChadronian)
MextOrellan<-Mext(Orellan,MtaxOrellan,ExtOrellan)
MextWhitneyan<-Mext(Whitneyan,MtaxWhitneyan,ExtWhitneyan)
MextArikareean<-Mext(Arikareean,MtaxArikareean,ExtArikareean)
MextHemingfordian<-Mext(Hemingfordian,MtaxHemingfordian,ExtHemingfordian)
MextBarstovian<-Mext(Barstovian,MtaxBarstovian,ExtBarstovian)
MextClarendonian<-Mext(Clarendonian,MtaxClarendonian,ExtClarendonian)
MextHemphillian<-Mext(Hemphillian,MtaxHemphillian,ExtHemphillian)
MextBlancan<-Mext(Blancan,MtaxBlancan,ExtBlancan)
MextIrvingtonian<-Mext(Irvingtonian,MtaxIrvingtonian,ExtIrvingtonian)
MextRancholabrean<-Mext(Rancholabrean,MtaxRancholabrean,ExtRancholabrean)
MextHolocene<-Mext(Holocene,MtaxHolocene,ExtHolocene)

###############################
###############################
#Matrix Correlations
MORIG<-c(mean(cor(MorigTorrejonian,MtaxTorrejonian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MorigTiffanian,MtaxTiffanian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MorigClarkforkian,MtaxClarkforkian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MorigWasatchian,MtaxWasatchian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MorigBridgerian,MtaxBridgerian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MorigUintan,MtaxUintan,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MorigDuchesnean,MtaxDuchesnean,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MorigChadronian,MtaxChadronian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MorigOrellan,MtaxOrellan,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MorigWhitneyan,MtaxWhitneyan,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MorigArikareean,MtaxArikareean,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MorigHemingfordian,MtaxHemingfordian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MorigBarstovian,MtaxBarstovian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MorigClarendonian,MtaxClarendonian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MorigHemphillian,MtaxHemphillian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MorigBlancan,MtaxBlancan,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MorigIrvingtonian,MtaxIrvingtonian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MorigRancholabrean,MtaxRancholabrean,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MorigHolocene,MtaxHolocene,use="pairwise.complete.obs"),na.rm=TRUE))

MEXT<-c(mean(cor(MextPuercan,MtaxPuercan,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextTorrejonian,MtaxTorrejonian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextTiffanian,MtaxTiffanian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextClarkforkian,MtaxClarkforkian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextWasatchian,MtaxWasatchian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextBridgerian,MtaxBridgerian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextUintan,MtaxUintan,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextDuchesnean,MtaxDuchesnean,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextChadronian,MtaxChadronian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextOrellan,MtaxOrellan,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextWhitneyan,MtaxWhitneyan,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextArikareean,MtaxArikareean,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextHemingfordian,MtaxHemingfordian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextBarstovian,MtaxBarstovian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextClarendonian,MtaxClarendonian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextHemphillian,MtaxHemphillian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextBlancan,MtaxBlancan,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextIrvingtonian,MtaxIrvingtonian,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextRancholabrean,MtaxRancholabrean,use="pairwise.complete.obs"),na.rm=TRUE),
        mean(cor(MextHolocene,MtaxHolocene,use="pairwise.complete.obs"),na.rm=TRUE))
        

############## Monte Carlo ##############
######### BEGIN HERE FOR A QUICKER START IF MATRICES ALREADY EXIST ################

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





NALMAs <- c("Puercan", "Torrejonian", "Tiffanian", "Clarkforkian",
            "Wasatchian", "Bridgerian", "Uintan", "Duchesnean",
            "Chadronian", "Orellan", "Whitneyan", "Arikareean",
            "Hemingfordian", "Barstovian", "Clarendonian", "Hemphillian",
            "Blancan","Irvingtonian", "Rancholabrean","Holocene")
l.NALMAs <- length(NALMAs)
iterations <- 10

res.matrix <- matrix(NA,nrow=iterations,ncol=l.NALMAs)
colnames(res.matrix)<-NALMAs

RCL.Randomization <- function(x){ #x is the Mtax matrix (e.g., MtaxPuercan), i is the name of the NALMA (e.g., "Puercan")
  for (iter in 1:iterations){
    
    Ne <- NALMA.BINS[1,8] #Number of extinctions in the focal bin
    ne <- (2*Ne) + nrow(x) #how many taxa went extinct in this interval? (number of 1s to put in the matrix)
    # ne <- Ne #if using half matrix
    n <- nrow(x) #Total number of rows and columns for the (full) matrix
    outTable <- matrix(0,n,n) #set up the matrix
    # outTable[lower.tri(outTable, diag=TRUE)]<-NA #remove the lower half of the matrix
    outTable[sample(n*n, ne)] <- 1 #insert that many 1s in the matrix in random locations
    R.cl <- mean(cor(outTable,x,use="pairwise.complete.obs"),na.rm=TRUE) #calculate a null Rcl
    res.matrix[iter,1] <- R.cl #Insert calculated random value of clustering index into results matrix
    res.matrix <- res.matrix
  }
 res.matrix<-res.matrix #save result
}

RCL.Randomization(MtaxPuercan)

#Moran's I
mu <- sum(colMeans(MextWasatchian))/ncol(MextWasatchian) #xbar in Moran's statistic - the average of the matrix of interest

w<-row.names(MtaxPuercan)
x<-row.names(MtaxTorrejonian)
y<-row.names(MtaxTiffanian)
z<-row.names(MtaxClarkforkian)
Nfl.x<-setdiff(x,union(w,y))
Nfl.y<-setdiff(y,union(x,z))
Nbt<-Reduce(intersect,list(x,y,z))

Nb.x <- Reduce(intersect, list(Nbt, Nfl.y))