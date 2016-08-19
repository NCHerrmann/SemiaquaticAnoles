lizards<-read.csv(file="Lizards.csv", header=TRUE)
habitats<-read.csv(file="Habitats.csv", header=TRUE)

#How many lizards in total were on each substrate type?

lizards=subset(lizards,lizards$Month=="Jan" & lizards$Repeat=="N")

liz=subset(lizards,lizards$Site=="400" | lizards$Site=="800")
liz=subset(lizards,lizards$Site=="CULV" | lizards$Site=="DOWN" | lizards$Site=="EBRA")

length(liz$Substrate[liz$Substrate=="W"]) #30,17
length(liz$Substrate[liz$Substrate=="L"]) #4,2
length(liz$Substrate[liz$Substrate=="B"]) #2,21
length(liz$Substrate[liz$Substrate=="G"]) #7,13

counts=c(30,4,2,7)
counts=c(17,2,21,13)

sum(counts)
prop=counts/sum(counts)

#Do male, female, and juvenille lizards differ in their substrate use?

library(MASS)
#data=subset(lizards,lizards$Site=="CULV" & (lizards$Sex=="M" |  lizards$Sex=="F" | lizards$Sex=="J"))
data=subset(lizards,(lizards$Site=="400" | lizards$Site=="800") & (lizards$Sex=="M" |  lizards$Sex=="F" | lizards$Sex=="J"))
tbl=table(data$Substrate,data$Sex)
tbl=tbl[,-1]
tbl
chisq.test(tbl)


#Are plots within La Selva different from those at Las Cruces in their substrate availability?

data=matrix(0,4,1)

habdata=subset(habitats,habitats$Site=="EBRA")
x=c(66,74,84,98,100,114)

sub=matrix(0,4,length(x))
for(i in 1:length(x)){
  W=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="W"]);
  L=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="L"]);
  B=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="B"]);
  G=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="G"]);
  prop=c(W,L,B,G)/sum(W,L,B,G);
  sub[,i]=prop
}
data=cbind(data,sub)

habdata=subset(habitats,habitats$Site=="CULV")
x=c(6,16,26,32,48,52,68,70,82,98,100,112)

sub=matrix(0,4,length(x))
for(i in 1:length(x)){
  W=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="W"]);
  L=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="L"]);
  B=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="B"]);
  G=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="G"]);
  prop=c(W,L,B,G)/sum(W,L,B,G);
  sub[,i]=prop
}
data=cbind(data,sub)

habdata=subset(habitats,habitats$Site=="DOWN")
x=c(6,16,26,32,48,52)

sub=matrix(0,4,length(x))
for(i in 1:length(x)){
  W=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="W"]);
  L=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="L"]);
  B=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="B"]);
  G=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="G"]);
  prop=c(W,L,B,G)/sum(W,L,B,G);
  sub[,i]=prop
}
data=cbind(data,sub)

habdata=subset(habitats,habitats$Site=="400")
x=c(0,18,24,38,42,58,64,78,84,90,108,112,128,138)

sub=matrix(0,4,length(x))
for(i in 1:length(x)){
  W=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="W"]);
  L=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="L"]);
  B=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="B"]);
  G=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="G"]);
  prop=c(W,L,B,G)/sum(W,L,B,G);
  sub[,i]=prop
}
data=cbind(data,sub)

habdata=subset(habitats,habitats$Site=="800")
x=c(2,12,26,38,46,58,66,78,102,116)

sub=matrix(0,4,length(x))
for(i in 1:length(x)){
  W=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="W"]);
  L=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="L"]);
  B=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="B"]);
  G=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="G"]);
  prop=c(W,L,B,G)/sum(W,L,B,G);
  sub[,i]=prop
}
data=cbind(data,sub)

cruces=data[,2:25]
selva=data[,26:49]

#FIGURE 2 - Comparison of sites

se=function(x){return(sqrt(var(x)/length(x)))}
t=qt(.975,23)

seLS=as.numeric()
seLC=as.numeric()
barmax=as.numeric()
barmin=as.numeric()
for(i in 1:4){
  seLS[i]=se(selva[i,])
  seLC[i]=se(cruces[i,])
  barmax=c(barmax,(mean(selva[i,])+t*seLS[i]))
  barmax=c(barmax,(mean(cruces[i,])+t*seLC[i]))
  barmin=c(barmin,(mean(selva[i,])-t*seLS[i]))
  barmin=c(barmin,(mean(cruces[i,])-t*seLC[i]))
}

LC =c(mean(cruces[1,]),mean(cruces[2,]),mean(cruces[3,]),mean(cruces[4,]))
LS =c(mean(selva[1,]),mean(selva[2,]),mean(selva[3,]),mean(selva[4,]))
habs=rbind(LS,LC)
colnames(habs)=c("Woody","Leafy","Rocky","Ground")
bp=barplot(habs,cex.axis=0.9, cex.names=0.8,col=c("gray","white"), xlab="Substrate", ylab="Relative Availability",ylim=c(0,0.8),beside=TRUE)
legend(1,0.8,cex=0.8,legend=c("La Selva (A. oxylophus)","Las Cruces (A. aquaticus)"),fill=c("gray","white"))
arrows(bp, barmin, bp,barmax, lwd = 1.5, angle = 90, code = 3, length = 0.05)
abline(0,0)
text(2,0.4,"*")
text(8,0.4,"***")
text(11,0.75,"***")

dev.copy2pdf(file="Figure1 Habitat Availability Between Forests")


#T-tests for whether sites differ in percentage of available substrates

t1a=(asin(sqrt(cruces[1,])))
t1b=(asin(sqrt(selva[1,])))
t2a=(asin(sqrt(cruces[2,])))
t2b=(asin(sqrt(selva[2,])))
t3a=(asin(sqrt(cruces[3,])))
t3b=(asin(sqrt(selva[3,])))
t4a=(asin(sqrt(cruces[4,])))
t4b=(asin(sqrt(selva[4,])))

library(nortest)
ad.test(t1a)
ad.test(t1b)
ad.test(t2a)
ad.test(t2b)
ad.test(t3a)
ad.test(t3b)
ad.test(t4a)
ad.test(t4b)
wtest=t.test(t1a,t1b,var.equal = TRUE)
ltest=t.test(t2a,t2b,var.equal = TRUE)
rtest=t.test(t3a,t3b,var.equal = TRUE)
gtest=t.test(t4a,t4b,var.equal = TRUE)

#Sequential Bonferroni

p=c(wtest$p.value,ltest$p.value,rtest$p.value,gtest$p.value)

p.adjust(p, method = "holm", n = length(p))


#t-test for whether the streams in different forests differ in width

SITES<-read.csv(file="SITES.csv", header=TRUE)
cruces=subset(SITES,SITES$Station=="LC")
selva=subset(SITES,SITES$Station=="LS")

t.test(cruces$Width,selva$Width)

#These line is needed for all regressions and simulations that follow (remove lizards from streams EBRA and DOWN)
lizards=subset(lizards,lizards$Month=="Jan" & lizards$Repeat=="N" & (lizards$Site=="400" | lizards$Site=="800" | lizards$Site=="CULV"))
habitats=subset(habitats,!(habitats$Site=="800" & (habitats$Plot=="102" |habitats$Plot=="116")))

#Within a stream, are lizards more common within stream segments based on the % of ground, or of their "preferred" substrate
#Each lizard will ge assigned to its closest plot, then I will test for the relationship

#site CULV

data=subset(lizards,lizards$Site=="CULV")
habdata=subset(habitats,habitats$Site=="CULV")

x=c(6,16,26,32,48,52,68,70,82,98,100,112)
ground=as.numeric()
boulder=as.numeric()
wood=as.numeric()
for(i in 1:length(x)){ #makes a vector with the percentage of ground in each plot
  ground[i]=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="G"])/sum(habdata$SA[habdata$Plot==x[i]])
}
for(i in 1:length(x)){ #makes a vector with the percentage of boulder in each plot
  boulder[i]=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="B"])/sum(habdata$SA[habdata$Plot==x[i]])
}
for(i in 1:length(x)){ #makes a vector with the percentage of wood in each plot
  wood[i]=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="W"])/sum(habdata$SA[habdata$Plot==x[i]])
}

y=data$Marker
lizmarker=as.numeric()
for(j in 1:length(y)){ #creates a vector that assigns each lizard to the ground percentage of its closest plot
  lizmarker[j]=x[which(abs(x-y[j])==min(abs(y[j]-x)))]
}

distance=rep(0,length(lizmarker)) #this will show the distance of the lizard to the nearest plot
perground=rep(0,length(lizmarker)) #this adds the percentage ground to the data
perboulder=rep(0,length(lizmarker)) #this adds the percentage boulder to the data
for(i in 1:length(lizmarker)){
  perground[i]=ground[which(x==lizmarker[i])];
  perboulder[i]=boulder[which(x==lizmarker[i])];
  distance[i]=min(c(abs(y[i]-lizmarker[i]),abs(y[i]+2-lizmarker[i])))
}
data=cbind(data,lizmarker,perground,perboulder,distance)

lizplot=rep(0,length(ground))
for(i in 1:length(ground)){ #creates a vector for the count of lizards in each bin
  lizplot[i]=length(data$perground[data$perground==ground[i]])
}


fit=glm(lizplot~ground, family="poisson")
summary(fit)
fit=glm(lizplot~boulder, family="poisson")
summary(fit)
exp(coef(fit)[2])

mean(distance)
length(lizmarker)


#Now for site STR400

data=subset(lizards,lizards$Site=="400")
habdata=subset(habitats,habitats$Site=="400")

x=c(0,18,24,38,42,58,64,78,84,90,108,112,128,138)
ground=as.numeric()
wood=as.numeric()
for(i in 1:length(x)){ #makes a vector with the percentage of ground in each plot
  ground[i]=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="G"])/sum(habdata$SA[habdata$Plot==x[i]])
}
for(i in 1:length(x)){ #makes a vector with the percentage of wood in each plot
  wood[i]=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="W"])/sum(habdata$SA[habdata$Plot==x[i]])
}

y=data$Marker
lizmarker=as.numeric()
for(j in 1:length(y)){ #creates a vector that assigns each lizard to the ground percentage of its closest plot
  lizmarker[j]=x[which(abs(x-y[j])==min(abs(y[j]-x)))]
}

distance=rep(0,length(lizmarker)) #this will show the distance of the lizard to the nearest plot
perground=rep(0,length(lizmarker)) #this adds the percentage ground to the data
perwood=rep(0,length(lizmarker)) #this adds the percentage boulder to the data
for(i in 1:length(lizmarker)){
  perground[i]=ground[which(x==lizmarker[i])];
  perwood[i]=wood[which(x==lizmarker[i])];
  distance[i]=min(c(abs(y[i]-lizmarker[i]),abs(y[i]+2-lizmarker[i])))
}
data=cbind(data,lizmarker,perground,perwood,distance)

lizplot=rep(0,length(ground))
for(i in 1:length(ground)){ #creates a vector for the count of lizards in each bin
  lizplot[i]=length(data$perground[data$perground==ground[i]])
}

mean(lizplot)
var(lizplot)

fit=glm(lizplot~ground, family="poisson")
summary(fit)
fit=glm(lizplot~wood, family="poisson")
summary(fit)

mean(residuals(fit))
plot(fit)

plot(lizplot~ground)
abline(fit)

mean(distance)
length(lizmarker)


#site STR800

data=subset(lizards,lizards$Site=="800")
habdata=subset(habitats,habitats$Site=="800")

x=c(2,12,26,38,46,58,66,78)
ground=as.numeric()
wood=as.numeric()
for(i in 1:length(x)){ #makes a vector with the percentage of ground in each plot
  ground[i]=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="G"])/sum(habdata$SA[habdata$Plot==x[i]])
}
for(i in 1:length(x)){ #makes a vector with the percentage of wood in each plot
  wood[i]=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="W"])/sum(habdata$SA[habdata$Plot==x[i]])
}

y=data$Marker
lizmarker=as.numeric()
for(j in 1:length(y)){ #creates a vector that assigns each lizard to the ground percentage of its closest plot
  lizmarker[j]=x[which(abs(x-y[j])==min(abs(y[j]-x)))]
}

distance=rep(0,length(lizmarker)) #this will show the distance of the lizard to the nearest plot
perground=rep(0,length(lizmarker)) #this adds the percentage ground to the data
perwood=rep(0,length(lizmarker)) #this adds the percentage boulder to the data
for(i in 1:length(lizmarker)){
  perground[i]=ground[which(x==lizmarker[i])];
  perwood[i]=wood[which(x==lizmarker[i])];
  distance[i]=min(c(abs(y[i]-lizmarker[i]),abs(y[i]+2-lizmarker[i])))
}
data=cbind(data,lizmarker,perground,perwood,distance)

lizplot=rep(0,length(ground))
for(i in 1:length(ground)){ #creates a vector for the count of lizards in each bin
  lizplot[i]=length(data$perground[data$perground==ground[i]])
}

fit=glm(lizplot~ground, family="poisson")
summary(fit)
fit=glm(lizplot~wood, family="poisson")
summary(fit)

plot(lizplot~ground)
abline(fit)

mean(distance)
length(lizmarker)





#Here are multinomial-style simulations with each simulated lizard assigned to the relative substrate probabilities in its nearest plot
#need vector of substrate availability for each plot
#then find number of lizards assigned to each plot and run sim
#add matrices to get total counts across the stream
#compare the sim number to the number of lizards actually perched on different substrates

data=subset(lizards,lizards$Site=="CULV")
habdata=subset(habitats,habitats$Site=="CULV")

liz<-c(sum(data$Substrate=="W"),sum(data$Substrate=="L"),sum(data$Substrate=="B"),sum(data$Substrate=="G"))
pliz<-liz/sum(liz) #W,L,B,G

x=c(6,16,26,32,48,52,68,70,82,98,100,112)
y=data$Marker
lizmarker=as.numeric()
for(j in 1:length(y)){ #creates a vector that assigns each lizard to its closest plot
  lizmarker[j]=x[which(abs(x-y[j])==min(abs(y[j]-x)))]
}

data=cbind(data,lizmarker)

lizplot=rep(0,length(x))
for(i in 1:length(x)){ #creates a vector lizplot for the count of lizards in each plot
  lizplot[i]=length(data$lizmarker[data$lizmarker==x[i]])
}

sim=matrix(0,4,20000)
EV=rep(0,4)
psub=rep(0,4)
for(i in 1:length(x)){
  W=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="W"]);
  L=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="L"]);
  B=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="B"]);
  G=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="G"]);
  prop=c(W,L,B,G)/sum(W,L,B,G);
  sim=sim+rmultinom(20000,lizplot[i],prop);
  EV=EV+prop*lizplot[i]
  DivEV=abs(EV-liz)
  psub=psub+prop*(lizplot[i]/sum(lizplot))
}



simg<-sim[4,]
hist(simg, breaks=100)
length(simg[simg<=EV[4]-DivEV[4]]) #P-value for ground

simb<-sim[3,]
hist(simb, breaks=100)
length(simb[simb>=EV[3]+DivEV[3]]) #P-value for rocks


#site STR400

data=subset(lizards,lizards$Site=="400")
habdata=subset(habitats,habitats$Site=="400")

liz<-c(sum(data$Substrate=="W"),sum(data$Substrate=="L"),sum(data$Substrate=="B"),sum(data$Substrate=="G"))
pliz<-liz/sum(liz) #W,L,B,G

x=c(0,18,24,38,42,58,64,78,84,90,108,112,128,138)
y=data$Marker
lizmarker=as.numeric()
for(j in 1:length(y)){ #creates a vector that assigns each lizard to its closest plot
  lizmarker[j]=x[which(abs(x-y[j])==min(abs(y[j]-x)))]
}

data=cbind(data,lizmarker)

lizplot=rep(0,length(x))
for(i in 1:length(x)){ #creates a vector lizplot for the count of lizards in each plot
  lizplot[i]=length(data$lizmarker[data$lizmarker==x[i]])
}

sim=matrix(0,4,20000)
EV=rep(0,4)
psub=rep(0,4)
for(i in 1:length(x)){
  W=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="W"]);
  L=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="L"]);
  B=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="B"]);
  G=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="G"]);
  prop=c(W,L,B,G)/sum(W,L,B,G);
  sim=sim+rmultinom(20000,lizplot[i],prop);
  EV=EV+prop*lizplot[i]
  DivEV=abs(EV-liz)
  psub=psub+prop*(lizplot[i]/sum(lizplot))
}

siml<-sim[2,]
hist(siml, breaks=100)
length(siml[(siml<=EV[2]-DivEV[2]) | (siml>=EV[2]+DivEV[2])])

simg<-sim[4,]
hist(simg, breaks=100)
length(simg[simg<=EV[4]-DivEV[4]]) #P-value for ground

simw<-sim[1,]
hist(simw, breaks=100)
length(simg[simw>=EV[1]+DivEV[1]]) #P-value for wood


#site STR800

data=subset(lizards,lizards$Site=="800")
habdata=subset(habitats,habitats$Site=="800")

liz<-c(sum(data$Substrate=="W"),sum(data$Substrate=="L"),sum(data$Substrate=="B"),sum(data$Substrate=="G"))
pliz<-liz/sum(liz) #W,L,B,G

x=c(2,12,26,38,46,58,66,78)
y=data$Marker
lizmarker=as.numeric()
for(j in 1:length(y)){ #creates a vector that assigns each lizard to its closest plot
  lizmarker[j]=x[which(abs(x-y[j])==min(abs(y[j]-x)))]
}

data=cbind(data,lizmarker)

lizplot=rep(0,length(x))
for(i in 1:length(x)){ #creates a vector lizplot for the count of lizards in each plot
  lizplot[i]=length(data$lizmarker[data$lizmarker==x[i]])
}

sim=matrix(0,4,20000)
EV=rep(0,4)
psub=rep(0,4)
for(i in 1:length(x)){
  W=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="W"]);
  L=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="L"]);
  B=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="B"]);
  G=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="G"]);
  prop=c(W,L,B,G)/sum(W,L,B,G);
  sim=sim+rmultinom(20000,lizplot[i],prop);
  EV=EV+prop*lizplot[i]
  DivEV=abs(EV-liz)
  psub=psub+prop*(lizplot[i]/sum(lizplot))
}

siml<-sim[2,]
hist(siml, breaks=100)
length(siml[(siml<=EV[2]-DivEV[2]) | (siml>=EV[2]+DivEV[2])])

simg<-sim[4,]
hist(simg, breaks=100)
length(simg[simg<=EV[4]-DivEV[4]]) #P-value for ground

simw<-sim[1,]
hist(simw, breaks=100)
length(simg[simw>=EV[1]+DivEV[1]]) #P-value for wood

#Round 2 simulations, with one substrate removed at a time

#without ground, site CULV

data=subset(lizards,lizards$Site=="CULV" & !lizards$Substrate=="G")
habdata=subset(habitats,habitats$Site=="CULV")

liz<-c(sum(data$Substrate=="W"),sum(data$Substrate=="L"),sum(data$Substrate=="B"))
pliz<-liz/sum(liz) #W,L,B

x=c(6,16,26,32,48,52,68,70,82,98,100,112)
y=data$Marker
lizmarker=as.numeric()
for(j in 1:length(y)){ #creates a vector that assigns each lizard to its closest plot
  lizmarker[j]=x[which(abs(x-y[j])==min(abs(y[j]-x)))]
}

data=cbind(data,lizmarker)

lizplot=rep(0,length(x))
for(i in 1:length(x)){ #creates a vector lizplot for the count of lizards in each plot
  lizplot[i]=length(data$lizmarker[data$lizmarker==x[i]])
}

sim=matrix(0,3,20000)
EV=rep(0,3)
psub=rep(0,3)
for(i in 1:length(x)){
  W=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="W"]);
  L=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="L"]);
  B=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="B"]);
  prop=c(W,L,B)/sum(W,L,B);
  sim=sim+rmultinom(20000,lizplot[i],prop);
  EV=EV+prop*lizplot[i]
  DivEV=abs(EV-liz)
  psub=psub+prop*(lizplot[i]/sum(lizplot))
}


simb<-sim[3,]
hist(simb, breaks=100)
length(simb[simb>=EV[3]+DivEV[3]]) #P-value for boulders


#WITHOUT BOULDERS

data=subset(lizards,lizards$Site=="CULV" & !lizards$Substrate=="B")
habdata=subset(habitats,habitats$Site=="CULV")

liz<-c(sum(data$Substrate=="W"),sum(data$Substrate=="L"),sum(data$Substrate=="G"))
pliz<-liz/sum(liz) #W,L,G

x=c(6,16,26,32,48,52,68,70,82,98,100,112)
y=data$Marker
lizmarker=as.numeric()
for(j in 1:length(y)){ #creates a vector that assigns each lizard to its closest plot
  lizmarker[j]=x[which(abs(x-y[j])==min(abs(y[j]-x)))]
}

data=cbind(data,lizmarker)

lizplot=rep(0,length(x))
for(i in 1:length(x)){ #creates a vector lizplot for the count of lizards in each plot
  lizplot[i]=length(data$lizmarker[data$lizmarker==x[i]])
}

sim=matrix(0,3,20000)
EV=rep(0,3)
psub=rep(0,3)
for(i in 1:length(x)){
  W=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="W"]);
  L=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="L"]);
  B=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="G"]);
  prop=c(W,L,B)/sum(W,L,B);
  sim=sim+rmultinom(20000,lizplot[i],prop);
  EV=EV+prop*lizplot[i]
  DivEV=abs(EV-liz)
  psub=psub+prop*(lizplot[i]/sum(lizplot))
}


simg<-sim[3,]
hist(simg, breaks=100)
length(simg[simg<=EV[3]-DivEV[3]]) #P-value for ground



#site STR400, no GROUND

data=subset(lizards,lizards$Site=="400" & !lizards$Substrate=="G")
habdata=subset(habitats,habitats$Site=="400")

liz<-c(sum(data$Substrate=="W"),sum(data$Substrate=="L"),sum(data$Substrate=="B"))
pliz<-liz/sum(liz) #W,L,B

x=c(0,18,24,38,42,58,64,78,84,90,108,112,128,138)
y=data$Marker
lizmarker=as.numeric()
for(j in 1:length(y)){ #creates a vector that assigns each lizard to its closest plot
  lizmarker[j]=x[which(abs(x-y[j])==min(abs(y[j]-x)))]
}

data=cbind(data,lizmarker)

lizplot=rep(0,length(x))
for(i in 1:length(x)){ #creates a vector lizplot for the count of lizards in each plot
  lizplot[i]=length(data$lizmarker[data$lizmarker==x[i]])
}

sim=matrix(0,3,20000)
EV=rep(0,3)
psub=rep(0,3)
for(i in 1:length(x)){
  W=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="W"]);
  L=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="L"]);
  B=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="B"]);
  prop=c(W,L,B)/sum(W,L,B);
  sim=sim+rmultinom(20000,lizplot[i],prop);
  EV=EV+prop*lizplot[i]
  DivEV=abs(EV-liz)
  psub=psub+prop*(lizplot[i]/sum(lizplot))
}

simw<-sim[1,]
hist(simw, breaks=100)
length(simw[simw>=EV[1]+DivEV[1]]) #P-value for wood


#site STR400, no WOOD

data=subset(lizards,lizards$Site=="400" & !lizards$Substrate=="W")
habdata=subset(habitats,habitats$Site=="400")

liz<-c(sum(data$Substrate=="G"),sum(data$Substrate=="L"),sum(data$Substrate=="B"))
pliz<-liz/sum(liz) #W,L,B

x=c(0,18,24,38,42,58,64,78,84,90,108,112,128,138)
y=data$Marker
lizmarker=as.numeric()
for(j in 1:length(y)){ #creates a vector that assigns each lizard to its closest plot
  lizmarker[j]=x[which(abs(x-y[j])==min(abs(y[j]-x)))]
}

data=cbind(data,lizmarker)

lizplot=rep(0,length(x))
for(i in 1:length(x)){ #creates a vector lizplot for the count of lizards in each plot
  lizplot[i]=length(data$lizmarker[data$lizmarker==x[i]])
}

sim=matrix(0,3,20000)
EV=rep(0,3)
psub=rep(0,3)
for(i in 1:length(x)){
  W=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="G"]);
  L=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="L"]);
  B=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="B"]);
  prop=c(W,L,B)/sum(W,L,B);
  sim=sim+rmultinom(20000,lizplot[i],prop);
  EV=EV+prop*lizplot[i]
  DivEV=abs(EV-liz)
  psub=psub+prop*(lizplot[i]/sum(lizplot))
}

simg<-sim[1,]
hist(simg, breaks=100)
length(simg[simg<=EV[1]-DivEV[1]]) #P-value for ground


#site STR800, no GROUND

data=subset(lizards,lizards$Site=="800" & !lizards$Substrate=="G")
habdata=subset(habitats,habitats$Site=="800")

liz<-c(sum(data$Substrate=="W"),sum(data$Substrate=="L"),sum(data$Substrate=="B"))
pliz<-liz/sum(liz) #W,L,B

x=c(2,12,26,38,46,58,66,78)
y=data$Marker
lizmarker=as.numeric()
for(j in 1:length(y)){ #creates a vector that assigns each lizard to its closest plot
  lizmarker[j]=x[which(abs(x-y[j])==min(abs(y[j]-x)))]
}

data=cbind(data,lizmarker)

lizplot=rep(0,length(x))
for(i in 1:length(x)){ #creates a vector lizplot for the count of lizards in each plot
  lizplot[i]=length(data$lizmarker[data$lizmarker==x[i]])
}

sim=matrix(0,3,20000)
EV=rep(0,3)
psub=rep(0,3)
for(i in 1:length(x)){
  W=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="W"]);
  L=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="L"]);
  B=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="B"]);
  prop=c(W,L,B)/sum(W,L,B);
  sim=sim+rmultinom(20000,lizplot[i],prop);
  EV=EV+prop*lizplot[i]
  DivEV=abs(EV-liz)
  psub=psub+prop*(lizplot[i]/sum(lizplot))
}

simw<-sim[1,]
hist(simw, breaks=100)
length(simw[simw>=EV[1]+DivEV[1]]) #P-value for wood


#site STR800, no WOOD

data=subset(lizards,lizards$Site=="800" & !lizards$Substrate=="W")
habdata=subset(habitats,habitats$Site=="800")

liz<-c(sum(data$Substrate=="G"),sum(data$Substrate=="L"),sum(data$Substrate=="B"))
pliz<-liz/sum(liz) #W,L,B

x=c(2,12,26,38,46,58,66,78)
y=data$Marker
lizmarker=as.numeric()
for(j in 1:length(y)){ #creates a vector that assigns each lizard to its closest plot
  lizmarker[j]=x[which(abs(x-y[j])==min(abs(y[j]-x)))]
}

data=cbind(data,lizmarker)

lizplot=rep(0,length(x))
for(i in 1:length(x)){ #creates a vector lizplot for the count of lizards in each plot
  lizplot[i]=length(data$lizmarker[data$lizmarker==x[i]])
}


sim=matrix(0,3,20000)
EV=rep(0,3)
psub=rep(0,3)
for(i in 1:length(x)){
  W=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="G"]);
  L=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="L"]);
  B=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="B"]);
  prop=c(W,L,B)/sum(W,L,B);
  sim=sim+rmultinom(20000,lizplot[i],prop);
  EV=EV+prop*lizplot[i]
  DivEV=abs(EV-liz)
  psub=psub+prop*(lizplot[i]/sum(lizplot))
}

simg<-sim[1,]
hist(simg, breaks=100)
length(simg[simg<=EV[1]-DivEV[1]]) #P-value for ground



#What is the probability of Type II error when the null is rejected?

#remove ground from the analysis (these are the only tests in which the null was rejected)
#create a bias in substrate selection with either a small, medium, and large effect size, based on Cohen's h
#give each lizard a perch assignment
#how often does this sample of n lizards result in failure to reject the null at P=0.05?
#this is the rate of Type II error for a given effect size


rm(list=ls())

lizards<-read.csv(file="Lizards.csv", header=TRUE)
habitats<-read.csv(file="Habitats.csv", header=TRUE)

lizards=subset(lizards,lizards$Month=="Jan" & lizards$Repeat=="N" & (lizards$Site=="400" | lizards$Site=="800" | lizards$Site=="CULV"))
habitats=subset(habitats,!(habitats$Site=="800" & (habitats$Plot=="102" |habitats$Plot=="116")))

#site CULV

data=subset(lizards,lizards$Site=="CULV" & !lizards$Substrate=="G")
habdata=subset(habitats,habitats$Site=="CULV")

#Here are the function to determine new probabilities for various effect sizes
h=0.8
#if I want simulated probability to be SMALLER than what's available
#cohen=function(p){return(sin((2*asin(sqrt(p))-h)/2)^2)}
#if I want simulated probability to be LARGER than what's available
cohen=function(p){return(sin((2*asin(sqrt(p))+h)/2)^2)}


#Meter markers of plots in stream
x=c(6,16,26,32,48,52,68,70,82,98,100,112) #CULV
#x=c(0,18,24,38,42,58,64,78,84,90,108,112,128,138) #STR400
#x=c(2,12,26,38,46,58,66,78) #STR800


nsim=20000 #number of multinomial simulations to determine how unlikely particular lizard sample is
metasim=5000 #number of runs for this error simulations
alpha=0.05 #level of significance for rejecting the null

y=data$Marker
lizmarker=as.numeric()
for(j in 1:length(y)){ #creates a vector that assigns each lizard to its closest plot
  lizmarker[j]=x[which(abs(x-y[j])==min(abs(y[j]-x)))]
}

data=cbind(data,lizmarker)

lizplot=rep(0,length(x))
for(i in 1:length(x)){ #creates a vector lizplot for the count of lizards in each plot
  lizplot[i]=length(data$lizmarker[data$lizmarker==x[i]])
}

n=sum(lizplot) #sample size of lizards caught
TypeII=matrix(0,nrow=length(n),ncol=metasim)#will hold rates of error

sub=matrix(0,3,length(x)) #generate matrix of substrate availabilities for each plot
for (i in 1:length(x)){
  W=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="W"]);
  L=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="L"]);
  B=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="B"]);
  prop=c(W,L,B)/sum(W,L,B);
  sub[,i]=prop
}

#This is the start of the error simulation
#The deviation from the null comes from a probability of perching different from availability
#The magnitude of this deviation is based on Cohen's h

for(k in 1:length(n)){
  for (b in 1:metasim){
    
    lizsub=matrix(0,3,length(lizmarker)) #give each lizard a perch assignment using appropriate Cohen's h
    for(g in 1:length(lizmarker)){
      perch=sub[,which(lizmarker[g]==x)]
      perch[3]=cohen(perch[3]) #This is tweaking the BOULDER effect size
      perch[1:2]=perch[1:2]*(1-perch[3])/(sum(perch[1:2]))#This is renormalizing so proportions add to 1
      lizsub[,g]=rmultinom(1,1,perch)
    }
    
    liz<-c(sum(lizsub[1,]),sum(lizsub[2,]),sum(lizsub[3,])) #summary of sample
    pliz<-liz/sum(liz) #W,L,B
  
    
    sim=matrix(0,3,nsim) #run the simulation as before
    EV=rep(0,3)
    psub=rep(0,3)
    for(q in 1:length(x)){
      W=sum(habdata$SA[habdata$Plot==x[q]&habdata$Substrate=="W"]);
      L=sum(habdata$SA[habdata$Plot==x[q]&habdata$Substrate=="L"]);
      B=sum(habdata$SA[habdata$Plot==x[q]&habdata$Substrate=="B"]);
      prop=c(W,L,B)/sum(W,L,B);
      sim=sim+rmultinom(nsim,lizplot[q],prop);
      EV=EV+prop*lizplot[q]
      DivEV=abs(EV-liz)
      psub=psub+prop*(lizplot[q]/sum(lizplot))
    }
    
    simb<-sim[3,]
    TypeII[k,b]=length(simb[simb>=EV[3]+DivEV[3]])>=alpha*nsim #this matrix holds the # of TypeII errors, each row is a different sample size
  }}

rowMeans(TypeII)


#sites STR400 and STR 800

rm(list=ls())

lizards<-read.csv(file="Lizards.csv", header=TRUE)
habitats<-read.csv(file="Habitats.csv", header=TRUE)

lizards=subset(lizards,lizards$Month=="Jan" & lizards$Repeat=="N" & (lizards$Site=="400" | lizards$Site=="800" | lizards$Site=="CULV"))
habitats=subset(habitats,!(habitats$Site=="800" & (habitats$Plot=="102" |habitats$Plot=="116")))


data=subset(lizards,lizards$Site=="400" & !lizards$Substrate=="G")
habdata=subset(habitats,habitats$Site=="400")
#data=subset(lizards,lizards$Site=="800" & !lizards$Substrate=="G")
#habdata=subset(habitats,habitats$Site=="800")

#Here are the function to determine new probabilities for various effect sizes
h=0.8
#if I want simulated probability to be SMALLER than what's available
#cohen=function(p){return(sin((2*asin(sqrt(p))-h)/2)^2)}
#if I want simulated probability to be LARGER than what's available
cohen=function(p){return(sin((2*asin(sqrt(p))+h)/2)^2)}


#Meter markers of plots in stream
#x=c(6,16,26,32,48,52,68,70,82,98,100,112) #CULV
x=c(0,18,24,38,42,58,64,78,84,90,108,112,128,138) #STR400
#x=c(2,12,26,38,46,58,66,78) #STR800


nsim=20000 #number of multinomial simulations to determine how unlikely particular lizard sample is
metasim=5000 #number of runs for this error simulations
alpha=0.05 #level of significance for rejecting the null


y=data$Marker
lizmarker=as.numeric()
for(j in 1:length(y)){ #creates a vector that assigns each lizard to its closest plot
  lizmarker[j]=x[which(abs(x-y[j])==min(abs(y[j]-x)))]
}

data=cbind(data,lizmarker)

lizplot=rep(0,length(x))
for(i in 1:length(x)){ #creates a vector lizplot for the count of lizards in each plot
  lizplot[i]=length(data$lizmarker[data$lizmarker==x[i]])
}

n=sum(lizplot) #sample size of lizards caught
TypeII=matrix(0,nrow=length(n),ncol=metasim)#will hold rates of error


sub=matrix(0,3,length(x)) #generate matrix of substrate availabilities for each plot
for (i in 1:length(x)){
  W=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="W"]);
  L=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="L"]);
  B=sum(habdata$SA[habdata$Plot==x[i]&habdata$Substrate=="B"]);
  prop=c(W,L,B)/sum(W,L,B);
  sub[,i]=prop
}

#This is the start of the error simulation
#The deviation from the null comes from a probability of perching different from availability
#The magnitude of this deviation is based on Cohen's h

for(k in 1:length(n)){
  for (b in 1:metasim){
    
    lizsub=matrix(0,3,length(lizmarker)) #give each lizard a perch assignment using appropriate Cohen's h
    for(g in 1:length(lizmarker)){
      perch=sub[,which(lizmarker[g]==x)]
      perch[1]=cohen(perch[1]) #This is tweaking the WOOD effect size
      perch[2:3]=perch[2:3]*(1-perch[1])/(sum(perch[2:3]))#This is renormalizing so proportions add to 1
      perch[is.nan(perch)] = 0
      lizsub[,g]=rmultinom(1,1,perch)
    }
    
    liz<-c(sum(lizsub[1,]),sum(lizsub[2,]),sum(lizsub[3,])) #summary of sample
    pliz<-liz/sum(liz) #W,L,B
  
    
    sim=matrix(0,3,nsim) #run the simulation as before
    EV=rep(0,3)
    psub=rep(0,3)
    for(q in 1:length(x)){
      W=sum(habdata$SA[habdata$Plot==x[q]&habdata$Substrate=="W"]);
      L=sum(habdata$SA[habdata$Plot==x[q]&habdata$Substrate=="L"]);
      B=sum(habdata$SA[habdata$Plot==x[q]&habdata$Substrate=="B"]);
      prop=c(W,L,B)/sum(W,L,B);
      sim=sim+rmultinom(nsim,lizplot[q],prop);
      EV=EV+prop*lizplot[q]
      DivEV=abs(EV-liz)
      psub=psub+prop*(lizplot[q]/sum(lizplot))
    }
    
    simw<-sim[1,]
    TypeII[k,b]=length(simw[simw>=EV[1]+DivEV[1]])>=alpha*nsim #this matrix holds the # of TypeII errors, each row is a different sample size
  }}

rowMeans(TypeII)

