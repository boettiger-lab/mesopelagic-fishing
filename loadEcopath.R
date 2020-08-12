# Program to load in Ecopath files and create vectors and matrices as needed for calculations.  This program is called from outside, with a specified filename, work director and root directory

#datadir<-paste(root.dir,modelname,sep="")
#setwd(datadir)

## IMPORT DATA#########################
#Ecopath_BAL <- read.csv(file = "/Users/koehnl/Dropbox/Run_Ecopath/CCEcopath_InputJULY25.csv", header = TRUE)

#diet.mat=read.csv("diet.csv",header=TRUE)
#diet_trade <- read.csv(file = "/Users/koehnl/Dropbox/Run_Ecopath/Pedigree/diet_original_tradeoff.csv", header = TRUE)
#diet_trade <- read.csv(file = "/Users/laurakoehn/Dropbox/All constrained/Diet8trade.csv", header = TRUE)
#diet.mat = diet_trade[-94,] # diet matrix without import diet row (is in the Group.data list)
#diet.mat = diet_trade

#Group.data=read.csv("Groupinfo.csv",header=TRUE)
# Group.data is now a dataframe created in runecopath_BEST
n.groups=dim(Group.data)[1]
##############################

## SET UP VECTORS###############
B=as.matrix(Group.data$Biomass)
PB=as.matrix(Group.data$PB)
QB=as.matrix(Group.data$QB)
EE=as.matrix(Group.data$EE)
BA.B=as.matrix(Group.data$BA)
GCE=as.matrix(Group.data$GCE)
Group.Type=as.matrix(Group.data$GroupType)
BA=BA.B/B
#Landings=as.matrix(Y.mat$Yield)
#Discards=as.matrix(Y.mat$Discard)
#Y=Landings+Discards
Y = Yield #from main runecopath file = landings + discards
Import.feeding=as.matrix(Group.data$Import) # was the last row of diet matrix
Group.names=as.vector(unlist(Group.data$Group))
p=as.matrix(diet.mat[,2:(n.groups+1)])
#Species.Type<-Group.data$SpeciesType <- don't use?
#######################################
#  make sure that diet matrices add up to 1 perfectly
sum.of.p<-colSums(p)+Import.feeding
# for groups that have no diet, just put a 1 in for sum of p - phytoplankton and detritus
is.zero<-which(sum.of.p==0)
if (length(is.zero)>0) sum.of.p[is.zero]=rep(1,length(is.zero))
sump<-matrix(sum.of.p,nrow=n.groups,ncol=n.groups,byrow=TRUE)
new.p<-p/sump
p<-new.p
import.notzero<-which(colSums(p)>0)
Import.feeding[import.notzero]<-Import.feeding[import.notzero]/sum.of.p[import.notzero]

#######################################

### DEFINE GROUPS
Group.Type[93] = "nonLiving" #HAD WRONG CAPITALIZATION
living.groups=which(Group.Type!="nonLiving")
n.living.groups=length(living.groups)
p.living=p[living.groups,living.groups]
producer.groups=which(Group.Type=="Producer")
detritus.groups=which(Group.Type=="nonLiving")
n.detritus.groups=length(detritus.groups)
consumer.groups=which(Group.Type=="Consumer")
n.consumer.groups=length(consumer.groups)
GCE=PB/QB
GCE[producer.groups]=rep(0,length(producer.groups)) #What about detritus group GCE?********** Make 0

## CALCULATE F, CONSUMPTION MATRIX AND ALPHAS
F=Y/B
B.QB=c(B*QB)
na.index=which(is.na(B.QB)==TRUE)
B.QB[na.index]=rep(0,length(na.index))
d.B.QB=diag(B.QB,nrow=n.groups,ncol=n.groups)
Bimat=B%*%matrix(1,nrow=1,ncol=n.groups)
C=p%*%d.B.QB
BPB<-matrix(rep(t(B*PB),n.groups),nrow=n.groups,ncol=n.groups)
Mp<-C/BPB # this gives the predation mortality by predator as a percentage of total mortality
Mp<-replace(Mp,is.na(Mp),0)
M<-PB-F
PB.M<-matrix(rep(t(PB/M),n.groups),nrow=n.groups,ncol=n.groups)
MpM<-Mp*PB.M # proportion of non-fishing mortality due to each predator

## OPTIONAL SWITCH TO REMOVE SMALL LINKS

# if (rm.weak.links){
#   prey.link.thresh<-0.025
#   pred.link.thresh<-0.025
#   p.small<-which(p<prey.link.thresh&p>0)
#   pred.small<-which(MpM<pred.link.thresh&MpM>0)
#   all.2.zero<-intersect(p.small,pred.small)
#   # Replace diet matrix with 0
#   if (length(all.2.zero)>0) {
#     new.p<-replace(p,all.2.zero,0)
#     sigma.p<-colSums(new.p)+Import.feeding
#     # Standardize P for living groups
#     for (grp in 1:n.consumer.groups){
#       consumer.id<-consumer.groups[grp]
#       summed.p<-sigma.p[consumer.id]
#       new.p[,consumer.id]<-new.p[,consumer.id]/summed.p
#       Import.feeding[consumer.id]<-Import.feeding[consumer.id]/summed.p
#     }
#     p<-new.p
#     # Update production / Mo and other parmaeters
#     C=p%*%d.B.QB
#     BPB<-matrix(rep(t(B*PB),n.groups),nrow=n.groups,ncol=n.groups)
#     Mp<-C/BPB # this gives the predation mortality by predator as a percentage of total mortality
#     Mp<-replace(Mp,is.na(Mp),0)
#     M<-PB-F
#     PB.M<-matrix(rep(t(PB/M),n.groups),nrow=n.groups,ncol=n.groups)
#     MpM<-Mp*PB.M # proportion of non-fishing mortality due to each predator
#     
#     
#     
#   }
# }


#setwd(workdir)