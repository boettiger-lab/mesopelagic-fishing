


################################################################
################# Trade-Offs ###################################
################################################################
# For each balanced run, calculate trade-offs
# Can tell the function values that represent 1 functional group you want trade-offs for
# Pass the function 2 values - 1 for the group's position in the yield vector, and 1 for it's position in biomass vector


setwd("~/Dropbox/Balanced/Oct6_2015")

#models <- "/Users/koehnl/Dropbox/Balanced/Oct6_2015"
models <- "/Users/laurakoehn/Dropbox/Balanced/Oct6_2015"
file.names <- dir(models, pattern = ".csv")
paramfiles = grep(pattern = 'params', file.names, value = TRUE)
dietfiles = grep(pattern = 'diet', file.names, value = TRUE) # seem to be in the right order 
#inits <- read.csv(file = "/Users/koehnl/Dropbox/CalCurEcopath_2015/GroupInfo.csv", header = TRUE) 
inits <- read.csv(file = "/Users/laurakoehn/Dropbox/CalCurEcopath_2015/GroupInfo.csv", header = TRUE) 

Group = inits$GroupName
Type = inits$GroupType

M = length(paramfiles)
n = 93
PBs <- matrix(NA, nrow = n, ncol = M)
for(i in 1:length(paramfiles)) {
  file <- read.csv(paramfiles[i],header=TRUE)
  PBs[,i] = file$V2
}
# sardine total M goes from 0.442-0.591, average = 0.517

QBs <- matrix(NA, nrow = n, ncol = M)
for(i in 1:length(paramfiles)) {
  file <- read.csv(paramfiles[i],header=TRUE)
  QBs[,i] = file$V3
}

PC = PBs/QBs

Bs <- matrix(NA, nrow = n, ncol = M)
for(i in 1:length(paramfiles)) {
  file <- read.csv(paramfiles[i],header=TRUE)
  Bs[,i] = file$V1
}
biomassout <- matrix(NA, ncol = 500, nrow = 93)
for(p in 1:500) {
  
  file <- read.csv(paramfiles[p],header=TRUE)
  file2 <- read.csv(dietfiles[p], header = TRUE); diet = file2[,-1]
  Bio = file$V1; PB = file$V2; QB = file$V3; EE = file$V4; Yield = file$V5
  BA = rep(0, length = length(Type))
  ecopathinput = list(Group = Group, Biomass = Bio, PB = PB, QB = QB, EE = EE, Yield = Yield, BA = BA, Type = Type, DC = diet)
  ecopathout=runecopath(ecopathinput)
  B=ecopathout$Bout
  biomassout[,p] = B
}
func.groups = Group

AllTradeOffs <- function(paramfiles, dietfiles, Type, Group, yieldcode, biomasscode) { 
  n = length(paramfiles) # number of ecopath models
  yieldtradeoffs <- matrix(NA, ncol = n, nrow = 34) # 35 is the amount of groups with yield, only 33 when aggregating anchovy, herring, sardine, 34 if herring & anchovy
  biotradeoffs <- matrix(NA, ncol = n, nrow = 91) # 92 without aggregation, only 90 when aggregating, 91 with herring & anchovy 
  biomass <- matrix(NA, ncol = n, nrow = 93)
  yieldfinal <- matrix(NA, ncol = n, nrow = 92) # 91 with all 3 forage fish aggregated, 92 herring + anchovy
  BA = rep(0, length = length(Type))
  
  for(l in 1:n) {
    file <- read.csv(paramfiles[l],header=TRUE)
    file2 <- read.csv(dietfiles[l], header = TRUE); diet = file2[,-1]
    Bio = file$V1; PB = file$V2; QB = file$V3; EE = file$V4; Yield = file$V5
    ecopathinput = list(Group = Group, Biomass = Bio, PB = PB, QB = QB, EE = EE, Yield = Yield, BA = BA, Type = Type, DC = diet)
    ecopathout=runecopath(ecopathinput)
    B=ecopathout$Bout
    biomass[,l] = B
    EE=ecopathout$EEout
    Mp = ecopathout$Mpmat
    GCE = ecopathout$GCE; GCE[1,1] = 0; GCE[93,1] = 0
    tradeoffdiet = as.matrix(diet[-94,]); tradeoffdiet = as.data.frame(cbind(Group, tradeoffdiet))
    names = append("Group", as.character(Group)); colnames(tradeoffdiet) = names; diet.mat = tradeoffdiet
    dietImport = as.matrix(diet); Import = as.vector(dietImport[94,])
    Group.data = data.frame(Group = Group, Biomass = B, PB = PB, QB = QB, EE = EE, GCE = GCE, GroupType = Type, BA = BA, Import = Import)
   source("loadEcopath.R", local = TRUE) # run before aggregate function
    
    # aggregate groups 
    #     combine.group.list=list(c(20,21,22)) # 19,20,21 = loligo, sardine, anchovy; 20,21,22 = sardine, anchovy, herring
    #     combine.group.names=c("Forage")
    #     combine.group.type<-rep("Consumer",1)
    #     source("runAggregateGroups.R", local = TRUE)
    #     yieldfinal[,l] = Y
    #     # run trade off 
    #     source("runTradeOffmodel.R", local = TRUE)
    #     newyield = as.matrix(T.xbar.adj); newbio = as.matrix(dxbar.output)
    #     yieldtradeoffs[,l] = as.vector(newyield[,yieldcode])
    #     biotradeoffs[,l] = as.vector(newbio[,biomasscode])
    
   # yieldfinal[,l] = Y
    # run trade off 
    source("runTradeOffmodel.R", local = TRUE)
    newyield = as.matrix(T.xbar.adj); newbio = as.matrix(dxbar.output)
    yieldtradeoffs[,l] = as.vector(newyield[,yieldcode])
    biotradeoffs[,l] = as.vector(newbio[,biomasscode])
    
    yieldgroups = Group.names.adj; biogroups = Group.names.living
    rm(Group.data)
  }
  return(list("Yield" = yieldtradeoffs,"Biomass" = biotradeoffs, "yieldgroups" = yieldgroups, "biogroups" = biogroups, "biomass" = biomass, "Catch" = yieldfinal))
}
sardine = AllTradeOffs(paramfiles, dietfiles, Type = Type, Group = func.groups, yieldcode = 4, biomasscode = 20) # individual sardine run

Meso = AllTradeOffs(paramfiles, dietfiles, Type = Type, Group = func.groups, yieldcode = 4, biomasscode = 15) # individual mesopelagic run
# biomass code is the # that mesopelagics are in the list of species
# yield code is what # mesopelagics are in 