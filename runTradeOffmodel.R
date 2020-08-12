# model to run yeild trade-offs for a generalized production function
# Must specify the theta (prey dependence), epsilon (predator dependence)
# in functional repsonse as well as gamma - density dependence in PB ratio

###################################################
# set up calculations for jacobian
##################################################
living.groups=which(Group.Type!="nonLiving")
n.living.groups=length(living.groups)
p.living=p[living.groups,living.groups]
producer.groups=which(Group.Type=="Producer")
detritus.groups=which(Group.Type=="nonLiving")
n.detritus.groups=length(detritus.groups)
Group.names.living<-Group.names[living.groups]
## CALCULATE F, CONSUMPTION MATRIX AND ALPHAS
F=Y/B
GCE=PB/QB
GCE[producer.groups]=rep(0,length(producer.groups))
F.living<-F[living.groups]
GCE.living<-GCE[living.groups]
Bimat<-matrix(B,nrow=n.groups,ncol=n.groups,byrow=FALSE)
## Solve for M0, assuming no BA
Mpred=rowSums(C)/B
Mo=PB-F-Mpred

# Adjust PB as needed if Mo is negative
PB<-PB-pmin(0,Mo) # essentially adding the production needed to make Mo = 0
Mo<-PB-rowSums(Mp)-F # get new "Mo" with adjusted PB, should be 0 if PB is adjusted
#***Mo FOR DETRITUS IS INFINITE... - just make 0? mortality of detritus should be nothing...

Y.living<-Y[living.groups]
B.living<-B[living.groups]
Mo.living<-Mo[living.groups]
QB.living<-QB[living.groups]
B.QB.living<-B.living*QB[living.groups]
C.living<-(C[living.groups,living.groups])
# set up matrices of biomass in columns (x.i) and in rows (x.j)
x.i<-matrix(B.living,nrow=n.living.groups,ncol=n.living.groups,byrow=FALSE)
x.j<-matrix(B.living,nrow=n.living.groups,ncol=n.living.groups,byrow=TRUE)
# set up theta,, epsilon and gamma coefficients

theta.use = 1
eps.use = 0.7
gamma.use = 1

theta<-matrix(theta.use,nrow=n.living.groups,ncol=n.living.groups)
eps<-matrix(eps.use,nrow=n.living.groups,ncol=n.living.groups)
gamma<-matrix(gamma.use,nrow=n.living.groups,ncol=1)
detritus.eps<-matrix(eps.use,nrow=n.living.groups,ncol=1)
import.eps<-matrix(eps.use,nrow=n.living.groups,ncol=1)

# apply exponents to relevant biomass levels
x.j.eps<-x.j^eps
x.i.theta<-x.i^theta
theta.minus.1<-theta-matrix(1,nrow=n.living.groups,ncol=n.living.groups)
eps.minus.1<-eps-matrix(1,nrow=n.living.groups,ncol=n.living.groups)
gamma.minus.1<-gamma-matrix(1,nrow=n.living.groups,ncol=1)
eps.minus.2<-eps-matrix(2,nrow=n.living.groups,ncol=n.living.groups)
theta.minus.2<-theta-matrix(2,nrow=n.living.groups,ncol=n.living.groups)
x.i.theta.minus.1<-x.i^theta.minus.1
x.j.eps.minus.1<-x.j^eps.minus.1
x.j.eps.minus.2<-x.j^eps.minus.2
x.i.theta.minus.2<-x.i^theta.minus.2
PP.eq<-matrix(0,nrow=n.living.groups,ncol=1)
PP.eq[producer.groups]<-PB[producer.groups]
P.eq<-PP.eq/diag(x.j.eps.minus.1)
Moeq.living<-Mo[living.groups]/(B.living^(gamma-matrix(1,nrow=n.living.groups,ncol=1)))

# calculate alphas
alpha<-C.living/(x.j.eps*x.i.theta)

# calculate consumption on detritus and import consumption
Detritus.consumption=matrix(0,n.living.groups,1)
if (n.detritus.groups>0){
  if (n.detritus.groups>1){ 
    p.detritus=colSums(p[detritus.groups,living.groups])
  } else {
    p.detritus=p[detritus.groups,living.groups]
  }
  alpha.detritus=B.QB[living.groups]*p.detritus/(B.living^detritus.eps)
  alpha.detritus[producer.groups]=0
}

Import.consumption=QB.living*Import.feeding[living.groups]*B.living
Import.consumption[producer.groups]=0
alpha.import<-Import.consumption/B.living^import.eps

# Calculate jacobian
Jr=matrix(0,nrow=n.living.groups,ncol=n.living.groups)
for (i in 1:n.living.groups){
  index<-living.groups[i]
  if (Group.Type[index]=="Consumer"){
  for (j in 1:n.living.groups){
    if (j==i){
      # compute deriviative along the diagonal, slightly more complicated, but includes consumption on prey in model, cannibalism, on detritus and impor consumptions, minus predation and other mortality
      Jr[i,j]<-GCE.living[i]*sum(alpha[-i,i]*x.i.theta[-i,i]*eps.minus.1[-i,i]*x.j.eps.minus.2[-i,i])+
        (GCE.living[i]-1)*alpha[i,i]*(eps[i,i]+theta[i,i]-1)*B.living[i]^(eps[i,i]+theta[i,i]-2)+
        GCE.living[i]*alpha.detritus[i]*(detritus.eps[i]-1)*B.living[i]^(detritus.eps[i]-2)+
        GCE.living[i]*alpha.import[i]*(import.eps[i]-1)*B.living[i]^(import.eps[i]-2)-
        sum(alpha[i,]*x.i.theta.minus.2[i,]*theta.minus.1[i,]*x.j.eps[i,])-gamma.minus.1[i]*Moeq.living[i]*B.living[i]^(gamma[i]-2)
    } else {
      # use these terms in the off diagonal
    Jr[i,j]<-GCE.living[i]*alpha[j,i]*theta[j,i]*x.i.theta.minus.1[j,i]*x.j.eps.minus.1[j,i]-
      alpha[i,j]*eps[i,j]*x.i.theta.minus.1[i,j]*x.j.eps.minus.1[i,j]
    }
  }
  } else {
    # this is for producers, I used eps to relate primary production to producer biomass
	  for (j in 1:n.living.groups){
	    if (j==i){
	      Jr[i,j]<-P.eq[i]*x.j.eps.minus.2[i,j]*eps.minus.1[i,j]-
	        sum(alpha[i,]*x.i.theta.minus.2[i,]*theta.minus.1[i,]*x.j.eps[i,])-gamma.minus.1[i]*Moeq.living[i]*B.living[i]^(gamma[i]-2)
	  } else {
	    Jr[i,j]<--alpha[i,j]*eps[i,j]*x.i.theta.minus.1[i,j]*x.j.eps.minus.1[i,j]
	  }
	}
}
}

#############################################################################################
#### Calculate yield trade offs, separating out detritus groups as a constant biomass level
############################################################################################
r.xbar=F.living
# Calculate Jacobian of r vector
Jr.inv<-matrix(NA,nrow=nrow(Jr),ncol=ncol(Jr))
text<-"Jr.inv=solve(Jr)"
try(eval(parse(text=text)),silent=FALSE)
drbar=diag(c(r.xbar),nrow=n.living.groups,ncol=n.living.groups)
dxbar=diag(c(B.living),nrow=n.living.groups,ncol=n.living.groups)

Amat=drbar%*%Jr.inv+dxbar
d.Amat=diag(Amat)
D.A=diag(d.Amat,nrow=n.living.groups,ncol=n.living.groups)
# This is the tradeoff matrix, dyi /dyj.  (i = row, j =col)
T.xbar<-matrix(NA,n.living.groups,n.living.groups)
text<-"T.xbar=Amat%*%solve(D.A)"
try(eval(parse(text=text)),silent=FALSE)
T.xbar.output=matrix(T.xbar,nrow=n.living.groups,ncol=n.living.groups,dimnames=list(Group.names.living,Group.names.living))
pos.Yield=which(Y.living>0)
Group.names.adj=Group.names.living[pos.Yield]
T.xbar.adj=matrix(T.xbar[pos.Yield,pos.Yield],nrow=length(pos.Yield),ncol=length(pos.Yield),dimnames=list(Group.names.adj,Group.names.adj))
A.adj=Amat[pos.Yield,pos.Yield]
dYdf.result.lv=matrix(Amat,nrow=n.living.groups,ncol=n.living.groups,dimnames=list(Group.names.living,Group.names.living))


# calculate dxbar/dy - change in biomass for a change in yield
# default values for dxbar is matrix of NA, replaced with actual values if matrices
# are invertible
dxbar.dy<-matrix(NA,n.living.groups,n.living.groups)
text<-"dxbar.dy<-solve(Jr)%*%solve(D.A)"
try(eval(parse(text=text)),silent=FALSE)
dxbar.output<-matrix(dxbar.dy,nrow=n.living.groups,ncol=n.living.groups,dimnames=list(Group.names.living,Group.names.living))
dxbar.forage<-(dxbar.output[,18:30])
