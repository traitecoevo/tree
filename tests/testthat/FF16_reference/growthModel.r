#load data & libaries

#Coding guidlines
# -arguments-> give in order: traits, size, env

#returns vector from lo to hi with multiplication steps of incr. Used for making ticks to a log-scaled axis
seqLog <- function(from, to, n,base=10){base^(seq(log(from,base),log(to,base),length.out=n))}


#ALLOMETRIC MODEL FOR SIZE OF CONMPONENTS
Height <- function(A){p.a_l1*A^p.a_l2}
dHdA<-function(A){p.a_l2*p.a_l1*A^(p.a_l2-1)}
LeafArea<-function(h){ (h/p.a_l1)^(1/p.a_l2)}
LeafMass<-function(lma, A){A*lma}
RootMass<-function(A){p.a_r1*A}
SapwoodMass<-function(rho, A,h){rho*p.theta*etac(p.eta)*A*h}
etac<-function(eta){1-2/(1+eta)+1/(1+2*eta)}
BarkMass<-function(rho, A,h){p.a_b1 * SapwoodMass(A,h, rho)}
LiveMass<-function(traits, A){
  ml =LeafMass(traits$lma, A)
  ms =SapwoodMass(traits$rho,A,Height(A))
  mb =BarkMass(traits$rho,A,Height(A))
  mr =RootMass(A)
  return(ml+ms+mb+mr)
  }

#SOLVE HEIGHT FOR GIVEN MASS
Height.mt<- function(traits, mt){
  #returns mass for given height
  LiveMass.wrap<-function(x, traits,mt){LiveMass(traits, LeafArea(x))-mt}
  y<-0*mt;
  for(i in 1:length(mt)){y[i]<-uniroot(LiveMass.wrap, c(0, 50), traits=traits, mt=mt[i])$root}
  return(y)}

#OPTIMISE GROWTH RATE WRT TRAIT: 1 = LMA, 2= WOOD DENSITY
optForGrowth<-function(traits, h, env, Range, option=1){
  if(length(h)>1){cat("error, h must length 1");}
  if(length(env)>1){cat("error, env must length 1");}

  #wrapper function to pass to optimise
  dHdt.wrap<-function(x, traits, h, env, option){
    if(option==1){  traits$lma<-x; XLAB = "LMA (kg/m2)";}
    if(option==2){  traits$rho<-x; XLAB = "RHO (kg/m3)";}

    dHdt(traits, h, env)}
  return(optimise(dHdt.wrap,Range, maximum= T, tol = 0.000001, traits=traits, h=h, env=env, option=option))
}

#CALCULATE WPLCP
WPLCP<-function(traits, h){
  if(length(h)>1){cat("error, h must length 1");}

  #wrapper function to pass to optimise
  production.wrap<-function(x, traits, h){Production(traits, h, env=x)}
  return(  uniroot(production.wrap, c(0, 1), traits=traits, h=h)$root)
}

#HEIGHT GROWTH RATE
dHdt<-function(traits, h, env){dHdA(LeafArea(h))*dAdMt(traits, LeafArea(h))*Production(traits, h, env)*(1-ReproductiveAllocation(traits$hmat,h))}

#MARGINAL COST OF LEAF AREA GROWTH
dMldA<-function(lma,A){A*0+lma}
dMsdA<-function(rho, A){rho*p.theta*etac(p.eta)*(Height(A)+A*dHdA(A))}
dMbdA<-function(rho, A){p.a_b1 * dMsdA(rho,A)}
dMrdA<-function(A){A*0+p.a_r1}
dMtdA<-function(traits,A){dMldA(traits$lma, A) + dMbdA(traits$rho,A) + dMsdA(traits$rho,A) + dMrdA(A)}
dAdMt<-function(traits, A){1/dMtdA(traits, A)}

#MARGINAL COST OF HEIGHT GROWTH
dMldH<-function(lma,h){ A=LeafArea(h); dMldA(lma,A)/dHdA(A)}
dMsdH<-function(rho,h){ A=LeafArea(h); dMsdA(rho,A)/dHdA(A)}
dMbdH<-function(rho,h){ A=LeafArea(h); dMbdA(rho,A)/dHdA(A)}
dMrdH<-function(h){ A=LeafArea(h); dMrdA(A)/dHdA(A)}

#reproductive allocation
ReproductiveAllocation <-function(hmat,h){p.a_f1/(1+exp(p.a_f2*(1-h/hmat)))}

#production functions
dMtdt<-function(traits, h, env){
  Production(traits, h, env)*(1-ReproductiveAllocation(traits$hmat,h))
}

Production <-function(traits, h, env, print=0){
  A = LeafArea(h)
  ms =SapwoodMass(traits$rho,A,h)
  mb =BarkMass(traits$rho,A,h)
  mr =RootMass(A)
  Production.detail(traits, A, ms, mb,  mr, env, print)
  }

Production.detail <-function(traits, A, ms, mb,  mr, env, print=0){
  if(print==1){
    cat("Assim = ", Assim(A, env), "\n")
    cat("Resp = ",Respiration(A*traits$lma, ms, mb,  mr), "\n")
    cat("Turn = ",Turnover(traits, A*traits$lma, ms, mb,  mr), "\n")
  }
  Assim(A, env) - Respiration(A*traits$lma, ms, mb,  mr) - Turnover(traits, A*traits$lma, ms, mb,  mr)
  }

Assim <- function(A, env){p.a_y*p.a_bio*A*p.a_p1 * env/(p.a_p2+env)}

Respiration <-function(ml, ms, mb,  mr){
  Respiration.leaf(ml) + Respiration.sapwood(ms) + Respiration.bark(mb) + Respiration.root(mr)
  }

Respiration.leaf <-function(ml){p.a_y*p.a_bio *(p.r_l*ml)}
Respiration.sapwood <-function(ms){p.a_y*p.a_bio * p.r_s * ms}
Respiration.bark <-function(mb){p.a_y*p.a_bio * p.r_b * mb}
Respiration.root <-function(mr){p.a_y*p.a_bio * p.r_r * mr}

Turnover <-function(traits, ml, ms, mb,  mr){
  Turnover.leaf(traits$lma, ml) + Turnover.sapwood(ms) + Turnover.bark(mb) + Turnover.root(mr)
  }
Turnover.leaf <-function(LMA, ml){p.k_l*ml}
Turnover.sapwood <-function(ms){p.k_s*ms}
Turnover.bark <-function(mb){p.k_b*mb}
Turnover.root <-function(mr){p.k_r*mr}
