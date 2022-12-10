set.seed(4535)
# 
# projectionwindow=45
# Functions preparation
smoother1 = function(x,y)
{
  require(mgcv)
  oldd = data.frame(x=x,y=y)  
  newd = data.frame(x=min(x):max(x))
  fit = gam(y~s(x,k=15),data=oldd)
  pred = as.vector(predict(fit,newdata=newd))
  # pred[pred<0]=0
  return(pred)
}
smoother = function(x,y)
{
  require(mgcv)
  oldd = data.frame(x=x,y=y)
  newd = data.frame(x=min(x):max(x))
  fit = gam(y~s(x,k=15),data=oldd)
  pred = as.vector(predict(fit,newdata=newd))
  pred[pred<0]=0
  return(pred)
}
pcumsum=function(inp,d)
{
  foo = c(rep(0,d),inp)
  outp = 0*inp
  j=1:length(inp) -1
  for(i in 1:d)outp = outp + foo[j+i]
  return(outp)
}

dcumsum=function(inp,d)
{
  foo = c(rep(0,d),inp)
  outp = foo[1:length(inp)]
  return(outp)
}

muf=function(dats,pars,type='icu',SCENARIO)
{
  if(type=='icu')
  { # consider the adjustment by the time point.
    foo1 = (pars$risk11/pars$duration)*pcumsum(dats$cases_community,pars$duration)+
      (pars$risk12/pars$duration)*pcumsum(dats$cases_imported,pars$duration)+
      (pars$risk13/pars$duration)*pcumsum(dats$cases_dorms,pars$duration)
    foo2 = (pars$risk21/pars$duration)*pcumsum(dats$cases_community,pars$duration)+
      (pars$risk22/pars$duration)*pcumsum(dats$cases_imported,pars$duration)+
      (pars$risk23/pars$duration)*pcumsum(dats$cases_dorms,pars$duration)#+foo0
    post_cb = as.numeric((dats$doy>=(153)))
    output = (1-post_cb)*foo1 +post_cb*foo2 
    output[output<0]=0
    return(output)
  }
  
  if(type=='local')
  {
    # consider 6 types of mobility to estimate/project the community infection, without spillover
    tv = dats$doy
    foo0=pars$a00
    alpha = exp(pars$a0 + pars$a1*(move_resident_projection$mC[1:length(dats$cases_community)]/100)
                + pars$a2*(move_transit_projection$mC[1:length(dats$cases_community)]/100)
                + pars$a3*(move_grocery_projection$mC[1:length(dats$cases_community)]/100)
                + pars$a4*(move_parks_projection$mC[1:length(dats$cases_community)]/100)
                + pars$a5*(move_works_projection$mC[1:length(dats$cases_community)]/100)
                + pars$a6*(move_retails_projection$mC[1:length(dats$cases_community)]/100)
    )
    communitypar = alpha#rep(alpha,length(dats$cases_community))
    communitypar[tv>=SCENARIO$doy1]=communitypar[tv>=SCENARIO$doy1]*SCENARIO$newR
    # communitypar[tv>=SCENARIO$doy1][which(communitypar[tv>=SCENARIO$doy1]>SCENARIO$newR)] = SCENARIO$newR*0.8
    
    foo2=communitypar*dcumsum(dats$cases_community,pars$duration2) # day 100 is 9 Apr, hotel based SHN 

    cb = as.numeric((dats$doy>=98)*(dats$doy<=153))
    foo2[cb==1] = foo2[cb==1]*pars$cbe
    # foo3[cb==1] = foo3[cb==1]*pars$cbe
    # output = (foo0+foo1+foo2+foo3)*(NPOP-cumsum(dats$cases_community))/NPOP# herd immunity factor
    output = (foo0+foo2)*(NPOP-cumsum(dats$cases_community))/NPOP# herd immunity factor
    return(output)
  }
  
  if(type=='imported')
  {
    # output = pars$a1+pars$a2*dnorm(dats$doy,pars$a3,pars$a4)
    output = pars$a1+pars$a2*dnorm(dats$doy,pars$a3,pars$a4) +  pars$a5*(dats$doy>=203) + (pars$a5-pars$a1)*as.numeric((dats$doy>182 & dats$doy<203))*c(rep(0,length(which(dats$doy<=182))),(dats$doy[(dats$doy>182)&(dats$doy<203)]-182)/(203-182),rep(0,length(which(dats$doy>=203))))#c(rep(0,159),(pars$a5*(dat$doy[160:length(dat$doy)]-178)))#pars$a5*dnorm(dat$doy,pars$a6,pars$a7)
    
    tv = dats$doy
    pv = (tv-SCENARIO$doy2)*(1/SCENARIO$importationrampup)
    pv[pv<0]=0
    pv[pv>1]=1
    qv=1-pv
    
    output= qv*output + pv*SCENARIO$importationlevel
    #if(dats$doy>=SCENARIO$doy2)output = SCENARIO$importationlevel
    return(output)
  }
  
  if(type=='deaths')
  {
    output = (pars$risk/pars$duration)*pcumsum(dats$icu,pars$duration)
    return(output)
  }
  
}

# Mobility for community infection forecast -------------------------------
# projection_transit ------------------------------------------------------

load(paste0('working/movement_projection_transit.rdata'))
move_transit=mobility_projection

# move2$move=move2$move[1:1000,]
# PROJECTIONS=mobility_projection
output = matrix(0,length(move_transit$t),5)
output[,2]=move_transit$t
output[,1]=as.Date(seq.Date(as.Date("2020-01-19"),
                            (as.Date("2020-01-19")+(length(move_transit$t)-1)),"day"))
# CI=matrix(0,dim(PROJECTIONS$move)[2],3)
CI=matrix(0,dim(move_transit$move)[2],3)
for(KKK in 1:3)
{
  z=move_transit$move
  for(j in 1:dim(z)[2])CI[j,]=quantile(z[,j],c(25,500,975)/1000)
  
  hat = CI[,KKK]#smoother(PROJECTIONS$t,CI[,KKK])
  hat = smoother(seq_along(move_transit$t),CI[,KKK])
  output[,2+KKK]=hat
  #plot(PROJECTIONS$t,CI[,KKK],type='l')
  #lines(PROJECTIONS$t,hat,col=2)
  
}

output = as.data.frame(output)
output$V1 = as.Date(seq.Date(as.Date("2020-01-19"),
                             (as.Date("2020-01-19")+(length(move_transit$t)-1)),"day"))
names(output) = c("date", "dow", "mL", "mC", "mU")
move_transit_projection=output
# 

# projection resident -----------------------------------------------------
load(paste0('working/movement_projection_resident.rdata'))
move_resident=mobility_projection
output = matrix(0,length(move_resident$t),5)
output[,2]=move_resident$t
output[,1]=as.Date(seq.Date(as.Date("2020-01-19"),
                            (as.Date("2020-01-19")+(length(move_resident$t)-1)),"day"))
# CI=matrix(0,dim(PROJECTIONS$move)[2],3)
CI=matrix(0,dim(move_resident$move)[2],3)
for(KKK in 1:3)
{
  z=move_resident$move
  for(j in 1:dim(z)[2])CI[j,]=quantile(z[,j],c(25,500,975)/1000)
  
  hat = CI[,KKK]#smoother(PROJECTIONS$t,CI[,KKK])
  hat = smoother(seq_along(move_resident$t),CI[,KKK])
  output[,2+KKK]=hat
  #plot(PROJECTIONS$t,CI[,KKK],type='l')
  #lines(PROJECTIONS$t,hat,col=2)
  
}

output = as.data.frame(output)
output$V1 = as.Date(seq.Date(as.Date("2020-01-19"),
                             (as.Date("2020-01-19")+(length(move_resident$t)-1)),"day"))
names(output) = c("date", "dow", "mL", "mC", "mU")
move_resident_projection=output


# projection grocery ------------------------------------------------------
load(paste0('working/movement_projection_grocery.rdata'))
move_grocery=mobility_projection
output = matrix(0,length(move_grocery$t),5)
output[,2]=move_grocery$t
output[,1]=as.Date(seq.Date(as.Date("2020-01-19"),
                            (as.Date("2020-01-19")+(length(move_grocery$t)-1)),"day"))
# CI=matrix(0,dim(PROJECTIONS$move)[2],3)
CI=matrix(0,dim(move_grocery$move)[2],3)
for(KKK in 1:3)
{
  z=move_grocery$move
  for(j in 1:dim(z)[2])CI[j,]=quantile(z[,j],c(25,500,975)/1000)
  
  hat = CI[,KKK]#smoother(PROJECTIONS$t,CI[,KKK])
  hat = smoother(seq_along(move_grocery$t),CI[,KKK])
  output[,2+KKK]=hat
  #plot(PROJECTIONS$t,CI[,KKK],type='l')
  #lines(PROJECTIONS$t,hat,col=2)
  
}

output = as.data.frame(output)
output$V1 = as.Date(seq.Date(as.Date("2020-01-19"),
                             (as.Date("2020-01-19")+(length(move_grocery$t)-1)),"day"))
names(output) = c("date", "dow", "mL", "mC", "mU")
move_grocery_projection=output


# projection parks -------------------------------------------------------------------
load(paste0('working/movement_projection_parks.rdata'))
move_parks=mobility_projection
output = matrix(0,length(move_parks$t),5)
output[,2]=move_parks$t
output[,1]=as.Date(seq.Date(as.Date("2020-01-19"),
                            (as.Date("2020-01-19")+(length(move_parks$t)-1)),"day"))
# CI=matrix(0,dim(PROJECTIONS$move)[2],3)
CI=matrix(0,dim(move_parks$move)[2],3)
for(KKK in 1:3)
{
  z=move_parks$move
  for(j in 1:dim(z)[2])CI[j,]=quantile(z[,j],c(25,500,975)/1000)
  
  hat = CI[,KKK]#smoother(PROJECTIONS$t,CI[,KKK])
  hat = smoother(seq_along(move_parks$t),CI[,KKK])
  output[,2+KKK]=hat
  #plot(PROJECTIONS$t,CI[,KKK],type='l')
  #lines(PROJECTIONS$t,hat,col=2)
  
}

output = as.data.frame(output)
output$V1 = as.Date(seq.Date(as.Date("2020-01-19"),
                             (as.Date("2020-01-19")+(length(move_parks$t)-1)),"day"))
names(output) = c("date", "dow", "mL", "mC", "mU")
move_parks_projection=output


# projection works -------------------------------------------------------------------
load(paste0('working/movement_projection_works.rdata'))
move_works=mobility_projection
output = matrix(0,length(move_works$t),5)
output[,2]=move_works$t
output[,1]=as.Date(seq.Date(as.Date("2020-01-19"),
                            (as.Date("2020-01-19")+(length(move_works$t)-1)),"day"))
# CI=matrix(0,dim(PROJECTIONS$move)[2],3)
CI=matrix(0,dim(move_works$move)[2],3)
for(KKK in 1:3)
{
  z=move_works$move
  for(j in 1:dim(z)[2])CI[j,]=quantile(z[,j],c(25,500,975)/1000)
  
  hat = CI[,KKK]#smoother(PROJECTIONS$t,CI[,KKK])
  hat = smoother(seq_along(move_works$t),CI[,KKK])
  output[,2+KKK]=hat
  #plot(PROJECTIONS$t,CI[,KKK],type='l')
  #lines(PROJECTIONS$t,hat,col=2)
  
}

output = as.data.frame(output)
output$V1 = as.Date(seq.Date(as.Date("2020-01-19"),
                             (as.Date("2020-01-19")+(length(move_works$t)-1)),"day"))
names(output) = c("date", "dow", "mL", "mC", "mU")
move_works_projection=output



# projection retails -----------------------------------------------------------------
load(paste0('working/movement_projection_retails.rdata'))
move_retails=mobility_projection
output = matrix(0,length(move_retails$t),5)
output[,2]=move_retails$t
output[,1]=as.Date(seq.Date(as.Date("2020-01-19"),
                            (as.Date("2020-01-19")+(length(move_retails$t)-1)),"day"))
# CI=matrix(0,dim(PROJECTIONS$move)[2],3)
CI=matrix(0,dim(move_retails$move)[2],3)
for(KKK in 1:3)
{
  z=move_retails$move
  for(j in 1:dim(z)[2])CI[j,]=quantile(z[,j],c(25,500,975)/1000)
  
  hat = CI[,KKK]#smoother(PROJECTIONS$t,CI[,KKK])
  hat = smoother(seq_along(move_retails$t),CI[,KKK])
  output[,2+KKK]=hat
  #plot(PROJECTIONS$t,CI[,KKK],type='l')
  #lines(PROJECTIONS$t,hat,col=2)
  
}

output = as.data.frame(output)
output$V1 = as.Date(seq.Date(as.Date("2020-01-19"),
                             (as.Date("2020-01-19")+(length(move_retails$t)-1)),"day"))
names(output) = c("date", "dow", "mL", "mC", "mU")
move_retails_projection=output



rm(move_resident,move_transit, move_grocery,move_parks,move_works,move_retails)
###########################################


SCENARIOS <<- read.csv("scenarios.csv")[1,]
source('code/sir_functions.r')
load(paste0(mcmc_result_folder,'/mcmc.rdata',sep=''))
projector = function(iteration,tdelta=14,SCENARIO)
{
  M=mcmc
  proj = as.list(dat)
  proj$cases = proj$cases_community+proj$cases_dorms +proj$cases_imported
  T0 = length(dat$doy)
  tmax = dat$doy[T0]
  proj$doy = c(proj$doy,max(proj$doy)+(1:tdelta))
  proj$icu = c(proj$icu,rep(0,tdelta))
  proj$deaths = c(proj$deaths,rep(0,tdelta))
  proj$cases = c(proj$cases,rep(0,tdelta))
  proj$cases_imported = c(proj$cases_imported,rep(0,tdelta))
  proj$cases_community = c(proj$cases_community,rep(0,tdelta))
  proj$cases_dorms = c(proj$cases_dorms,rep(0,tdelta))
  proj$infns_dorms = c(proj$infns_dorms,rep(0,tdelta))
  
  sirpars = initialise(data=proj$doy,S0 = NFWs)
  sirpars$E0=M$m4$E0[iteration]
  sirpars$beta=M$m4$beta[iteration]
  sirpars$p =M$m4$p[iteration]
  sirpars$threshold=M$m4$threshold[iteration]
  sirpars$interventioneffect=M$m4$interventioneffect[iteration]
  epidemic_foreign=SEIR(sirpars)$dIcalib
  proj$infns_dorms = epidemic_foreign
  
  for(td in 1:tdelta)
  {
    I_m = epidemic_foreign[T0+td]
    toohigh = I_m > (sirpars$threshold/sirpars$p)
    mu = sirpars$p*I_m*(!toohigh) + sirpars$threshold*(toohigh)
    proj$cases_dorms[T0+td] = rpois(1,mu)
    proj$infns_dorms[T0+td] = I_m
    
    mu = muf(proj,M$m3[iteration,],'local',SCENARIO)
    proj$cases_community[T0+td] = rpois(1,mu[T0+td])
    
    mu = muf(proj,M$m2[iteration,],'imported',SCENARIO)
    proj$cases_imported[T0+td] = rpois(1,mu[T0+td])
    
    
    mu = muf(proj,M$m1[iteration,],'icu',SCENARIO)
    # proj$icu[T0+td] = round(rnorm(1,mu[T0+td],sqrt(mu[T0+td])*M$m1$s[iteration]));if(proj$icu[T0+td]<0)proj$icu[T0+td]=0
    proj$icu[T0+td] = round(rpois(1,mu[T0+td]));if(proj$icu[T0+td]<0)proj$icu[T0+td]=0
    mu=muf(proj,M$m5[iteration,],'deaths',SCENARIO)
    proj$deaths[T0+td] = mu[T0+td]
    
    
  }
  
  
  for(ti in 1:T0)
  {
    
    proj$cases_imported[ti] = rpois(1,muf(dat,M$m2[iteration,],'imported',SCENARIO)[ti])
    proj$cases_community[ti] = rpois(1,muf(dat,M$m3[iteration,],'local',SCENARIO)[ti])
    
    I_m = epidemic_foreign[1:T0]
    toohigh = I_m > (sirpars$threshold/sirpars$p)
    mu = sirpars$p*I_m*(!toohigh) + sirpars$threshold*(toohigh)
    proj$cases_dorms[ti] = rpois(1,mu[ti])
    proj$infns_dorms[ti] = I_m[ti]
    
    mu=muf(dat,M$m1[iteration,],'icu',SCENARIO)[ti]
    # proj$icu[ti] = rnorm(1,mu,sqrt(mu)*M$m1$s[iteration])
    proj$icu[ti] = rpois(1,mu)
    if(proj$icu[ti]<0)proj$icu[ti]=0
    proj$deaths[ti]=muf(dat,M$m5[iteration,],'deaths',SCENARIO)[ti]
  }
  
  proj$cases = proj$cases_community+proj$cases_dorms +proj$cases_imported
  return(proj)
}

# 
# for(scenarioid in 1:dim(SCENARIOS)[1])
# {
#   cat('Projecting scenario',scenarioid,'\n')
#   version=1 # redundant
#   SCENARIO = SCENARIOS[scenarioid,]
#   p=projector(100,projectionwindow,SCENARIO)
#   sampleit = seq(10,MCMCITS,10)
#   #sampleit = seq(1,MCMCITS,1)
#   M=matrix(0,length(sampleit),length(p$icu))
#   output = list(t=p$doy,icu=M,deaths=M,
#                 cases_imported=M,cases_community=M,cases_dorms=M,infns_dorms=M,cuminfns_dorms=M,
#                 cumcases_imported=M,cumcases_community=M,cumcases_dorms=M)
# 
#   for(i in seq(sampleit))
#   {
#     p=projector(sampleit[i],projectionwindow,SCENARIO)
#     output$icu[i,]=p$icu
#     output$deaths[i,]=p$deaths
#     output$cumcases_imported[i,]=cumsum(p$cases_imported)
#     output$cumcases_community[i,]=cumsum(p$cases_community)
#     output$cumcases_dorms[i,]=cumsum(p$cases_dorms)
#     output$cases_imported[i,]=(p$cases_imported)
#     output$cases_community[i,]=(p$cases_community)
#     output$cases_dorms[i,]=(p$cases_dorms)
#     output$infns_dorms[i,]=p$infns_dorms
#     output$cuminfns_dorms[i,]=cumsum(p$infns_dorms)
#   }
# 
#   projections = output
# #projections
# # if(version==1)save('projections',file = paste0(folder,'/working/projections.rdata',sep=''))
# # if(version==2)save('projections',file = paste0(folder,'/working/projections_v2.rdata',sep=''))
# # if(version==3)save('projections',file = paste0(folder,'/working/projections_v3.rdata',sep=''))
#   save('projections',file = paste0(folder,'/working/projections_scenario',scenarioid,'.rdata'))
# }
# 
# plot(dat$case_community)
# lines(projections$case_community,col=2)
 # rm(dat,M,mcmc,output,p,projections,i,sampleit,dcumsum,pcumsum,projector)
 # rm(initialise,loglikelihood,mh,SEIR,muf,version)

