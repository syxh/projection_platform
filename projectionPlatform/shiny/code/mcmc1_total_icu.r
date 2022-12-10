
set.seed(98798)
# dat = dailydata
dat$cases = dat$cases_community+dat$cases_dorms+dat$cases_imported


pcumsum=function(inp,d)
{
  foo = c(rep(0,d),inp)
  outp = 0*inp
  j=1:length(inp) -1
  for(i in 1:d)outp = outp + foo[j+i]
  return(outp)
}


head(dat)

#plot(dat$doy,dat$icu)
#lines(dat$doy,pcumsum(dat$it,17))


muf=function(dat,pars)
{
  # return((pars$risk1/pars$duration)*pcumsum(dat$cases_imported,pars$duration)+
  #          (pars$risk2/pars$duration)*pcumsum(dat$cases_community,pars$duration)+
  #          (pars$risk3/pars$duration)*pcumsum(dat$cases_dorms,pars$duration))
  foo0 = pars$const
  foo1 = (pars$risk11/pars$duration)*pcumsum(dat$cases_community,pars$duration)+
    (pars$risk12/pars$duration)*pcumsum(dat$cases_imported,pars$duration)+
    (pars$risk13/pars$duration)*pcumsum(dat$cases_dorms,pars$duration)
  foo2 = (pars$risk21/pars$duration)*pcumsum(dat$cases_community,pars$duration)+
    (pars$risk22/pars$duration)*pcumsum(dat$cases_imported,pars$duration)+
    (pars$risk23/pars$duration)*pcumsum(dat$cases_dorms,pars$duration)#+foo0
  post_cb = as.numeric((dat$doy>=(147)))
  output = (1-post_cb)*foo1 +post_cb*foo2 #+ foo0*post_cb
  output[output<0]=0
  return(output)
}


logl = function(dat,pars)
{
  mu = muf(dat,pars)
  # sgima = sqrt(mu)*pars$s
  # sgima[sgima<0.01]=0.01
  # LL = sum(dnorm(dat$icu,mu,sgima,log=TRUE))
  LL = sum(dpois(dat$icu,mu,log=TRUE))
  pars$LL = LL
  return(pars)
}

mh = function(oldp,newp,dat)
{
  reject = FALSE
  # if(newp$s<0)reject=TRUE
  # if(newp$duration<1)reject=TRUE
  # if(newp$risk1<0)reject=TRUE
  # if(newp$risk2<0)reject=TRUE
  # if(newp$risk3<0)reject=TRUE
  if(newp$duration<1)reject=TRUE
  if(newp$risk11<0)reject=TRUE
  if(newp$risk12<0)reject=TRUE
  if(newp$risk13<0)reject=TRUE
  
  if(newp$risk21<0)reject=TRUE
  if(newp$risk22<0)reject=TRUE
  if(newp$risk23<0)reject=TRUE
  
  if(!reject)
  {
    newp = logl(dat,newp)
    la = newp$LL - oldp$LL
    lu = -rexp(1)
    if(lu>la)reject=TRUE
  }
  if(reject)return(oldp)
  return(newp)
}

# pars = list(s=1.84,duration=25,risk1=0.8,risk2=0.6,risk3=0.02)
pars = list(const=1,duration=25,risk11=0.6,risk12=0.8,risk13=0.02,risk21=0.5,risk22=0.7,risk23=0.04)
pars = logl(dat,pars)


storage=list(
  duration=rep(0,MCMCITS),
  const=rep(0,MCMCITS),
  # post_cb=rep(0,MCMCITS),
  risk11=rep(0,MCMCITS),
  risk12=rep(0,MCMCITS),
  risk13=rep(0,MCMCITS),
  risk21=rep(0,MCMCITS),
  risk22=rep(0,MCMCITS),
  risk23=rep(0,MCMCITS),
  LL=rep(0,MCMCITS)
)
SUBITS=10
for(iteration in 1:MCMCITS)
{
  if(iteration%%1000==0)cat(iteration,'in',MCMCITS,'d =',pars$duration,'LL:',round(pars$LL),'\n')
  for(subit in 1:SUBITS)
  {
    oldpars = pars; pars$duration = round(rnorm(1,pars$duration,1)) ; pars = mh(oldpars,pars,dat)
    # oldpars = pars; pars$s = rnorm(1,pars$s,0.1) ; pars = mh(oldpars,pars,dat)
    # oldpars = pars; pars$risk1 = rnorm(1,pars$risk1,0.1) ; pars = mh(oldpars,pars,dat)
    # oldpars = pars; pars$risk2 = rnorm(1,pars$risk2,0.1) ; pars = mh(oldpars,pars,dat)
    # oldpars = pars; pars$risk3 = rnorm(1,pars$risk3,0.02) ; pars = mh(oldpars,pars,dat)
    oldpars = pars; pars$const = rnorm(1,pars$const,0.1) ; pars = mh(oldpars,pars,dat)
    oldpars = pars; pars$risk11 = rnorm(1,pars$risk11,0.1) ; pars = mh(oldpars,pars,dat)
    oldpars = pars; pars$risk12 = rnorm(1,pars$risk12,0.1) ; pars = mh(oldpars,pars,dat)
    oldpars = pars; pars$risk13 = rnorm(1,pars$risk13,0.01) ; pars = mh(oldpars,pars,dat)
    oldpars = pars; pars$risk21 = rnorm(1,pars$risk21,0.1) ; pars = mh(oldpars,pars,dat)
    oldpars = pars; pars$risk22 = rnorm(1,pars$risk22,0.1) ; pars = mh(oldpars,pars,dat)
    oldpars = pars; pars$risk23 = rnorm(1,pars$risk23,0.01) ; pars = mh(oldpars,pars,dat)
    
  }
  storage$duration[iteration] = pars$duration
  # storage$s[iteration] = pars$s
  # storage$risk1[iteration] = pars$risk1
  # storage$risk2[iteration] = pars$risk2
  # storage$risk3[iteration] = pars$risk3
  storage$const[iteration] = pars$const
  storage$risk11[iteration] = pars$risk11
  storage$risk12[iteration] = pars$risk12  
  storage$risk13[iteration] = pars$risk13
  storage$risk21[iteration] = pars$risk21
  storage$risk22[iteration] = pars$risk22
  storage$LL[iteration] = pars$LL
}


storage = as.data.frame(storage)
# plot(dat$doy,dat$icu)
# lines(dat$doy,muf(dat,pars),col=2)

write.csv(storage,paste0(mcmc_result_folder,'/mcmc1_ICU.csv',sep=''),row.names = FALSE)
# plot(storage$LL)


rm(iteration,subit,SUBITS,logl)
rm(oldpars,pars,storage)
rm(pcumsum)
rm(muf)

