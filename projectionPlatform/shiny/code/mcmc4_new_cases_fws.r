set.seed(54235)
library(mvtnorm)
source('code/sir_functions.r')
# LEVEL = c('high','medium','low')
LEVEL = c('medium')

BURNIN=1000
for(level in LEVEL)
{
  # cat('Scenario',level,'\n')
  # if(level=='low'){interventioneffect=0.3;testingrate=0.4}
  # if(level=='medium'){interventioneffect=0.6;testingrate=0.6}
  # if(level=='high'){interventioneffect=1;testingrate=0.8}
  interventioneffect=0.6;
  testingrate=0.6
  # dat=dailydata
  
  pars = initialise(NULL,dat$cases_dorms,NFWs)
  pars$p = testingrate
  pars$interventioneffect = interventioneffect
  pars$seir = SEIR(pars)
  # pars$seir = SEIR(pars,interventioneffect)
  pars$LL = loglikelihood(pars,dat$cases_dorms)
  
  
  z=rep(0,MCMCITS)
  storage = list(beta=z,E0=z,interventioneffect=z,threshold=z,p=z)
  # storage = list(beta=z,E0=z,p=z,threshold=z)
  rm(z)
  
  if(0)# if you've run before and want to tune
  {
    z=as.data.frame(storage)
    z = z[, 1:4] # 0616
    SIGMA = cov(z)
    dump('SIGMA','')
  }
  
  SIGMA <-
    structure(c(0.000154283903863271, -5.57086346445566e-07, 0, 0.00126632337184688, 
                -5.57086346445566e-07, 2.05648534763829e-09, 0, -3.78552109679242e-06, 
                0, 0, 0, 0, 0.00126632337184688, -3.78552109679242e-06, 0, 21.6082174166351
    ), .Dim = c(4L, 4L), .Dimnames = list(c("beta", "E0", "p", "threshold"
    ), c("beta", "E0", "p", "threshold")))
  
  SIGMA = matrix(0,4,4)
  SIGMA[1,1] = 0.00015
  SIGMA[2,2] = 2.1e-09
  SIGMA[3,3] = 0.001
  SIGMA[4,4] = 22
  
  SIGMA <-
    structure(c(0.00040761261361945398, -8.6549105131211031e-07, 
                -0.00041664202147499181, 0.0041584921099424828, -8.6549105131211031e-07, 
                2.0230033420942829e-09, 8.9401963770000138e-07, -6.6088719665757288e-06, 
                -0.00041664202147499181, 8.9401963770000138e-07, 0.00044630468213974726, 
                -0.0038973412304475069, 0.0041584921099424828, -6.6088719665757288e-06, 
                -0.0038973412304475069, 15.068651246754158), .Dim = c(4L, 4L), .Dimnames = list(
                  c("beta", "E0", "interventioneffect", "threshold"), c("beta", 
                                                                        "E0", "interventioneffect", "threshold")))
  
  SUBITS=10
  for(iteration in (-BURNIN):MCMCITS){
    if(iteration%%1000==0)cat('Iteration ',iteration,' of ',MCMCITS,': ',pars$LL,' (R0 = ',pars$beta/pars$mu,')\n',sep='') # if verbose
    for(subit in 1:SUBITS)  {
      
      oldp = pars; 
      #pars$p = rnorm(1,pars$p,0.1)
      #pars$threshold = rnorm(1,pars$threshold,20)
      delta = rmvnorm(1,c(pars$beta,pars$E0,pars$interventioneffect,pars$threshold),SIGMA)
      pars$beta = delta[1]#rnorm(1,pars$beta,0.01); pars$seir = SEIR(pars); pars = mh(oldp,pars,dat$ct3)#infection
      pars$E0 = delta[2]
      pars$interventioneffect = delta[3]
      pars$p = testingrate
      #pars$p=delta[3]
      pars$threshold=delta[4]
      
      pars$seir = SEIR(pars)
      pars = mh(oldp,pars,dat$cases_dorms)       #E0
      
    }
    if(iteration>0)
    {
      storage$beta[iteration] = pars$beta
      storage$E0[iteration] = pars$E0
      storage$interventioneffect[iteration] = pars$interventioneffect
      storage$threshold[iteration] = pars$threshold
      storage$p[iteration] = pars$p
    }
  }
  
  storage = as.data.frame(storage)
  
  write.csv(storage,paste0(mcmc_result_folder,'/mcmc4_cases_foreign_',level,'.csv'),row.names = FALSE)
}


# rm(oldp,pars,SIGMA,storage,iteration,subit,SUBITS,initialise,loglikelihood,mh,SEIR)
# rm(dat,delta)
