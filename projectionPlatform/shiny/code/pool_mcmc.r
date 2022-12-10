mcmc=list()
mcmc$m1 = read.csv(paste0(mcmc_result_folder,'/mcmc1_ICU.csv',sep=''))
mcmc$m2 = read.csv(paste0(mcmc_result_folder,'/mcmc2_cases_imported.csv',sep=''))
mcmc$m3 = read.csv(paste0(mcmc_result_folder,'/mcmc3_cases_local.csv',sep=''))
mcmc$m4v2 = read.csv(paste0(mcmc_result_folder,'/mcmc4_cases_foreign_high.csv',sep=''))
mcmc$m4 = read.csv(paste0(mcmc_result_folder,'/mcmc4_cases_foreign_medium.csv',sep=''))
mcmc$m4v3 = read.csv(paste0(mcmc_result_folder,'/mcmc4_cases_foreign_low.csv',sep=''))
mcmc$m5 = read.csv(paste0(mcmc_result_folder,'/mcmc5_deaths.csv',sep=''))
save('mcmc',file=paste0(mcmc_result_folder,'/mcmc.rdata',sep=''))

# mcmc = data.frame(m1d=m1$duration,
#                   m1s=m1$s,
#                   m1risk1=m1$risk1,
#                   m1risk2=m1$risk2,
#                   m1risk3=m1$risk3,
#                   m2a1=m2$a1,
#                   m2a2=m2$a2,
#                   m2a3=m2$a3,
#                   m2a4=m2$a4,
#                   m3a0=m3$a0,
#                   m3a1=m3$a1,
#                   m3a2=m3$a2,
#                   m3a3=m3$a3,
#                   m3cbe=m3$cbe,
#                   m3d1=m3$duration1,
#                   m3d2=m3$duration2,
#                   m3d3=m3$duration3,
#                   m4beta=m4$beta,
#                   m4E0=m4$E0,
#                   m4p=m4$p,
#                   m4threshold=m4$threshold,
#                   m4v2beta=m4v2$beta,
#                   m4v2E0=m4v2$E0,
#                   m4v2p=m4v2$p,
#                   m4v2threshold=m4v2$threshold,
#                   m4v3beta=m4v3$beta,
#                   m4v3E0=m4v3$E0,
#                   m4v3p=m4v3$p,
#                   m4v3threshold=m4v3$threshold,
#                   m5d=m5$duration,
#                   m5risk=m5$risk)
# rm(m1,m2,m3,m4,m4v2,m4v3,m5)

# write.csv(mcmc,'working/mcmc_all.csv',row.names=FALSE)
# rm(mcmc)
