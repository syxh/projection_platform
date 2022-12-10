
# dat=dailydata


smoother = function(x,y)
{
  require(mgcv)
  oldd = data.frame(x=x,y=y)
  newd = data.frame(x=min(x):max(x))
  fit = gam(y~s(x,k=50),data=oldd)
  pred = as.vector(predict(fit,newdata=newd))
  pred[pred<0]=0
  return(pred)
}

for(scenarioid in 1:dim(SCENARIOS)[1])
{
  version=1
  SCENARIO = SCENARIOS[scenarioid,]
  load(paste0(folder,'/working/projections_scenario',scenarioid,'.rdata',sep=''))

  PROJECTIONS=projections

  for(KKK in 1:3)
  {
    output = matrix(0,length(PROJECTIONS$t),18)
    output[,1]=PROJECTIONS$t
    for(typeid in 1:17)
    {
      
      
      type=c('icu','deaths','ci','cc','cd','id','cci','ccc','ccd','logcci','logccc','logccd','cid','logcid','logci','logcc','logcd')[typeid]
      
      if(type=='icu'){z = PROJECTIONS$icu}
      if(type=='deaths'){z = PROJECTIONS$deaths}
      if(type=='cci'){z = PROJECTIONS$cumcases_imported}
      if(type=='ccc'){z = PROJECTIONS$cumcases_community}
      if(type=='ccd'){z = PROJECTIONS$cumcases_dorms}
      if(type=='ci'){z = PROJECTIONS$cases_imported}
      if(type=='cc'){z = PROJECTIONS$cases_community}
      if(type=='cd'){z = PROJECTIONS$cases_dorms}
      if(type=='logci'){z = PROJECTIONS$cases_imported;z[z<1]=0.5;z=log10(z);}
      if(type=='logcc'){z = PROJECTIONS$cases_community;z[z<1]=0.5;z=log10(z);}
      if(type=='logcd'){z = PROJECTIONS$cases_imported;z[z<1]=0.5;z=log10(z);}
      if(type=='id'){z = PROJECTIONS$infns_dorms}
      if(type=='logcci'){z = PROJECTIONS$cumcases_imported;z[z<1]=0.5;z=log10(z);}
      if(type=='logccc'){z = PROJECTIONS$cumcases_community;z[z<1]=0.5;z=log10(z);}
      if(type=='logccd'){z = PROJECTIONS$cumcases_dorms;z[z<1]=0.5;z=log10(z);}
      if(type=='cid'){z = PROJECTIONS$cuminfns_dorms}
      if(type=='logcid'){z = PROJECTIONS$cuminfns_dorms;z[z<1]=0.5;z=log10(z);}
      
      CI=matrix(0,dim(z)[2],3)
      for(j in 1:dim(z)[2])CI[j,]=quantile(z[,j],c(25,500,975)/1000)
      
      hat = CI[,KKK]#smoother(PROJECTIONS$t,CI[,KKK])
      if(type=='icu')hat = smoother(PROJECTIONS$t,CI[,KKK])
      if(type=='deaths')hat = smoother(PROJECTIONS$t,CI[,KKK])
      if(type=='cc')hat = smoother(PROJECTIONS$t,CI[,KKK])
      if(type=='ci')hat = smoother(PROJECTIONS$t,CI[,KKK])
      if(type=='cd')hat = smoother(PROJECTIONS$t,CI[,KKK])
      
      output[,(typeid+1)]=hat
      #plot(PROJECTIONS$t,CI[,KKK],type='l')
      #lines(PROJECTIONS$t,hat,col=2)
    }
  
    output = as.data.frame(output)
    names(output) = c("t", "icu", "deaths", "cases_imported", "cases_community", "cases_dorms", "logcases_imported", "logcases_community", "logcases_dorms","infns_dorms", 
                      "cumcases_imported", "cumcases_community", "cumcases_dorms",
                      "logcumcases_imported", "logcumcases_community", "logcumcases_dorms","cuminfns_dorms",
                      "logcuminfns_dorms")
    # if(version==1)write.csv(output,c(paste0(project_data_folder,'/projection_worst_lower.csv',sep=''),
    #                                  paste0(project_data_folder,'/projection_worst_central.csv',sep=''),
    #                                  paste0(project_data_folder,'/projection_worst_upper.csv',sep=''))[KKK],row.names = FALSE)
    # if(version==2)write.csv(output,c(paste0(project_data_folder,'/projection_base_lower.csv',sep=''),
    #                                  paste0(project_data_folder,'/projection_base_central.csv',sep=''),
    #                                  paste0(project_data_folder,'/projection_base_upper.csv',sep=''))[KKK],row.names = FALSE)
    # if(version==3)write.csv(output,c(paste0(project_data_folder,'/projection_best_lower.csv',sep=''),
    #                                  paste0(project_data_folder,'/projection_best_central.csv',sep=''),
    #                                  paste0(project_data_folder,'/projection_best_upper.csv',sep=''))[KKK],row.names = FALSE)
    stem = c(paste0(project_data_folder,'/projection_lower',sep=''),
             paste0(project_data_folder,'/projection_central',sep=''),
             paste0(project_data_folder,'/projection_upper',sep=''))[KKK]
    # fileout = paste0(stem,'_scenario',scenarioid,'.csv')
    fileout = paste0(stem,'.csv')
    write.csv(output,fileout,row.names = FALSE)
    
  }
}

# rm(CI,dat,output,PROJECTIONS,projections,version,z,hat,j,KKK,type,typeid,smoother)

