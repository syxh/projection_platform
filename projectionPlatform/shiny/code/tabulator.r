library(grid)

dat=dailydata


extractor = function(type,file,scenarioid=1)
{
  oC = read.csv(paste0(project_data_folder,'/projection_central_scenario',scenarioid,'.csv'))
  oL = read.csv(paste0(project_data_folder,'/projection_lower_scenario',scenarioid,'.csv'))
  oU = read.csv(paste0(project_data_folder,'/projection_upper_scenario',scenarioid,'.csv'))
  
  
  k=0
  if(type=='icu'){k=2;ylabel='In ICU'}
  if(type=='deaths'){k=3;ylabel='Deceased'}
  if(type=='cci'){k=8;ylabel='Total cases (imported)';top=FALSE}
  if(type=='ccc'){k=9;ylabel='Total cases (local)';top=TRUE}
  if(type=='ccd'){k=10;ylabel='Total cases (FWs)';top=TRUE}
  if(type=='ci'){k=4;ylabel='New cases (imported)'}
  if(type=='cc'){k=5;ylabel='New cases (local)'}
  if(type=='cd'){k=6;ylabel='New cases (FWs)'}
  if(type=='logcci'){k=11;ylabel='Total cases (imported)';logy=TRUE;top=FALSE}
  if(type=='logccc'){k=12;ylabel='Total cases (local)';logy=TRUE;top=TRUE}
  if(type=='logccd'){k=13;ylabel='Total cases (FWs)';logy=TRUE;top=FALSE}
  if(type=='truecd'){k=7;ylabel='New infections (FWs)'}
  if(type=='trueccd'){k=14;ylabel='Total infections (FWs)'}
  
  projC = oC[,k]
  projL = oL[,k]
  projU = oU[,k]
  projx = oC[,1]

  output = data.frame(t=projx,
                      estimate=round(projC,2),
                      CIL=round(projL,2),
                      CIU=round(projU,2))
  
  return(output)
}

for(scenarioid in 1:dim(SCENARIOS)[1])
{
  e01=extractor('icu',scenarioid)
  e02=extractor('deaths',scenarioid)
  
  e03=extractor('ci',scenarioid)
  e04=extractor('cc',scenarioid)
  e05=extractor('cd',scenarioid)
  
  e06=extractor('logcci',scenarioid)
  e07=extractor('logccc',scenarioid)
  e08=extractor('logccd',scenarioid)
  
  e09=extractor('cci',scenarioid)
  e10=extractor('ccc',scenarioid)
  e11=extractor('ccd',scenarioid)
  
  e12=extractor('truecd',scenarioid)
  e13=extractor('trueccd',scenarioid)
  
  output = data.frame(
    t=e01$t,
    ICU=e01$estimate,
    ICUlower=e01$CIL,
    ICUupper=e01$CIU,
    deaths=e02$estimate,
    deathslower=e02$CIL,
    deathsupper=e02$CIU,
    imported=e03$estimate,
    importedlower=e03$CIL,
    importedupper=e03$CIU,
    community=e04$estimate,
    communitylower=e04$CIL,
    communityupper=e04$CIU,
    dorms=e05$estimate,
    dormslower=e05$CIL,
    dormsupper=e05$CIU,
    cum_imported=e09$estimate,
    cum_importedlower=e09$CIL,
    cum_importedupper=e09$CIU,
    cum_community=e10$estimate,
    cum_communitylower=e10$CIL,
    cum_communityupper=e10$CIU,
    cum_dorms=e11$estimate,
    cum_dormslower=e11$CIL,
    cum_dormsupper=e11$CIU,
    dorm_infections=e12$estimate,
    dorm_infectionslower=e12$CIL,
    dorm_infectionsupper=e12$CIU,
    cum_dorm_infections=e13$estimate,
    cum_dorm_infectionslower=e13$CIL,
    cum_dorm_infectionsupper=e13$CIU
  )
  
  write.csv(output,paste0(project_data_folder,'/'))
}

rm(extractor)
rm(dat,smoother)
