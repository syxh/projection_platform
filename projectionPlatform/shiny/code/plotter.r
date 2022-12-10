library(grid)
load(paste0(folder,'/working/projections.rdata',sep=''))
# if(version==1)load(paste0(folder,'/working/projections.rdata',sep=''))
# if(version==2)load(paste0(folder,'/working/projections_v2.rdata',sep=''))
# if(version==3)load(paste0(folder,'/working/projections_v3.rdata',sep=''))

xaxes2=function(xlm)
{
  
  # Mo-Fr
  xtk1 = as.Date('2020-01-06')-as.Date('2019-12-31')+14*seq(0,30,1)-0.5
  xtk2 = xtk1+7
  
  i=which((xtk1>xlm[1])&(xtk2<xlm[2]))
  xtk1touse=xtk1[i]
  xtk2touse=xtk2[i]
  i=which((xtk1<xlm[2])&(xtk2>=xlm[2]))
  if(length(i)>0)
  {
    xtk1touse=c(xtk1touse,xtk1[i])
    xtk2touse=c(xtk2touse,xlm[2])
  }
  
  for(k in seq(xtk1touse))grid.polygon(unit(c(xtk1touse[k],xtk1touse[k],xtk2touse[k],xtk2touse[k]),'native'),
                                       unit(c(0,1,1,0),'npc'),
                                       gp=gpar(col=NA,fill='#f0f7fe'))
  
  #sat
  # xtk1 = as.Date('2020-01-04')-as.Date('2019-12-31')+7*seq(0,30,1)-0.5
  # xtk2 = xtk1+1
  # 
  # i=which((xtk1>xlm[1])&(xtk2<xlm[2]))
  # xtk1=xtk1[i]
  # xtk2=xtk2[i]
  # 
  # for(k in seq(xtk1))grid.polygon(unit(c(xtk1[k],xtk1[k],xtk2[k],xtk2[k]),'native'),unit(c(0,1,1,0),'npc'),
  #                                 gp=gpar(col=NA,fill='#f0f7fe'))
  # #sun
  # xtk1 = as.Date('2020-01-05')-as.Date('2019-12-31')+7*seq(0,30,1)-0.5
  # xtk2 = xtk1+1
  # 
  # i=which((xtk1>xlm[1])&(xtk2<xlm[2]))
  # xtk1=xtk1[i]
  # xtk2=xtk2[i]
  # 
  # for(k in seq(xtk1))grid.polygon(unit(c(xtk1[k],xtk1[k],xtk2[k],xtk2[k]),'native'),unit(c(0,1,1,0),'npc'),
  #                                 gp=gpar(col=NA,fill='#f0f7fe'))
  
  
  
  
  
  
  xtk = as.Date(c('2020-02-01','2020-03-01','2020-04-01','2020-05-01','2020-06-01','2020-07-01','2020-08-01','2020-09-01','2020-10-01'))-as.Date('2019-12-31')
  xtk = xtk[xtk<=xlm[2]]
  grid.xaxis(at=xtk,label=FALSE,main = TRUE)
  grid.text('J',x=unit(0.5+as.Date('2020-01-22')-as.Date('2019-12-31'),'native'),y=unit(-1,'lines'))
  grid.text('F',x=unit(0.0+as.Date('2020-02-15')-as.Date('2019-12-31'),'native'),y=unit(-1,'lines'))
  grid.text('M',x=unit(0.5+as.Date('2020-03-15')-as.Date('2019-12-31'),'native'),y=unit(-1,'lines'))
  grid.text('A',x=unit(0.0+as.Date('2020-04-15')-as.Date('2019-12-31'),'native'),y=unit(-1,'lines'))
  grid.text('M',x=unit(0.0+as.Date('2020-05-15')-as.Date('2019-12-31'),'native'),y=unit(-1,'lines'))
  if(xlm[2]>(as.Date('2020-06-15')-as.Date('2019-12-31')))grid.text('J',x=unit(0.0+as.Date('2020-06-15')-as.Date('2019-12-31'),'native'),y=unit(-1,'lines'))
  if(xlm[2]>(as.Date('2020-07-15')-as.Date('2019-12-31')))grid.text('J',x=unit(0.0+as.Date('2020-07-15')-as.Date('2019-12-31'),'native'),y=unit(-1,'lines'))
  if(xlm[2]>(as.Date('2020-08-15')-as.Date('2019-12-31')))grid.text('A',x=unit(0.0+as.Date('2020-08-15')-as.Date('2019-12-31'),'native'),y=unit(-1,'lines'))
  if(xlm[2]>(as.Date('2020-09-15')-as.Date('2019-12-31')))grid.text('S',x=unit(0.0+as.Date('2020-09-15')-as.Date('2019-12-31'),'native'),y=unit(-1,'lines'))
  
  
}


yaxis = function(ylm,log=FALSE)
{
  if(!log)
  {
    ytk = pretty(ylm)
    ylb = as.character(ytk)
    for(ij in seq(ylb)){if(ytk[ij]>9999)ylb[ij]=paste0(ytk[ij]/1000,'k')}
    ij=which(ytk<ylm[2])
    ytk=ytk[ij]
    ylb=ylb[ij]
    grid.yaxis(ytk,ylb)
  }
  if(log)
  {
    ytk = 0:10#pretty(ylm)
    ylb = as.character(10^ytk)
    for(ij in seq(ylb)){if(ytk[ij]>3)ylb[ij]=paste0(10^ytk[ij]/1000,'k')}
    ylb[ytk<0]='0'
    ij=which(ytk<=ylm[2])
    ytk=ytk[ij]
    ylb=ylb[ij]
    ij=which(ytk>=ylm[1])
    ytk=ytk[ij]
    ylb=ylb[ij]
    
    grid.yaxis(ytk,ylb)
    z=1:9
    Z = log10(c(z,10*z,100*z,1000*z,10000*z,100000*z))
    Z = Z[Z<ylm[2]]
    if(length(Z)>0)grid.yaxis(at=Z,label=FALSE,gp=gpar(cex=0.5))
  }
  
}

smoother = function(x,y)
{
  require(mgcv)
  oldd = data.frame(x=x,y=y)
  newd = data.frame(x=min(x):max(x))
  fit = gam(y~s(x,k=20),data=oldd)
  pred = as.vector(predict(fit,newdata=newd))
  pred[pred<0]=0
  return(pred)
}

paneller = function(type,
                    # file,
                    version = version)
{
  oC = read.csv(paste0(project_data_folder,'/projection_worst_central.csv',sep=''))
  oL = read.csv(paste0(project_data_folder,'/projection_worst_lower.csv',sep=''))
  oU = read.csv(paste0(project_data_folder,'/projection_worst_upper.csv',sep=''))
  
  if(version==2)
  {
    oC = read.csv(paste0(project_data_folder,'/projection_base_central.csv',sep=''))
    oL = read.csv(paste0(project_data_folder,'/projection_base_lower.csv',sep=''))
    oU = read.csv(paste0(project_data_folder,'/projection_base_upper.csv',sep=''))
  }
  if(version==3)
  {
    oC = read.csv(paste0(project_data_folder,'/projection_best_central.csv',sep=''))
    oL = read.csv(paste0(project_data_folder,'/projection_best_lower.csv',sep=''))
    oU = read.csv(paste0(project_data_folder,'/projection_best_upper.csv',sep=''))
  }
  
  dailydata = dat #xh
  
  da = list(t=dailydata$doy,
            icu = dailydata$icu,
            deaths = dailydata$deaths,
            cases_imported = dailydata$cases_imported,
            cases_community = dailydata$cases_community,
            cases_dorms = dailydata$cases_dorms,
            infns_dorms = dailydata$cases_dorms,# lies all lies
            cumcases_imported = cumsum(dailydata$cases_imported),
            cumcases_community = cumsum(dailydata$cases_community),
            cumcases_dorms = cumsum(dailydata$cases_dorms)
  )
  z=da$cumcases_imported;z[z<1]=0.5;z=log10(z)
  da$logcumcases_imported=z
  z=da$cumcases_community;z[z<1]=0.5;z=log10(z)
  da$logcumcases_community=z
  z=da$cumcases_dorms;z[z<1]=0.5;z=log10(z)
  da$logcumcases_dorms=z
  da$cumcases_dorms2 = cumsum(dailydata$cases_dorms)
  z=da$cumcases_dorms2;z[z<1]=0.5;z=log10(z)
  da$logcumcases_dorms2=z
  da = as.data.frame(da)
  
  
  # png(file,height=8,width=8,units='cm',res=300,pointsize=10)
  xlm = range(projections$t)
  logy=FALSE
  top=TRUE
  
  k=0
  if(type=='icu'){k=2;ylabel='In ICU'}
  if(type=='deaths'){k=3;ylabel='Deceased'}
  if(type=='cci'){k=8;ylabel='Total cases (imported)';top=FALSE}
  if(type=='ccc'){k=9;ylabel='Total cases (local)';top=FALSE}
  if(type=='ccd'){k=10;ylabel='Total cases (FWs)';top=TRUE}
  if(type=='ci'){k=4;ylabel='New cases (imported)'}
  if(type=='cc'){k=5;ylabel='New cases (local)'}
  if(type=='cd'){k=6;ylabel='New cases (FWs)'}
  if(type=='logcci'){k=11;ylabel='Total cases (imported)';logy=TRUE;top=FALSE}
  if(type=='logccc'){k=12;ylabel='Total cases (local)';logy=TRUE;top=FALSE}
  if(type=='logccd'){k=13;ylabel='Total cases (FWs)';logy=TRUE;top=FALSE}
  if(type=='truecd'){k=7;ylabel='New infections (FWs)'}
  if(type=='trueccd'){k=14;ylabel='Total infections (FWs)'}
  if(type=='logtrueccd'){k=15;ylabel='Total infections (FWs)';logy=TRUE;top=FALSE}
  
  projC = oC[,k]
  projL = oL[,k]
  projU = oU[,k]
  truth = da[,k]
  projx = oC[,1]
  truthx  = da[,1]
  ylm=c(0,max(c(projU,truth))*1.05)
  if(logy)ylm[1]=min(c(projL,truth))*0.95
  pushViewport(plotViewport(c(4,4,1,1),
                            xscale=xlm,yscale=ylm))
  xaxes2(xlm)
  
  mastercol = '#ed2939'
  if(version==2)mastercol= '#003171'
  if(version==3)mastercol= '#607c3c'
  
  
  q=as.vector(col2rgb(mastercol))/255
  colbg = rgb(q[1],q[2],q[3],0.5)
  
  grid.polygon(c(projx,rev(projx)),c(projL,rev(projU)),
               default.units = 'native',gp=gpar(col=NA,fill=colbg))
  grid.lines(projx,projC,default.units = 'native',gp=gpar(col=mastercol,lwd=2))
  
  grid.points(truthx,truth,gp=gpar(cex=0.5),pch=16)
  
  
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  if(!logy)yaxis(ylm)
  if(logy)yaxis(ylm,log = TRUE)
  if(top)grid.text(ylabel,x=unit(0.5,'lines'),y=unit(1,'npc')-unit(1,'lines'),hjust=0)
  if(!top)grid.text(ylabel,x=unit(1,'npc')+unit(-0.5,'lines'),y=unit(0,'npc')+unit(1,'lines'),hjust=1)
  popViewport()
  # dev.off()
  
}

if(group=="group1"){
  pushViewport(viewport(layout = grid.layout(2,2)))
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
  paneller('icu',version = version)
  popViewport()
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=2))
  paneller('deaths',version = version)
  popViewport()
  popViewport()
}
if(group=='group2'){
  pushViewport(viewport(layout = grid.layout(2,2)))
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
  paneller('ci',version = version)
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2,layout.pos.row=1))
  paneller('cc',version = version)
  popViewport()
  
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=2))
  paneller('cd',version = version)
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2,layout.pos.row=2))
  paneller('truecd',version = version)
  popViewport()
  popViewport()
}

if(group=='group3'){
  pushViewport(viewport(layout = grid.layout(2,2)))
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
  paneller('cci',version = version)
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2,layout.pos.row=1))
  paneller('ccc',version = version)
  popViewport()
  
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=2))
  paneller('ccd',version = version)
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2,layout.pos.row=2))
  paneller('trueccd',version = version)
  popViewport()
  popViewport()
}

if(group=='group4'){
  pushViewport(viewport(layout = grid.layout(2,2)))
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
  paneller('logcci',version = version)
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2,layout.pos.row=1))
  paneller('logccc',version = version)
  popViewport()
  
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=2))
  paneller('logccd',version = version)
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2,layout.pos.row=2))
  paneller('logtrueccd',version = version)
  popViewport()
  popViewport()
}

rm(paneller,xaxes2,yaxis,projections)
