#This Forecasting System is developed by
#     Author: Alex R Cook
#Affiliation: Saw Swee Hock School of Public Health
#             National University of Singapore
#Created Date: 13 May 2020
# Modified: 17 June 2020
# folder = c("~/Documents/nBox/Shared_covid_forecast/version_2020-05-13/") # Xiaohe
folder <<- getwd()
brand = function(file1,position="left")
{
  # require(png)
  # require(jpeg)
  # require(grid)
  filetype = strsplit(file1,'.',fixed=TRUE)[[1]][2]
  index <- as.numeric(ifelse(position=="right",1,0))
  if(filetype=='jpg')
  {
    logo = readJPEG(file1)
    grid.raster(logo,
                x = unit(abs(index+0.05), "npc"), y = unit(0.96, "npc"),
                width = unit(0.12, "npc"), height = unit(0.06, "npc") ,
                just = position)
  }else{
    logo=readPNG(file1)
    grid.raster(logo,
                x = unit(abs(index+0.05), "npc"), y = unit(0.96, "npc"),
                width = unit(0.12, "npc"), height = unit(0.06, "npc") ,
                just = position)
  }
}
credentials <- data.frame(
  user = c("alex", "moh", "alexlab"),
  password = c("alex", "moh", "alexlab"),
  stringsAsFactors = FALSE
)
SCENARIOS <<- read.csv("scenarios.csv")
####################
library(mvtnorm)
library(grid)
server = function(input, output, session)
{  
  # For password
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  
  # read in the daily data, cleaned version
  DailyData <- reactive({
    file1 = input$file1
    if(is.null(file1)){
      dailydata <- read.csv(paste0(folder,'/data/daily_data.csv'))
    }else{
      dailydata <- read.csv(file = file1$datapath, header = T)
    }
    dailydata$date = as.Date(as.character(dailydata$date),"%Y/%m/%d")
    dailydata
  })
  MobilityData <- reactive({
    file2 = input$file2
    if(is.null(file2)){
      mobilitydata <- read.csv(paste0(folder,'/data/SG_2020-09-01.csv'))
    }else{
      mobilitydata <- read.csv(file = file2$datapath, header = T)
    }
    mobilitydata <- mobilitydata[,-c(1:6)]
    mobilitydata$date <- as.Date(as.character(mobilitydata$date),"%Y-%m-%d")
    mobilitydata
    
  })
  # create the folders needed (organization)
  mcmc_result_folder <<- paste0(folder,'/working/mcmc_result')
  if(!file.exists(mcmc_result_folder)){
    dir.create(mcmc_result_folder)
  }
  project_data_folder <<- paste0(folder,'/output/data')
  if(!file.exists(project_data_folder)){
    dir.create(project_data_folder)
  }
  plot_folder <<- paste0(folder,'/output/plots')
  if(!file.exists(plot_folder)){
    dir.create(plot_folder)
  }

  # Step 1: If click 'RunMCMC' button, then the mcmc will start to run. [Could be skipped]

  
  MCMCResultGen <- eventReactive(input$runMCMC,{
    dat0 <<- DailyData()
    mobility <<- MobilityData()
    id_fill <- which(!dat0$date%in%mobility$date)
    id_fill <- which(dat0$date < as.Date("2020-02-15","%Y-%m-%d") & !dat0$date%in%mobility$date)
    x = as.data.frame(do.call('cbind', lapply(2:length(colnames(mobility)),function(i){rep(0,length(id_fill))})))
    names(x) <- colnames(mobility)[-1]
    mobility_extr <<- cbind(date=dat0$date[id_fill],x)
    mobility <<- rbind(mobility_extr,mobility)
    print(head(mobility))
    ############################
    ###### due to the mobility data, need to truncat the daily data (for mcmc3)#####
    if(nrow(dat0)>nrow(mobility)){
      dat <<- dat0[which(dat0$date%in%mobility$date),]
    }else{
      dat <<- dat0[which(mobility$date%in%dat0$date),]
    }
    print(tail(dat))
    
    mcmc_result  <- FALSE
    NFWs <<- input$NEWs
    MCMCITS <<- input$MCMCITS
    NPOP <<- 5700000-NFWs
    source(paste0(folder,'/code/functions.r',sep=''))
    # source(paste0(folder,'/code/mcmc6_rt_data.r',sep=''))
    # source(paste0(folder,'/code/mcmc7_rt.r',sep=''))
    withProgress(message = 'MCMC running, please be patient! ', value = 0, {
      incProgress(1/5, detail = paste("Doing MCMC of icu cases"))
    source(paste0(folder,'/code/mcmc1_total_icu.r',sep=''))
      incProgress(1/5, detail = paste("Doing MCMC of imported cases"))
    source(paste0(folder,'/code/mcmc2_new_cases_imported.r',sep=''))
      incProgress(1/5, detail = paste("Doing MCMC of local cases"))
    source(paste0(folder,'/code/mcmc3_new_cases_local.r',sep=''))
    
    # source(paste0(folder,'/code/sir_functions.r',sep=''))
      incProgress(1/5, detail = paste("Doing MCMC of foreign workers' cases"))
    source(paste0(folder,'/code/mcmc4_new_cases_fws.r',sep=''))
      incProgress(1/5, detail = paste("Doing MCMC of deaths"))
    source(paste0(folder,'/code/mcmc5_deaths.r',sep=''))
    n = 5
    
    })
    cat("mcmc results saved into working folder \n")
    mcmc_result = TRUE
    mcmc_result
  }) 
  message_mcmc<-reactive({
    criteria <- FALSE
    if(input$runMCMC){
      criteria <- MCMCResultGen()
      if(criteria){
        "MCMC running was done!"
      }else{
        "MCMC running, please be patient!"
      }
    }else{
      "MCMC running could be skipped. Please click Run button if you want to run MCMC!"
    }
    
  })
  output$message_mcmc <- renderText({
  message_mcmc()
})

  # After this step, pls make sure you have a folder named 'working' to save all the processing results, 

  
  MCMCResultRead <- reactive({
    source(paste0(folder,'/code/pool_mcmc.r',sep=''))
    mcmc
  })
  
  
  Projector <-  eventReactive(input$analyse,{
    dat <<- as.data.frame(DailyData())
    projectionwindow <<- input$projectionwindow
    NFWs <<- input$NEWs
    NPOP <<- 5700000-NFWs
    dat0 <<- DailyData()
    mobility <<- MobilityData()
    id_fill <- which(!dat0$date%in%mobility$date)
    id_fill <- which(dat0$date < as.Date("2020-02-15","%Y-%m-%d") & !dat0$date%in%mobility$date)
    x = as.data.frame(do.call('cbind', lapply(2:length(colnames(mobility)),function(i){rep(0,length(id_fill))})))
    names(x) <- colnames(mobility)[-1]
    mobility_extr <<- cbind(date=dat0$date[id_fill],x)
    mobility <<- rbind(mobility_extr,mobility)
    print(head(mobility))
    ############################
    ###### due to the mobility data, need to truncat the daily data (for mcmc3)#####
    if(nrow(dat0)>nrow(mobility)){
      dat <<- dat0[which(dat0$date%in%mobility$date),]
    }else{
      dat <<- dat0[which(mobility$date%in%dat0$date),]
    }
    print(tail(dat))
    if(is.null(paste0(mcmc_result_folder,sep=""))){stop("Please check the MCMC results in the working folder.")}
    mcmc <- MCMCResultRead()
    if(!input$runMCMC){
      MCMCITS <<- nrow(mcmc[[1]])
      print(MCMCITS)
    }else{
      MCMCITS <<- input$MCMCITS
    }
    project_data_message <- FALSE
    source(paste0(folder,'/code/sir_functions.r',sep=''))
    # MCMCITS=100000
    source('code/mobility_prediction_transit.R')
    source('code/mobility_prediction_resident.R')
    source('code/mobility_prediction_grocery.R')
    source('code/mobility_prediction_parks.R')
    source('code/mobility_prediction_works.R')
    source('code/mobility_prediction_retail.R')
  
    source(paste0(folder,'/code/projector.r',sep=''))
    withProgress(message = 'Projecting running, please be patient! ', value = 0, {
    for(scenarioid in 1)#:dim(SCENARIOS)[1])
    {      
      # incProgress(1/dim(SCENARIOS)[1], detail = paste("Doing Projecting scenario ",scenarioid,sep=""))

      cat('Projecting scenario',scenarioid,'\n')
      version=1 # redundant
      SCENARIO = SCENARIOS[scenarioid,]
      p=projector(100,projectionwindow,SCENARIO)
      sampleit = seq(10,MCMCITS,10)
      #sampleit = seq(1,MCMCITS,1)
      M=matrix(0,length(sampleit),length(p$icu))
      output = list(t=p$doy,icu=M,deaths=M,
                    cases_imported=M,cases_community=M,cases_dorms=M,infns_dorms=M,cuminfns_dorms=M,
                    cumcases_imported=M,cumcases_community=M,cumcases_dorms=M)
      
      for(i in seq(sampleit))
      {
        p=projector(sampleit[i],projectionwindow,SCENARIO)
        output$icu[i,]=p$icu
        output$deaths[i,]=p$deaths
        output$cumcases_imported[i,]=cumsum(p$cases_imported)
        output$cumcases_community[i,]=cumsum(p$cases_community)
        output$cumcases_dorms[i,]=cumsum(p$cases_dorms)
        output$cases_imported[i,]=(p$cases_imported)
        output$cases_community[i,]=(p$cases_community)
        output$cases_dorms[i,]=(p$cases_dorms)
        output$infns_dorms[i,]=p$infns_dorms
        output$cuminfns_dorms[i,]=cumsum(p$infns_dorms)
      }
      projections = output
      #projections
      # if(version==1)save('projections',file = paste0(folder,'/working/projections.rdata',sep=''))
      # if(version==2)save('projections',file = paste0(folder,'/working/projections_v2.rdata',sep=''))
      # if(version==3)save('projections',file = paste0(folder,'/working/projections_v3.rdata',sep=''))
      save('projections',file = paste0(folder,'/working/projections_scenario',scenarioid,'.rdata'))
    }
      
      })
    
    # cat("projection results saved \n")
    
    source(paste0(folder,'/code/process_mcmc_output.r',sep=''))
    
    # cat("project data prepared \n")
    project_data_message <- TRUE
    return(project_data_message)
  })
  message_proj<-reactive({
    if(input$analyse){
      criteria_pr <- Projector()
      if(criteria_pr){
        if(input$runMCMC){
          "Projection step was calculated according to the updated MCMC results."
        }else{
          "Projection step was calculated according to previous MCMC results."
        }
        # "Projection step was done!"
      }else{"Projection is running..."}
    }else{
      "Please click Analyse button!"
    }

}
)
  output$message_proj <- renderText(
    message_proj()
  )
  
  # observe(input$analyse,{
  #   fs <- list.files(project_data_folder)
  #   file.remove(paste0(project_data_folder,"/",fs))
  # 
  # })


  Plotter <- eventReactive(input$plot,{
      plots <- FALSE
      scenarioid <- 1#as.numeric(as.character(input$scenarioid))
      # version <- as.numeric(input$version)
      group <<- as.character(input$group)
      dat <<- DailyData()
      # source(paste0(folder,'/code/plotter.r',sep=''))

# plotter -----------------------------------------------------------------

      # load(paste0(folder,'/working/projections.rdata',sep=''))
      xaxes2=function(xlm)
      {
        
        # Mo-Fr
        xtk1 = as.Date('2020-01-06')-as.Date('2019-12-31')+14*seq(0,30,1)-0.5
        xtk2 = xtk1+7
        
        i=which((xtk1>xlm[1])&(xtk2<xlm[2]))
        xtk1=xtk1[i]
        xtk2=xtk2[i]
        
        for(k in seq(xtk1))grid.polygon(unit(c(xtk1[k],xtk1[k],xtk2[k],xtk2[k]),'native'),unit(c(0,1,1,0),'npc'),
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
        
        
        
        
        
        
        xtk = as.Date(c('2020-02-01','2020-03-01','2020-04-01','2020-05-01','2020-06-01','2020-07-01',
                        '2020-08-01','2020-09-01','2020-10-01','2020-11-01','2020-12-01'))-as.Date('2019-12-31')
        
        grid.xaxis(at=xtk,label=FALSE,main = TRUE)
        grid.text('J',x=unit(0.5+as.Date('2020-01-25')-as.Date('2019-12-31'),'native'),y=unit(-1,'lines'))
        grid.text('F',x=unit(0.0+as.Date('2020-02-15')-as.Date('2019-12-31'),'native'),y=unit(-1,'lines'))
        grid.text('M',x=unit(0.5+as.Date('2020-03-15')-as.Date('2019-12-31'),'native'),y=unit(-1,'lines'))
        grid.text('A',x=unit(0.0+as.Date('2020-04-15')-as.Date('2019-12-31'),'native'),y=unit(-1,'lines'))
        grid.text('M',x=unit(0.0+as.Date('2020-05-15')-as.Date('2019-12-31'),'native'),y=unit(-1,'lines'))
        grid.text('J',x=unit(0.0+as.Date('2020-06-15')-as.Date('2019-12-31'),'native'),y=unit(-1,'lines'))
        grid.text('J',x=unit(0.0+as.Date('2020-07-15')-as.Date('2019-12-31'),'native'),y=unit(-1,'lines'))
        grid.text('A',x=unit(0.0+as.Date('2020-08-15')-as.Date('2019-12-31'),'native'),y=unit(-1,'lines'))
        grid.text('S',x=unit(0.0+as.Date('2020-09-15')-as.Date('2019-12-31'),'native'),y=unit(-1,'lines'))
        grid.text('O',x=unit(0.0+as.Date('2020-10-15')-as.Date('2019-12-31'),'native'),y=unit(-1,'lines'))
        grid.text('N',x=unit(0.0+as.Date('2020-11-15')-as.Date('2019-12-31'),'native'),y=unit(-1,'lines'))
        grid.text('D',x=unit(0.0+as.Date('2020-12-15')-as.Date('2019-12-31'),'native'),y=unit(-1,'lines'))
        
        
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
                          scenarioid=1)
      {
        # oC = read.csv(paste0(project_data_folder,'/projection_central_scenario',scenarioid,'.csv',sep=''))
        # oL = read.csv(paste0(project_data_folder,'/projection_lower_scenario',scenarioid,'.csv',sep=''))
        # oU = read.csv(paste0(project_data_folder,'/projection_upper_scenario',scenarioid,'.csv',sep=''))
        oC = read.csv(paste0(project_data_folder,'/projection_central.csv',sep=''))
        oL = read.csv(paste0(project_data_folder,'/projection_lower.csv',sep=''))
        oU = read.csv(paste0(project_data_folder,'/projection_upper.csv',sep=''))
        
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
        # xlm = range(projections$t)
        xlm = range(oC$t)
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
        # if(scenarioid==2)mastercol= '#003171'
        # if(scenarioid==3)mastercol= '#607c3c'
        # if(scenarioid==4)mastercol= '#674e81'
        # if(scenarioid==5)mastercol= '#272323'
        # if(scenarioid==6)mastercol= '#ff3562'
        
        
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
        paneller('icu',scenarioid)
        popViewport()
        pushViewport(viewport(layout.pos.col=1,layout.pos.row=2))
        paneller('deaths',scenarioid)
        popViewport()
        popViewport()
      }
      if(group=='group2'){
        pushViewport(viewport(layout = grid.layout(2,2)))
        pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
        paneller('ci',scenarioid)
        popViewport()
        
        pushViewport(viewport(layout.pos.col=2,layout.pos.row=1))
        paneller('cc',scenarioid)
        popViewport()
        
        pushViewport(viewport(layout.pos.col=1,layout.pos.row=2))
        paneller('cd',scenarioid)
        popViewport()
        
        pushViewport(viewport(layout.pos.col=2,layout.pos.row=2))
        paneller('truecd',scenarioid)
        popViewport()
        popViewport()
      }
      
      if(group=='group3'){
        pushViewport(viewport(layout = grid.layout(2,2)))
        pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
        paneller('cci',scenarioid)
        popViewport()
        
        pushViewport(viewport(layout.pos.col=2,layout.pos.row=1))
        paneller('ccc',scenarioid)
        popViewport()
        
        pushViewport(viewport(layout.pos.col=1,layout.pos.row=2))
        paneller('ccd',scenarioid)
        popViewport()
        
        pushViewport(viewport(layout.pos.col=2,layout.pos.row=2))
        paneller('trueccd',scenarioid)
        popViewport()
        popViewport()
      }
      
      if(group=='group4'){
        pushViewport(viewport(layout = grid.layout(2,2)))
        pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
        paneller('logcci',scenarioid)
        popViewport()
        
        pushViewport(viewport(layout.pos.col=2,layout.pos.row=1))
        paneller('logccc',scenarioid)
        popViewport()
        
        pushViewport(viewport(layout.pos.col=1,layout.pos.row=2))
        paneller('logccd',scenarioid)
        popViewport()
        
        pushViewport(viewport(layout.pos.col=2,layout.pos.row=2))
        paneller('logtrueccd',scenarioid)
        popViewport()
        popViewport()
      }
      
      rm(paneller,xaxes2,yaxis)
      
#####################      
      plots <- TRUE
      plots
    }
  )
  zoom = 1.5
  output$plot <- renderPlot({
    Plotter()
    ## How to arrange the plots? want to plot in one page or different panel?
  },height=550*zoom, width = 550*zoom)
  
  message_plot<-reactive({
    if(input$plot){
      criteria_p <- Plotter()
      if(criteria_p){
        if(input$analyse){
          "Plots were based on the updated projection results. Please click Plot button if you change the plot setting."
        }else{
          "Plots were based on the previous projection results. Please click Plot button if you change the plot setting."
        }
      }else{"Plotting..."}
    }else{
      "Please click Plot button for each setting!"
    }

  })

  output$message_plot <- renderText(
    message_plot()
  )

  updateSelectInput(session,'downfile',label=NULL, choices = list.files(
    # paste0(folder,'/output/data/',sep='')
    project_data_folder
    ))
  projection_data <- reactive({
    fileName <- input$downfile
    # read.csv(paste0(folder,'/output/data/',fileName,sep=''))
    read.csv(paste0(project_data_folder,'/',fileName,sep=''))
    
  })
  process_download <- reactive({
    dat <- projection_data()
    log_col <- c(11:13,15)
    dat <- dat[,-log_col]
    round_col <- c(2,4:11)
    dat[,round_col] <- round(dat[,round_col],0)
    dat
  })
  output$downloadData = downloadHandler(
    filename=function(){
      paste(Sys.Date(),"_",input$downfile, sep = "")
      
    },
    content=function(file){write.csv(process_download(),file,row.names = F, na="")}
  )
}