# setwd("~/nBox/Shared_covid_forecast/version_2020-05-15/")
NFWs=323000
NPOP=5700000-NFWs
MCMCITS=10000
SCENARIOS = read.csv('scenarios.csv')

dailydata = read.csv('data/daily_data.csv',as.is=TRUE)
tail(dailydata)
dailydata$date=as.Date(dailydata$date,format = '%Y/%m/%d')


mobility = read.csv("data/SG_2020-09-01.csv")
mobility = mobility[,-c(1:6)]
tail(mobility)
mobility$date <- as.Date(as.character(mobility$date),"%Y-%m-%d")
# From Jan 19 till Feb 14, fill the mobility information as 0
id_fill <- which(dailydata$date < as.Date("2020-02-15","%Y-%m-%d") & !dailydata$date%in%mobility$date)
x = as.data.frame(do.call('cbind', lapply(2:length(colnames(mobility)),function(i){rep(0,length(id_fill))})))
names(x) <- colnames(mobility)[-1]
mobility_extr <- cbind(date=dailydata$date[id_fill],x)
mobility <- rbind(mobility_extr,mobility)
rm("id_fill","mobility_extr",'x')

if(nrow(dailydata)>nrow(mobility)){
  dailydata <- dailydata[which(dailydata$date%in%mobility$date),]
}else{
  dailydata <- dailydata[which(mobility$date%in%dailydata$date),]
  mobility <- mobility[which(mobility$date%in%dailydata$date),]
}
tail(dailydata)
dat=dailydata
folder = getwd()
mcmc_result_folder = paste0(folder,'/working/mcmc_result')
project_data_folder = paste0(folder,'/output/data')
plot_folder = paste0(folder,'/output/plots')
projectionwindow=60
library(mvtnorm)
library(lubridate)
source('code/functions.r')

source('code/mcmc1_total_icu.r')
source('code/mcmc2_new_cases_imported.r')

source('code/mcmc3_new_cases_local.r')
source('code/mcmc4_new_cases_fws.r')
source('code/mcmc5_deaths.r')

source('code/pool_mcmc.r')
MCMCITS=100000
source('code/mobility_prediction_transit.R')
source('code/mobility_prediction_resident.R')
source('code/mobility_prediction_grocery.R')
source('code/mobility_prediction_parks.R')
source('code/mobility_prediction_works.R')
source('code/mobility_prediction_retail.R')
MCMCITS=10000
source('code/sir_functions.r')
source('code/projector.r')
source('code/process_mcmc_output.r')
# source('code/plotter.r')

