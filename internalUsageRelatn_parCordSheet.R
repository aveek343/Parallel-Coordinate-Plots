################################################################################################################
################################################Purpose of this Script##########################################
################################################################################################################
# This script is written to check the relationship between different factors related to shower end use and Average
# and/or total water use for entire REUS-2 dataset 

################################################################################################################
## Data export
# daily.use.2016 is the data frame imported from the WRF-Residential End Uses of Water 2016 Database's REU2016_Daily_Use_Main
# table. It can be done by using RODBC package of R
# RODBC does not work well in a 64 bit operating system, additional driver must be downloaded before using RODBC package
# Details are in the "https://stackoverflow.com/questions/13070706/how-to-connect-r-with-access-database-in-64-bit-window"
# discussion page and follow the discussion of Aren Cambre at the bottom. That worked for me. The additional driver can be
# found here: "https://www.microsoft.com/en-us/download/details.aspx?id=54920"
# or, if the operating system and/or rstudio is 64-bit and MS office products are in 32-bit version, it will not be able to use RODBC library
# we can just export the table to an excel format file and use it.
# for this script, I exported the table to an excel format file and used that to create a dataframe in R
# Created by: Mahmudur Rahman Aveek
# -----------------------------------------------------------------------------------------------------------------------------

## Setting the working directory
setwd("D:\\R\\R\\Parallel Coordinate Plots")
## required packages for excel files; we will create excel files for checking the data
library (readxl)
library (xlsx)
library (plotly)
a <- read_excel("REU2016_Survey - End Use Sample.xlsx")

# creating a dataframe consisting of the different water end-uses volume
a.wat.use <-a[,c(1,172,173,182:189)]


# checking if the average valuesa are correct;


# omitting all NA cells
a.wat.use <-na.omit(a.wat.use)

##############################################################################################################
# Ranking as per the total water use; 1st as per indoor trace and then outdoor trace. We can do a combined usage rank but,
# it will not reflect the actual scenario as not all HH was monitored during same season. Also, definition of season was 
# quite vague in Residential End Uses of Water 2016 report.

# ranking as per indoor trace (i.e., indoor average use per HH)
a.wat.use$rankIndoorAvg <- rank(a.wat.use$TraceIndoorDaily)

# ranking as per outdoor trace (i.e., outdoor average use per HH)
a.wat.use$rankOutdoorAvg <- rank(a.wat.use$TraceOutdoorDaily)

# ranking as per bathtub use
a.wat.use$rankBathtubAvg <- rank(a.wat.use$BathtubDaily)

# ranking as per clotheswasher use
a.wat.use$rankClothesWasherAvg <- rank(a.wat.use$ClothesWasherDailyVolume)

# ranking as per dishwasher use
a.wat.use$rankDishWasherAvg <- rank(a.wat.use$DishwasherDaily)

# ranking as per Faucet use
a.wat.use$rankFaucetVolAvg <- rank(a.wat.use$FaucetDaily)

# ranking as per Leak
a.wat.use$rankLeakVolAvg <- rank(a.wat.use$LeakDaily)

# ranking as per uncategorized other use
a.wat.use$rankOtherAvg <- rank(a.wat.use$OtherDaily)

# ranking as per Shower use
a.wat.use$rankShowerVolAvg <- rank(a.wat.use$ShowerDailyVolume)

# ranking as per Tolet use
a.wat.use$rankToiletVolAvg <- rank(a.wat.use$ToiletDailyVolume)


# writing an excel file for better visualization
write.xlsx(a.wat.use,file="internalUsageRelatn.xlsx")


################################################################################################################################
# Checking the usage inequality among different HH for different appliances. Considering all of the outdoor use was for irrigation
# The gini function is not yet updated, it was copied from a bigger function that checked the log-normality among different end-use
# The function will be updated soon. For now, we will use this clumsy function.
# we will have to run the function for each end-use (appliance use)
# Gini values those we will get from the function is hardcoded in the legend at the last plot.
library(ineq)
#gini coefficient

#creating a data frame for gini coefficient calculation
# Description of variables:
#   hh_col= population data
#   h1= the data set
#   file_name= name of the exported excel file
#   lab_his_x= label for x-axis in th1e histogram (e.g. gal or gal/day)
#   lab_his_main = Main h1eader for th1e histogram
#   qqmain= header for qq plot
#   trans_log_main= Main header for the log transformed graph
#   log_norm_main= Main header for log normal graph
giniFunction<-function (hh_col,h,file_name,lab_his_x='',lab_his_main='',qqmain='', trans_log_main='', log_norm_main='')
{   
  
  
  popu_data= data.frame(hh_col,h)
  names(popu_data)[2]<-"usage"
  #View(popu_data)
  #checking for cells with 0 values and removing them
  for (i in popu_data){
    if (0 %in% popu_data$usage){
      
      popu_data_1= apply(popu_data,1,function(row)all(row!=0))
      popu_data[popu_data_1,]
      popu_data<-data.frame(popu_data[popu_data_1,])
      
    }
    
    
    gini_df<- data.frame(popu_data)
    names(gini_df)[1]<-"hh_data"
    
    names(gini_df)[2]<-"usage"
    #gini_df<- gini_df%>%group_by(gini_df$hh_data)%>% summarize_all(sum)
    gini_df <- aggregate(gini_df$usage, by=list(hh_data=gini_df$hh_data), FUN=sum)
    
    gini_df$hh_data<-1
    names(gini_df)[1]<-"hh_no"
    names(gini_df)[2]<-"usage"
    #View(gini_df)
    #creating an additional column for House hold number
    #gini_df$hh_no<-ifelse(gini_df$hh_keycode>0,1,0)
    #rearranging column index
    #gini_df<-gini_df[c(1,3,2)]
    #creating the % columns for house hold and usage
    gini_df=mutate(gini_df,hh_pct=hh_no/sum(hh_no)*100,usage_pct=usage/sum(usage)*100)
    #ranking the data according to usage_pct
    gini_df<- gini_df[order(gini_df$usage_pct),]
    #creating cumulative % columns
    gini_df$cum_hh_pct<-cumsum(gini_df$hh_pct)
    gini_df$cum_usage_pct<-cumsum(gini_df$usage_pct)
    data_frame_gini<<-data.frame(gini_df)
    
    #View(gini_df)
    
    #plotting the data
    
    plt_gini<-ggplot(gini_df, aes(cum_usage_pct,cum_hh_pct))+geom_point()+xlab("Cum % usage")+ylab("cum % population")+ggtitle(paste("Cum % Total population Vs Cum% use for",file_name))
    print(plt_gini+ geom_smooth())
    gini_coeff<<-ineq(gini_df$cum_usage_pct,type="Gini")
    print(paste("gini coefficient for the",file_name,"=",ineq(gini_df$cum_usage_pct,type="Gini")))
    y<-plot(Lc(gini_df$cum_usage_pct),col="darkred",lwd=2, xlab="country", ylab="income",main=paste("lorenz curve for",file_name))
    
    gini(gini_df[which(gini_df$cum_hh_pct > 0), ]$cum_usage_pct)
    break}
  
}

indoorGini<- giniFunction(a.wat.use$Keycode, a.wat.use$TraceIndoorDaily,"Indoor Water Use","GPHD",
                          "histogram for Indoor Use","qq plot for Inoor usage Volume",
                          "transformed histogram for Indoor Volume","log normal curve Indoor Use")
indoorGini<-data_frame_gini


bathGini<- giniFunction(a.wat.use$Keycode, a.wat.use$BathtubDaily,"Bathtub Volume","GPHD",
                        "histogram for Bathtub volume","qq plot for Bathtub Volume",
                        "transformed histogram for Bathtub Volume","log normal curve for Bathtub Volume")


bathGini<- data_frame_gini


toiletGini<- giniFunction(a.wat.use$Keycode, a.wat.use$ToiletDailyVolume,"Toilet usage Volume (2016)","GPHD",
                          "histogram for Toilet usage Volume (2016)","qq plot for Toilet usage Volume (2016)",
                          "transformed histogram for Toilet usage Volume (2016)","log normal curve for Toilet usage Volume (2016)")
toiletGini<-data_frame_gini


showerGini<- giniFunction(a.wat.use$Keycode, a.wat.use$ShowerDailyVolume,"shower Volume","GPHD",
                          "histogram for shower volume","qq plot for shower Volume",
                          "transformed histogram for shower Volume","log normal curve for shower Volume")
showerGini<-data_frame_gini




faucetGini<- giniFunction(a.wat.use$Keycode, a.wat.use$FaucetDaily,"faucet Volume","GPHD",
                          "histogram for faucet volume","qq plot for faucet Volume",
                          "transformed histogram for faucet Volume","log normal curve for faucet Volume")
faucetGini<-data_frame_gini




clotheswasherGini<- giniFunction(a.wat.use$Keycode, a.wat.use$ClothesWasherDailyVolume,"Clothes Washer Volume","GPHD",
                                 "histogram for Clothes Washer volume","qq plot for Clothes Washer Volume",
                                 "transformed histogram for Clothes Washer Volume","log normal curve for Clothes Washer Volume")

clotheswasherGini<-data_frame_gini



dishwasherGini<- giniFunction(a.wat.use$Keycode, a.wat.use$DishwasherDaily,"Dish Washer Volume","GPHD",
                              "histogram for Dish Washer volume","qq plot for Dish Washer Volume",
                              "transformed histogram for Dish Washer Volume","log normal curve for Dish Washer Volume")
dishwasherGini <- data_frame_gini



leakGini<- giniFunction(a.wat.use$Keycode, a.wat.use$LeakDaily,"leak Volume","GPHD",
                        "histogram for leak volume","qq plot for leak Volume",
                        "transformed histogram for leak Volume","log normal curve for leak Volume")

leakGini<-data_frame_gini


irrigationGini<- giniFunction(a.wat.use$Keycode, a.wat.use$TraceOutdoorDaily,"Outdoor Volume","GPHD",
                              "histogram for Outdoor volume","qq plot for Outdoor Volume",
                              "transformed histogram for Outdoor Volume","log normal curve for Outdoor Volume")

irrigationGini <- data_frame_gini



Lc.indoor <- Lc(indoorGini$cum_usage_pct)
Lc.toilet <- Lc(toiletGini$cum_usage_pct)
Lc.shower <- Lc(showerGini$cum_usage_pct)
Lc.faucet <- Lc(faucetGini$cum_usage_pct)
Lc.clotheswasher <- Lc(clotheswasherGini$cum_usage_pct)
Lc.leak <- Lc(leakGini$cum_usage_pct)
Lc.bath <- Lc(bathGini$cum_usage_pct)
Lc.dishwasher <- Lc(dishwasherGini$cum_usage_pct)
Lc.irrigation <- Lc(irrigationGini$cum_usage_pct)

plot(Lc.indoor, xlab="cum population or HH", ylab="cum usage", main="Lorenz Curve for different appliance\n from 2016 Study")
lines(Lc.toilet, col=2)
lines(Lc.shower, col=3)
lines(Lc.faucet, col=4)
lines(Lc.clotheswasher, col=5)
lines(Lc.leak, col=6)
lines(Lc.bath, col=7)
lines(Lc.dishwasher, col=8)
lines(Lc.irrigation, col=9)

legend("topleft", legend=c('indoor, Gini Index:0.45 ','toilet, Gini Index:0.47 ','shower, Gini Index:0.52 ','faucet, Gini Index:0.48 ',
                           'clotheswasher, Gini Index:0.51 ','leak, Gini Index:0.67 ','bathtub, Gini Index:0.79 ', 'dishwasher, Gini Index:0.66 ',
                           'irrigation, Gini Index:0.86 '), col= 1:9,lty = 1, box.col = 1)






###############################################################################################################
# Creating the parallel Coordinate Plot
###############################################################################################################

# Checking linear relationship with indoor avg. water use with individual appliance use
p <- a.wat.use %>%
  plot_ly(type = 'parcoords',
          line = list(color = ~ rankIndoorAvg,
                      colorscale = 'Blues',
                      showscale = TRUE,
                      reversescale = TRUE,
                      cmin = min(a.wat.use$rankIndoorAvg),
                      cmax = max(a.wat.use$rankIndoorAvg)),
          dimensions = list(
            list(range = c(~min(TraceIndoorDaily),~max(TraceIndoorDaily)),
                 label = 'Avg indoor', values = ~TraceIndoorDaily),
            list(range = c(~min(BathtubDaily),~max(BathtubDaily)),
                 label = 'BathTubVol', values = ~BathtubDaily),
            list(range = c(~min(ClothesWasherDailyVolume),~max(ClothesWasherDailyVolume)),
                 label = 'CWasher', values = ~ClothesWasherDailyVolume),
            list(range = c(~min(DishwasherDaily),~max(DishwasherDaily)),
                 label = 'DWasherDaily', values = ~DishwasherDaily),
            list(range = c(~min(FaucetDaily),~max(FaucetDaily)),
                 label = 'FaucetVol', values = ~FaucetDaily),
            list(range = c(~min(LeakDaily),~max(LeakDaily)),
                 label = 'LeakVol', values = ~LeakDaily),
            list(range = c(~min(OtherDaily),~max(OtherDaily)),
                 label = 'OtherVol', values = ~OtherDaily),
            list(range = c(~min(ShowerDailyVolume),~max(ShowerDailyVolume)),
                 label = 'ShowerVol', values = ~ShowerDailyVolume),
            list(range = c(~min(ToiletDailyVolume),~max(ToiletDailyVolume)),
                 label = 'ToiletVol', values = ~ToiletDailyVolume),
            list(range = c(~min(TraceOutdoorDaily),~max(TraceOutdoorDaily)),
                 label = 'Avg Outdoor', values = ~TraceOutdoorDaily)
            
          )
  )

p


# Checking linear relationship with indoor avg. water user ranks and other appliance usage ranks
q <- a.wat.use %>%
  plot_ly(type = 'parcoords',
          line = list(color = ~ rankIndoorAvg,
                      colorscale = 'Blues',
                      showscale = TRUE,
                      reversescale = TRUE,
                      cmin = min(a.wat.use$rankIndoorAvg),
                      cmax = max(a.wat.use$rankIndoorAvg)),
          dimensions = list(
            list(range = c(~min(rankIndoorAvg),~max(rankIndoorAvg)),
                 label = 'IndoorUseRank', values = ~rankIndoorAvg),
            list(range = c(~min(rankBathtubAvg),~max(rankBathtubAvg)),
                 label = 'BathTubRank', values = ~rankBathtubAvg),
            list(range = c(~min(rankClothesWasherAvg),~max(rankClothesWasherAvg)),
                 label = 'CWasherRank', values = ~rankClothesWasherAvg),
            list(range = c(~min(rankDishWasherAvg),~max(rankDishWasherAvg)),
                 label = 'DWasherRank', values = ~rankDishWasherAvg),
            list(range = c(~min(rankFaucetVolAvg),~max(rankFaucetVolAvg)),
                 label = 'FaucetRank', values = ~rankFaucetVolAvg),
            list(range = c(~min(rankLeakVolAvg),~max(rankLeakVolAvg)),
                 label = 'LeakRank', values = ~rankLeakVolAvg),
            list(range = c(~min(rankOtherAvg),~max(rankOtherAvg)),
                 label = 'OtherRank', values = ~rankOtherAvg),
            list(range = c(~min(rankShowerVolAvg),~max(rankShowerVolAvg)),
                 label = 'ShowerRank', values = ~rankShowerVolAvg),
            list(range = c(~min(rankToiletVolAvg),~max(rankToiletVolAvg)),
                 label = 'ToiletRank', values = ~rankToiletVolAvg),
            list(range = c(~min(rankOutdoorAvg),~max(rankOutdoorAvg)),
                 label = 'OutdoorUseRank', values = ~rankOutdoorAvg)
            
          )
  )

q


############################################################################################################################
############################################################################################################################
# Now same analysis, but we will use the event number each appliance per HH instead of the Volume.
############################################################################################################################
############################################################################################################################



# creating a dataframe consisting of the different water end-uses volume
a.wat.use.event <-a[,c(1,172,173,174:181)]


# checking if the average valuesa are correct;


# omitting all NA cells
a.wat.use.event <-na.omit(a.wat.use.event)


##############################################################################################################
# Creating rank fields for each appliance use event


# ranking as per indoor trace (i.e., indoor average use per HH)
a.wat.use.event$rankIndoorAvg <- rank(a.wat.use.event$TraceIndoorDaily)

# ranking as per outdoor trace (i.e., outdoor average use per HH)
a.wat.use.event$rankOutdoorAvg <- rank(a.wat.use.event$TraceOutdoorDaily)

# ranking as per bathtub use
a.wat.use.event$rankBathtubAvg <- rank(a.wat.use.event$Bathtubevents)

# ranking as per clotheswasher use
a.wat.use.event$rankClothesWasherAvg <- rank(a.wat.use.event$Clotheswasherevents)

# ranking as per dishwasher use
a.wat.use.event$rankDishWasherAvg <- rank(a.wat.use.event$Dishwasherevents)

# ranking as per Faucet use
a.wat.use.event$rankFaucetVolAvg <- rank(a.wat.use.event$Faucetevents)

# ranking as per Leak
a.wat.use.event$rankLeakVolAvg <- rank(a.wat.use.event$Leakevents)

# ranking as per uncategorized other use
a.wat.use.event$rankOtherAvg <- rank(a.wat.use.event$Otherevents)

# ranking as per Shower use
a.wat.use.event$rankShowerVolAvg <- rank(a.wat.use.event$Showerevents)

# ranking as per Tolet use
a.wat.use.event$rankToiletVolAvg <- rank(a.wat.use.event$Toiletevents)


# writing an excel file for better visualization
write.xlsx(a.wat.use.event,file="internalUsageRelatn_eventWise.xlsx")

###############################################################################################################
#Creating the parallel Coordinate Plot
###############################################################################################################

# Checking linear relationship with indoor avg. water use volume with individual appliance use event over the entire observation
p <- a.wat.use.event %>%
  plot_ly(type = 'parcoords',
          line = list(color = ~ rankIndoorAvg,
                      colorscale = 'Blues',
                      showscale = TRUE,
                      reversescale = TRUE,
                      cmin = min(a.wat.use.event$rankIndoorAvg),
                      cmax = max(a.wat.use.event$rankIndoorAvg)),
          dimensions = list(
            list(range = c(~min(TraceIndoorDaily),~max(TraceIndoorDaily)),
                 label = 'Avg indoor', values = ~TraceIndoorDaily),
            list(range = c(~min(Bathtubevents),~max(Bathtubevents)),
                 label = 'BathTubEvent', values = ~Bathtubevents),
            list(range = c(~min(Clotheswasherevents),~max(Clotheswasherevents)),
                 label = 'CWasherEvent', values = ~Clotheswasherevents),
            list(range = c(~min(Dishwasherevents),~max(Dishwasherevents)),
                 label = 'DWasherEvent', values = ~Dishwasherevents),
            list(range = c(~min(Faucetevents),~max(Faucetevents)),
                 label = 'FaucetEvent', values = ~Faucetevents),
            list(range = c(~min(Leakevents),~max(Leakevents)),
                 label = 'LeakEvent', values = ~Leakevents),
            list(range = c(~min(Otherevents),~max(Otherevents)),
                 label = 'OtherEvent', values = ~Otherevents),
            list(range = c(~min(Showerevents),~max(Showerevents)),
                 label = 'ShowerEvent', values = ~Showerevents),
            list(range = c(~min(Toiletevents),~max(Toiletevents)),
                 label = 'ToiletEvent', values = ~Toiletevents),
            list(range = c(~min(TraceOutdoorDaily),~max(TraceOutdoorDaily)),
                 label = 'Avg Outdoor', values = ~TraceOutdoorDaily)
            
          )
  )

p


# Checking linear relationship with indoor avg. water use ranks and ranked appliance wise event
q <- a.wat.use.event %>%
  plot_ly(type = 'parcoords',
          line = list(color = ~ rankIndoorAvg,
                      colorscale = 'Blues',
                      showscale = TRUE,
                      reversescale = TRUE,
                      cmin = min(a.wat.use.event$rankIndoorAvg),
                      cmax = max(a.wat.use.event$rankIndoorAvg)),
          dimensions = list(
            list(range = c(~min(rankIndoorAvg),~max(rankIndoorAvg)),
                 label = 'IndoorUseRank', values = ~rankIndoorAvg),
            list(range = c(~min(rankBathtubAvg),~max(rankBathtubAvg)),
                 label = 'BathTubRank', values = ~rankBathtubAvg),
            list(range = c(~min(rankClothesWasherAvg),~max(rankClothesWasherAvg)),
                 label = 'CWasherRank', values = ~rankClothesWasherAvg),
            list(range = c(~min(rankDishWasherAvg),~max(rankDishWasherAvg)),
                 label = 'DWasherRank', values = ~rankDishWasherAvg),
            list(range = c(~min(rankFaucetVolAvg),~max(rankFaucetVolAvg)),
                 label = 'FaucetRank', values = ~rankFaucetVolAvg),
            list(range = c(~min(rankLeakVolAvg),~max(rankLeakVolAvg)),
                 label = 'LeakRank', values = ~rankLeakVolAvg),
            list(range = c(~min(rankOtherAvg),~max(rankOtherAvg)),
                 label = 'OtherRank', values = ~rankOtherAvg),
            list(range = c(~min(rankShowerVolAvg),~max(rankShowerVolAvg)),
                 label = 'ShowerRank', values = ~rankShowerVolAvg),
            list(range = c(~min(rankToiletVolAvg),~max(rankToiletVolAvg)),
                 label = 'ToiletRank', values = ~rankToiletVolAvg),
            list(range = c(~min(rankOutdoorAvg),~max(rankOutdoorAvg)),
                 label = 'OutdoorUseRank', values = ~rankOutdoorAvg)
            
          )
  )

q

##############################################################################################################################
##############################################################################################################################
### In this part we will look into the shower time, shower taken per day, toilet flushes, number of people in the HH etc. to get
### a better understanding
##############################################################################################################################
##############################################################################################################################


# Creating the dataframe with different survey data e.g., survey_number_of_adults,survey_number_of_teenagers,
# survey_number_of_children, survey_number_of_infants, survey_replaced_toilets_10,survey_replaced_showerheads_10,
# survey_replaced_clothes_washer_10, and ClothesWasherUseVolume,ClothesWasherDailyUses,ShowerUseVolume,
# ShowerDailyUses,ShowerDailyMinutes,ToiletUseVolume,ToiletUseStDev,ToiletDailyUses values

a.wat.use <-a[,c(1,172,173,16:19,51:53,190:192, 194:196,198)]

# omitting all NA cells
a.wat.use <-na.omit(a.wat.use) # the number of HH reduced down to 662 from 771

# Changing the yes/no to 0 and 1
a.wat.use[, 8:10]<- ifelse(a.wat.use[, 8:10]=="Yes",1,0)

# ranking as per indoor trace (i.e., indoor average use per HH)
a.wat.use$rankIndoorAvg <- rank(a.wat.use$TraceIndoorDaily)

# ranking as per outdoor trace (i.e., outdoor average use per HH)
a.wat.use$rankOutdoorAvg <- rank(a.wat.use$TraceOutdoorDaily)

# writing an excel file for better visualization
write.xlsx(a.wat.use,file="internalUsageRelatn_ToiletShowerCWasher_Focus.xlsx")


###############################################################################################################
#Creating the parallel Coordinate Plot
###############################################################################################################

# Checking linear relationship with indoor avg. water use volume with individual variable
p <- a.wat.use %>%
  plot_ly(type = 'parcoords',
          line = list(color = ~ rankIndoorAvg,
                      colorscale = 'Blues',
                      showscale = TRUE,
                      reversescale = TRUE,
                      cmin = min(a.wat.use$rankIndoorAvg),
                      cmax = max(a.wat.use$rankIndoorAvg)),
          dimensions = list(
            list(range = c(~min(TraceIndoorDaily),~max(TraceIndoorDaily)),
                 label = 'Avg indoor', values = ~TraceIndoorDaily),
            list(range = c(~min(survey_number_of_adults),~max(survey_number_of_adults)),
                 label = 'AdultNumber', values = ~survey_number_of_adults),
            list(range = c(~min(survey_number_of_teenagers),~max(survey_number_of_teenagers)),
                 label = 'TeenNumber', values = ~survey_number_of_teenagers),
            list(range = c(~min(survey_number_of_children),~max(survey_number_of_children)),
                 label = 'ChildrenNumber', values = ~survey_number_of_children),
            list(range = c(~min(survey_number_of_infants),~max(survey_number_of_infants)),
                 label = 'InfantNumber', values = ~survey_number_of_infants),
            list(range = c(~min(survey_replaced_toilets_10),~max(survey_replaced_toilets_10)),
                 label = 'ToiletReplaced', values = ~survey_replaced_toilets_10),
            list(range = c(~min(survey_replaced_showerheads_10),~max(survey_replaced_showerheads_10)),
                 label = 'ShowerHeadReplaced', values = ~survey_replaced_showerheads_10),
            list(range = c(~min(survey_replaced_clothes_washer_10),~max(survey_replaced_clothes_washer_10)),
                 label = 'CWasherReplaced', values = ~survey_replaced_clothes_washer_10),
            list(range = c(~min(ClothesWasherUseVolume),~max(ClothesWasherUseVolume)),
                 label = 'CWasherVolpLoad', values = ~ClothesWasherUseVolume),
            list(range = c(~min(ClothesWasherDailyUses),~max(ClothesWasherDailyUses)),
                 label = 'DailyCWasherUseNum', values = ~ClothesWasherDailyUses),
            list(range = c(~min(ShowerUseVolume),~max(ShowerUseVolume)),
                 label = 'ShowerVolpEvent', values = ~ShowerUseVolume),
            list(range = c(~min(ShowerDailyUses),~max(ShowerDailyUses)),
                 label = 'ShowerDailyUses', values = ~ShowerDailyUses),
            list(range = c(~min(ShowerDailyMinutes),~max(ShowerDailyMinutes)),
                 label = 'ShowerDailyMinutes', values = ~ShowerDailyMinutes),
            list(range = c(~min(ToiletUseVolume),~max(ToiletUseVolume)),
                 label = 'ToiletUseVolpEvent', values = ~ToiletUseVolume),
            list(range = c(~min(ToiletDailyUses),~max(ToiletDailyUses)),
                 label = 'ToiletUsespDay', values = ~ToiletDailyUses),
            list(range = c(~min(TraceOutdoorDaily),~max(TraceOutdoorDaily)),
                 label = 'Avg Outdoor', values = ~TraceOutdoorDaily)
            
          )
  )

p



##############################################################################################################################
##############################################################################################################################
### In this part we will check how the Users are responding to attitude questions for better understanding the users
### The attituted questions are;
# REU2016_attitude_A 	Most households in my community know where their water comes from when they turn on the tap. 
# REU2016_attitude_B 	Residents should be allowed to track their household water use via the web or by reading their own water meter. 
# REU2016_attitude_C 	Households would conserve more water if they had an easier way to monitor their water use. 
# REU2016_attitude_D 	Without looking at past bills, I know about how much my average (typical) household water bill was (in dollars) last year. 
# REU2016_attitude_E 	Without looking at past bills, I know about how much water my household used in an average (typical) billing period last year. 
# REU2016_attitude_F 	The cost of water is an important factor for me when deciding how much water to use indoors (e.g. for washing dishes, 
# REU2016_attitude_G 	The cost of water is an important factor for me when deciding how much water to use outdoors (e.g., for watering the lawn or garden, etc.).                                                                                                                REU2016_attitude_H 	"I take into account the cost of wastewater (sewer) service when deciding how much water to use.*
#  					          *If you are charged a flat rate for wastewater/sewer service, mark ""not applicable."""
# REU2016_attitude_I 	Conservation of water is critical for the future of my community. 
# REU2016_attitude_J 	There should be strong financial penalties for people who use too much water. 
# REU2016_attitude_K 	I am aware of rebates offered by my water utility. 
# REU2016_attitude_L 	Water rates should be increased to encourage water conservation. 
# REU2016_attitude_M 	I conserve water to save money. 
# REU2016_attitude_N 	I conserve water to save energy. 
# REU2016_attitude_O 	I conserve water because it is the right thing to do. 
# REU2016_attitude_P 	People who use more water should pay more per gallon for their water. 
# REU2016_attitude_Q 	My water utility should be more active in promoting water conservation on the part of households and businesses. 
# REU2016_attitude_R 	My water utility should provide financial incentives to conserve water. 
# REU2016_attitude_S 	My water utility should promote water use guidelines. 

# we will refere to these questions by the last letter (i.e., A-S)
##############################################################################################################################
##############################################################################################################################

a.wat.use <-a[,c(1,172,173,149:167)]


# changing reuws2_attitude question answers to numeric values
# "Not applicable" =0
# "Strongly disagree"=1
# "Somewhat disagree"=2
# "Somewhat agree"=3
# "Strongly agree"=4


a.wat.use[, c(4:22)] <- with(a.wat.use[, c(4:22)], ifelse(a.wat.use[, c(4:22)]=="Not applicable",0,
                                                  ifelse(a.wat.use[, c(4:22)]=="Strongly disagree",1,
                                                         ifelse(a.wat.use[, c(4:22)]=="Somewhat disagree",2,
                                                                ifelse(a.wat.use[, c(4:22)]=="Somewhat agree",3,
                                                                       ifelse(a.wat.use[, c(4:22)]=="Strongly agree",4, 0))))))



# Empty cells i.e.,unanswered attitude questions are converted to 0                  
# a.wat.use[, c(4:22)][is.na(a.wat.use[, c(4:22)])] <- 0

# omitting all NA cells
a.wat.use <-na.omit(a.wat.use)

# ranking as per indoor trace (i.e., indoor average use per HH)
a.wat.use$rankIndoorAvg <- rank(a.wat.use$TraceIndoorDaily)

# ranking as per outdoor trace (i.e., outdoor average use per HH)
a.wat.use$rankOutdoorAvg <- rank(a.wat.use$TraceOutdoorDaily)

# writing an excel file for better visualization
write.xlsx(a.wat.use,file="internalUsageRelatn_AttitudeResponse.xlsx")

###############################################################################################################
#Creating the parallel Coordinate Plot
###############################################################################################################

# Checking linear relationship with indoor avg. water use volume with attitude responses
p <- a.wat.use %>%
  plot_ly(type = 'parcoords',
          line = list(color = ~ rankIndoorAvg,
                      colorscale = 'Blues',
                      showscale = TRUE,
                      reversescale = TRUE,
                      cmin = min(a.wat.use$rankIndoorAvg),
                      cmax = max(a.wat.use$rankIndoorAvg)),
          dimensions = list(
            list(range = c(~min(TraceIndoorDaily),~max(TraceIndoorDaily)),
                 label = 'Avg indoor', values = ~TraceIndoorDaily),
            list(range = c(~min(reuws2_attitude_A),~max(reuws2_attitude_A)),
                 label = 'A', values = ~reuws2_attitude_A),
            list(range = c(~min(reuws2_attitude_B),~max(reuws2_attitude_B)),
                 label = 'B', values = ~reuws2_attitude_B),
            list(range = c(~min(reuws2_attitude_C),~max(reuws2_attitude_C)),
                 label = 'C', values = ~reuws2_attitude_C),
            list(range = c(~min(reuws2_attitude_D),~max(reuws2_attitude_D)),
                 label = 'D', values = ~reuws2_attitude_D),
            list(range = c(~min(reuws2_attitude_E),~max(reuws2_attitude_E)),
                 label = 'E', values = ~reuws2_attitude_E),
            list(range = c(~min(reuws2_attitude_F),~max(reuws2_attitude_F)),
                 label = 'F', values = ~reuws2_attitude_F),
            list(range = c(~min(reuws2_attitude_G),~max(reuws2_attitude_G)),
                 label = 'G', values = ~reuws2_attitude_G),
            list(range = c(~min(reuws2_attitude_H),~max(reuws2_attitude_H)),
                 label = 'H', values = ~reuws2_attitude_H),
            list(range = c(~min(reuws2_attitude_I),~max(reuws2_attitude_I)),
                 label = 'I', values = ~reuws2_attitude_I),
            list(range = c(~min(reuws2_attitude_J),~max(reuws2_attitude_J)),
                 label = 'J', values = ~reuws2_attitude_J),
            list(range = c(~min(reuws2_attitude_K),~max(reuws2_attitude_K)),
                 label = 'K', values = ~reuws2_attitude_K),
            list(range = c(~min(reuws2_attitude_L),~max(reuws2_attitude_L)),
                 label = 'L', values = ~reuws2_attitude_L),
            list(range = c(~min(reuws2_attitude_M),~max(reuws2_attitude_M)),
                 label = 'M', values = ~reuws2_attitude_M),
            list(range = c(~min(reuws2_attitude_N),~max(reuws2_attitude_N)),
                 label = 'N', values = ~reuws2_attitude_N),
            list(range = c(~min(reuws2_attitude_O),~max(reuws2_attitude_O)),
                 label = 'O', values = ~reuws2_attitude_O),
            list(range = c(~min(reuws2_attitude_P),~max(reuws2_attitude_P)),
                 label = 'P', values = ~reuws2_attitude_P),
            list(range = c(~min(reuws2_attitude_Q),~max(reuws2_attitude_Q)),
                 label = 'Q', values = ~reuws2_attitude_Q),
            list(range = c(~min(reuws2_attitude_R),~max(reuws2_attitude_R)),
                 label = 'R', values = ~reuws2_attitude_R),
            list(range = c(~min(reuws2_attitude_S),~max(reuws2_attitude_S)),
                 label = 'S', values = ~reuws2_attitude_S),
            list(range = c(~min(TraceOutdoorDaily),~max(TraceOutdoorDaily)),
                 label = 'Avg Outdoor', values = ~TraceOutdoorDaily)
            
          )
  )

p


# Checking linear relationship with indoor avg. water use rank with attitude responses
q <- a.wat.use %>%
  plot_ly(type = 'parcoords',
          line = list(color = ~ rankIndoorAvg,
                      colorscale = 'Blues',
                      showscale = TRUE,
                      reversescale = TRUE,
                      cmin = min(a.wat.use$rankIndoorAvg),
                      cmax = max(a.wat.use$rankIndoorAvg)),
          dimensions = list(
            list(range = c(~min(rankIndoorAvg),~max(rankIndoorAvg)),
                 label = 'Avg indoor', values = ~rankIndoorAvg),
            list(range = c(~min(reuws2_attitude_A),~max(reuws2_attitude_A)),
                 label = 'A', values = ~reuws2_attitude_A),
            list(range = c(~min(reuws2_attitude_B),~max(reuws2_attitude_B)),
                 label = 'B', values = ~reuws2_attitude_B),
            list(range = c(~min(reuws2_attitude_C),~max(reuws2_attitude_C)),
                 label = 'C', values = ~reuws2_attitude_C),
            list(range = c(~min(reuws2_attitude_D),~max(reuws2_attitude_D)),
                 label = 'D', values = ~reuws2_attitude_D),
            list(range = c(~min(reuws2_attitude_E),~max(reuws2_attitude_E)),
                 label = 'E', values = ~reuws2_attitude_E),
            list(range = c(~min(reuws2_attitude_F),~max(reuws2_attitude_F)),
                 label = 'F', values = ~reuws2_attitude_F),
            list(range = c(~min(reuws2_attitude_G),~max(reuws2_attitude_G)),
                 label = 'G', values = ~reuws2_attitude_G),
            list(range = c(~min(reuws2_attitude_H),~max(reuws2_attitude_H)),
                 label = 'H', values = ~reuws2_attitude_H),
            list(range = c(~min(reuws2_attitude_I),~max(reuws2_attitude_I)),
                 label = 'I', values = ~reuws2_attitude_I),
            list(range = c(~min(reuws2_attitude_J),~max(reuws2_attitude_J)),
                 label = 'J', values = ~reuws2_attitude_J),
            list(range = c(~min(reuws2_attitude_K),~max(reuws2_attitude_K)),
                 label = 'K', values = ~reuws2_attitude_K),
            list(range = c(~min(reuws2_attitude_L),~max(reuws2_attitude_L)),
                 label = 'L', values = ~reuws2_attitude_L),
            list(range = c(~min(reuws2_attitude_M),~max(reuws2_attitude_M)),
                 label = 'M', values = ~reuws2_attitude_M),
            list(range = c(~min(reuws2_attitude_N),~max(reuws2_attitude_N)),
                 label = 'N', values = ~reuws2_attitude_N),
            list(range = c(~min(reuws2_attitude_O),~max(reuws2_attitude_O)),
                 label = 'O', values = ~reuws2_attitude_O),
            list(range = c(~min(reuws2_attitude_P),~max(reuws2_attitude_P)),
                 label = 'P', values = ~reuws2_attitude_P),
            list(range = c(~min(reuws2_attitude_Q),~max(reuws2_attitude_Q)),
                 label = 'Q', values = ~reuws2_attitude_Q),
            list(range = c(~min(reuws2_attitude_R),~max(reuws2_attitude_R)),
                 label = 'R', values = ~reuws2_attitude_R),
            list(range = c(~min(reuws2_attitude_S),~max(reuws2_attitude_S)),
                 label = 'S', values = ~reuws2_attitude_S),
            list(range = c(~min(TraceOutdoorDaily),~max(TraceOutdoorDaily)),
                 label = 'Avg Outdoor', values = ~TraceOutdoorDaily)
            
          )
  )

q
