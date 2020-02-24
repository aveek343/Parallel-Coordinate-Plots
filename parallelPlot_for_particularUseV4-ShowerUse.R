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
a <- read_excel("REU2016_Survey - End Use Sample.xlsx")

# creating a dataframe that has values or answers related to outdoor use and water conservation
# a.out <- a[, c(1,71,74,88,89,90,94,95,96,97,98,100,102,105,108,109,110,111,112,113,114,126,127,128,129,130,217,228,229,230,232,233,234)]

# creating a dataframe that has values or answers related to indoor use and water conservation
a.in <- a[,c(1,14:17,19:29,42:52,109:112,114:126, 140:145,149:154, 168:201, 216)]

# changing reuws2_attitude question answers to numeric values
# "Not applicable" =0
# "Strongly disagree"=1
# "Somewhat disagree"=2
# "Somewhat agree"=3
# "Strongly agree"=4


a.in[, c(51:56)] <- with(a.in[, c(51:56)], ifelse(a.in[, c(51:56)]=="Not applicable",0,
                                   ifelse(a.in[, c(51:56)]=="Strongly disagree",1,
                                   ifelse(a.in[, c(51:56)]=="Somewhat disagree",2,
                                   ifelse(a.in[, c(51:56)]=="Somewhat agree",3,
                                   ifelse(a.in[, c(51:56)]=="Strongly agree",4, 0))))))

# Empty cells are converted to 0                  
a.in[, c(51:56)][is.na(a.in[, c(51:56)])] <- 0                   
# summing the response from reuws2_attitude answers
a.in$reuws2ResponseScore <- rowSums(a.in[, c(51:56)])
    
# to check the output, write a excel file and put colindex number manually and see if anything is missing, 
# if so, then add the column index in line 64

write.xlsx(a.in,file="REUS-Enduse_Survey_Indoor.xlsx")
# first making all empty cells NA
 #datavalues<-na.omit(datavalues, cols=c("DataValue"))
#a.in <-na.omit(a.in)

###########DO NOT CHANGE OR ADD ANY LINES################################################
# Starting with shower data. All questions and values related to shower data is sub-setted
a.in.shower <- a.in[, c(1,4:6,10:15,25,27:32,34:35,43,45:61,69,77,81:84,91:92)]
write.xlsx(a.in.shower,file="REUS-Enduse_Survey_Indoor-Shower.xlsx")

# Changing the yes/no to 0 and 1
a.in.shower[, 12:20]<- ifelse(a.in.shower[, 12:20]=="Yes",1,0)

# converting the "NA" values within the these columns to 0
a.in.shower[, 12:20][is.na(a.in.shower[, c(12:20)])] <- 0

# summing the survey response values
a.in.shower$ConservationResponseScore <- rowSums(a.in.shower[, c(12:20)])


a.in.shower[, c(23)] <- with(a.in.shower[, c(23)], ifelse(a.in.shower[, c(23)]=="Own",1,0))
a.in.shower[, c(23)][is.na(a.in.shower[, c(23)])] <- 0  

write.xlsx(a.in.shower,file="REUS-Enduse_Survey_Indoor-Shower_updated.xlsx")


# sub-setting data required for the parallel coordinate plot (column index may change
# use the View() command to view the dataframe and use the column header related to the topic instead)
pcord.a.in.shower <- a.in.shower[, c(1:5,10,12,13,21,23:25,36:40,42:44:46)]

# in this stage all the values with "NA" are removed.
pcord.a.in.shower<-na.omit(pcord.a.in.shower, cols=c("ShowerUseVolume"))
write.xlsx(pcord.a.in.shower,file="parallelCordDataShower.xlsx")

##############################################################################################################################
############################################Ranking the dataframe#############################################################
######Ranked based on average daily Use (which is TraceIndoorDaily column in the dataset)#####################################
##############################################################################################################################

# We will rank the whole dataframe according to average daily Use
# Second, we will rank other factors according to respective observed or calculated values(e.g., aggregate of response values, or
# the values like number of adults in a HH), and the total indoor water use volume (thus making sure individual factor
# ranking stays unique)
# 

pcord.a.in.shower$rankavg <- rank(pcord.a.in.shower$TraceIndoorDaily)

#1 ranking the dataframe as per number of adults
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$survey_number_of_adults,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankAdult <- 1:nrow(pcord.a.in.shower)

#2 ranking the dataframe as per number of survey_number_of_teenagers
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$survey_number_of_teenagers,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankTeen <- 1:nrow(pcord.a.in.shower)

#3 ranking the dataframe as per number of survey_number_of_infants
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$survey_number_of_infants,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankInfant <- 1:nrow(pcord.a.in.shower)

#4 ranking the dataframe as per number of survey_number_of_showers
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$survey_number_of_showers,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankShowerNumber <- 1:nrow(pcord.a.in.shower)

#5 ranking the dataframe as per number of survey_replaced_showerheads_10
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$survey_replaced_showerheads_10,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankReplacedShower <- 1:nrow(pcord.a.in.shower)

#6 ranking the dataframe as per number of adults staying at home during weekdays
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$survey_homies,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankAdultatHome <- 1:nrow(pcord.a.in.shower)

#7 ranking the dataframe as per age of home
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$survey_age_of_home,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankHomeAge <- 1:nrow(pcord.a.in.shower)

#8 ranking the dataframe as per ownership of the household (rent or own)
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$survey_renter,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankOwnRent <- 1:nrow(pcord.a.in.shower)

#9 ranking the dataframe as per number of adutlts at work during weekdays
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$survey_adults_employed,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankAdultEmployed <- 1:nrow(pcord.a.in.shower)

#10 ranking the dataframe as per daily Shower Daily Volume
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$ShowerDailyVolume,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankDailyAvg <- 1:nrow(pcord.a.in.shower)

#11 ranking the dataframe as per shower event wise volume
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$ShowerUseVolume,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankEventWiseVOl <- 1:nrow(pcord.a.in.shower)

#12 ranking the dataframe as per daily Showerevents
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$Showerevents,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankShowerEvent <- 1:nrow(pcord.a.in.shower)

#13 ranking the dataframe as per Conservation Response Score
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$ConservationResponseScore,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankConRes <- 1:nrow(pcord.a.in.shower)

#14 ranking the dataframe as per reuws2 response Score
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$reuws2ResponseScore,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankREUWSRes <- 1:nrow(pcord.a.in.shower)

#15 ranking the dataframe as per average indoor use
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$IndoorKgal,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankTotUse <- 1:nrow(pcord.a.in.shower)

#16 ranking the dataframe as per daily Avg Shower Time (in minutes)
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$ShowerDailyMinutes,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankShowerTimeM <- 1:nrow(pcord.a.in.shower)


pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$rankavg), ]

#####################################################################################################################################
#################################################Plotting the Parallel###############################################################
#####################################################Coordinate Plot#################################################################
#####################################################################################################################################

# Creating a percentile plot for shower use to get an idea about the usage
#creating the quantile plotting positions according to Cunnane plotting position p=(i-0.4)/(n+0.2)
pcord.a.in.shower$pPosition <- (pcord.a.in.shower$rankavg-0.4)/(length(pcord.a.in.shower$rankavg)+0.2)
#creating the normal quantile and percentile positions for from the plotting position
pcord.a.in.shower$nqPosition<-qnorm(pcord.a.in.shower$pPosition)
pcord.a.in.shower$qpPosition <-pcord.a.in.shower$pPosition*100

# Plotting the percentile Plot
plot(pcord.a.in.shower$qpPosition,pcord.a.in.shower$TraceIndoorDaily, xlab="percent position", ylab="Avg daily use per HH\n over entire observation", 
     main="position of HH in a percentile plot\n according to Average Indoor Water use", ylim=c(0,500))


pcord.a.in.shower$userType <- ""
pcord.a.in.shower[c(1:516),43] <- "Average to Medium Users"
pcord.a.in.shower[c(517:612),43] <- "High Users (Bottom tier)"
pcord.a.in.shower[c(613:645),43] <- "High Users (Top tier)"

pcord.a.in.shower$colorCode <- ""
pcord.a.in.shower[c(1:516),44] <- 1
pcord.a.in.shower[c(517:612),44] <- 2
pcord.a.in.shower[c(613:645),44] <- 3

write.xlsx(pcord.a.in.shower,file="parallelCordData_updated.xlsx")

library (plotly)
p <- pcord.a.in.shower %>%
  plot_ly(type = 'parcoords',
          line = list(color = ~colorCode,
                      colorscale = 'Jet',
                      showscale = TRUE,
                      reversescale = TRUE,
                      cmin = 1,
                      cmax = 2),
          dimensions = list(
            list(range = c(~min(rankavg),~max(rankavg)),
                 label = 'AvgInoorUseRank', values = ~rankavg),
            list(range = c(~min(rankAdult),~max(rankAdult)),
                 label = 'AdultNumRank', values = ~rankAdult),
            list(range = c(~min(rankTeen),~max(rankTeen)),
                 label = 'TeenNumRank', values = ~rankTeen),
            list(range = c(~min(rankInfant),~max(rankInfant)),
                 label = 'InfantNumRank', values = ~rankInfant),
            list(range = c(~min(rankShowerNumber),~max(rankShowerNumber)),
                 label = 'NumShowerRank', values = ~rankShowerNumber),
            list(range = c(~min(rankReplacedShower),~max(rankReplacedShower)),
                 label = 'NumReplacedHead', values = ~rankReplacedShower),
            list(range = c(~min(rankAdultatHome),~max(rankAdultatHome)),
                 label = 'UnempRank', values = ~rankAdultatHome),
            list(range = c(~min(rankAdultEmployed),~max(rankAdultEmployed)),
                 label = 'EmployedRank', values = ~rankAdultEmployed),
            list(range = c(~min(rankHomeAge),~max(rankHomeAge)),
                 label = 'HomeAgeRank', values = ~rankHomeAge),
            list(range = c(~min(rankOwnRent),~max(rankOwnRent)),
                 label = 'rentOwnRank', values = ~rankOwnRent),
            list(range = c(~min(rankEventWiseVOl),~max(rankEventWiseVOl)),
                 label = 'showerEventVolRank', values = ~rankEventWiseVOl),
            list(range = c(~min(rankDailyAvg),~max(rankDailyAvg)),
                 label = 'ShowerDailyRank', values = ~rankDailyAvg),
            list(range = c(~min(rankShowerEvent),~max(rankShowerEvent)),
                 label = 'ShowerEventNumRank', values = ~rankShowerEvent),
            list(range = c(~min(rankShowerTimeM),~max(rankShowerTimeM)),
                 label = 'ShowerTimeRank', values = ~rankShowerTimeM),
            list(range = c(~min(rankConRes),~max(rankConRes)),
                 label = 'ConserveResponseRank', values = ~rankConRes),
            list(range = c(~min(rankREUWSRes),~max(rankREUWSRes)),
                 label = 'REUWSResponseRank', values = ~rankREUWSRes),
            list(range = c(~min(rankTotUse),~max(rankTotUse)),
                 label = 'RankTotalIndoorUseKgal', values = ~rankTotUse)
          )
  )

p



##############################################################################################################################
############################################Ranking the dataframe#############################################################
######Ranked based on Total Indoor Water use (which is IndoorKgal column in the dataset)#####################################
##############################################################################################################################

# We will rank the whole dataframe according to Total Indoor Water Use
# Second, we will rank other factors according to respective observed or calculated values(e.g., aggregate of response values, or
# the values like number of adults in a HH), and the total indoor water use volume (thus making sure individual factor
# ranking stays unique)
# 
pcord.a.in.shower <- a.in.shower[, c(1:5,10,12,13,21,23:25,36:40,42:44:46)]
pcord.a.in.shower<-na.omit(pcord.a.in.shower, cols=c("IndoorKgal"))
pcord.a.in.shower$rankavg <- rank(pcord.a.in.shower$IndoorKgal)
#pcord.a.in.shower$rankavg <- 1:nrow(pcord.a.in.shower)

#1 ranking the dataframe as per number of adults
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$survey_number_of_adults,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankAdult <- 1:nrow(pcord.a.in.shower)

#2 ranking the dataframe as per number of survey_number_of_teenagers
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$survey_number_of_teenagers,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankTeen <- 1:nrow(pcord.a.in.shower)

#3 ranking the dataframe as per number of survey_number_of_infants
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$survey_number_of_infants,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankInfant <- 1:nrow(pcord.a.in.shower)

#4 ranking the dataframe as per number of survey_number_of_showers
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$survey_number_of_showers,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankShowerNumber <- 1:nrow(pcord.a.in.shower)

#5 ranking the dataframe as per number of survey_replaced_showerheads_10
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$survey_replaced_showerheads_10,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankReplacedShower <- 1:nrow(pcord.a.in.shower)

#6 ranking the dataframe as per number of adults staying at home during weekdays
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$survey_homies,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankAdultatHome <- 1:nrow(pcord.a.in.shower)

#7 ranking the dataframe as per age of home
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$survey_age_of_home,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankHomeAge <- 1:nrow(pcord.a.in.shower)

#8 ranking the dataframe as per ownership of the household (rent or own)
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$survey_renter,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankOwnRent <- 1:nrow(pcord.a.in.shower)

#9 ranking the dataframe as per number of adutlts at work during weekdays
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$survey_adults_employed,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankAdultEmployed <- 1:nrow(pcord.a.in.shower)

#10 ranking the dataframe as per daily Shower Daily Volume
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$ShowerDailyVolume,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankDailyAvg <- 1:nrow(pcord.a.in.shower)

#11 ranking the dataframe as per shower event wise volume
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$ShowerUseVolume,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankEventWiseVOl <- 1:nrow(pcord.a.in.shower)

#12 ranking the dataframe as per daily Showerevents
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$Showerevents,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankShowerEvent <- 1:nrow(pcord.a.in.shower)

#13 ranking the dataframe as per Conservation Response Score
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$ConservationResponseScore,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankConRes <- 1:nrow(pcord.a.in.shower)

#14 ranking the dataframe as per reuws2 response Score
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$reuws2ResponseScore,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankREUWSRes <- 1:nrow(pcord.a.in.shower)

#15 ranking the dataframe as per average indoor use
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$ShowerDailyVolume,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankAvgShowerDailyVol <- 1:nrow(pcord.a.in.shower)

#16 ranking the dataframe as per daily Avg Shower Time (in minutes)
pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$ShowerDailyMinutes,pcord.a.in.shower$rankavg), ]
pcord.a.in.shower$rankShowerTimeM <- 1:nrow(pcord.a.in.shower)


pcord.a.in.shower <- pcord.a.in.shower[order(pcord.a.in.shower$rankavg), ]

#####################################################################################################################################
#################################################Plotting the Parallel###############################################################
#####################################################Coordinate Plot#################################################################
#####################################################################################################################################

# Creating a percentile plot for shower use to get an idea about the usage
#creating the quantile plotting positions according to Cunnane plotting position p=(i-0.4)/(n+0.2)
pcord.a.in.shower$pPosition <- (pcord.a.in.shower$rankavg-0.4)/(length(pcord.a.in.shower$rankavg)+0.2)
#creating the normal quantile and percentile positions for from the plotting position
pcord.a.in.shower$nqPosition<-qnorm(pcord.a.in.shower$pPosition)
pcord.a.in.shower$qpPosition <-pcord.a.in.shower$pPosition*100

# Plotting the percentile Plot
plot(pcord.a.in.shower$qpPosition,pcord.a.in.shower$TraceIndoorDaily, xlab="percent position", ylab="Avg daily use per HH\n over entire observation", 
     main="position of HH in a percentile plot\n according to Average Indoor Water use", ylim=c(0,500))


pcord.a.in.shower$userType <- ""
pcord.a.in.shower[c(1:516),43] <- "Average to Medium Users"
pcord.a.in.shower[c(517:612),43] <- "High Users (Bottom tier)"
pcord.a.in.shower[c(613:645),43] <- "High Users (Top tier)"

pcord.a.in.shower$colorCode <- ""
pcord.a.in.shower[c(1:516),44] <- 1
pcord.a.in.shower[c(517:612),44] <- 2
pcord.a.in.shower[c(613:645),44] <- 3

write.xlsx(pcord.a.in.shower,file="parallelCordData_updated.xlsx")

library (plotly)
q <- pcord.a.in.shower %>%
  plot_ly(type = 'parcoords',
          line = list(color = ~colorCode,
                      colorscale = 'Viridis',
                      showscale = TRUE,
                      reversescale = TRUE,
                      cmin = 1.9,
                      cmax = 2),
          dimensions = list(
            list(range = c(~min(rankavg),~max(rankavg)),
                 label = 'AvgInoorUseRank', values = ~rankavg),
            list(range = c(~min(rankAdult),~max(rankAdult)),
                 label = 'AdultNumRank', values = ~rankAdult),
            list(range = c(~min(rankTeen),~max(rankTeen)),
                 label = 'TeenNumRank', values = ~rankTeen),
            list(range = c(~min(rankInfant),~max(rankInfant)),
                 label = 'InfantNumRank', values = ~rankInfant),
            list(range = c(~min(rankShowerNumber),~max(rankShowerNumber)),
                 label = 'NumShowerRank', values = ~rankShowerNumber),
            list(range = c(~min(rankReplacedShower),~max(rankReplacedShower)),
                 label = 'NumReplacedHead', values = ~rankReplacedShower),
            list(range = c(~min(rankAdultatHome),~max(rankAdultatHome)),
                 label = 'UnempRank', values = ~rankAdultatHome),
            list(range = c(~min(rankAdultEmployed),~max(rankAdultEmployed)),
                 label = 'EmployedRank', values = ~rankAdultEmployed),
            list(range = c(~min(rankHomeAge),~max(rankHomeAge)),
                 label = 'HomeAgeRank', values = ~rankHomeAge),
            list(range = c(~min(rankOwnRent),~max(rankOwnRent)),
                 label = 'rentOwnRank', values = ~rankOwnRent),
            list(range = c(~min(rankEventWiseVOl),~max(rankEventWiseVOl)),
                 label = 'showerEventVolRank', values = ~rankEventWiseVOl),
            list(range = c(~min(rankDailyAvg),~max(rankDailyAvg)),
                 label = 'ShowerDailyRank', values = ~rankDailyAvg),
            list(range = c(~min(rankShowerEvent),~max(rankShowerEvent)),
                 label = 'ShowerEventNumRank', values = ~rankShowerEvent),
            list(range = c(~min(rankShowerTimeM),~max(rankShowerTimeM)),
                 label = 'ShowerTimeRank', values = ~rankShowerTimeM),
            list(range = c(~min(rankConRes),~max(rankConRes)),
                 label = 'ConserveResponseRank', values = ~rankConRes),
            list(range = c(~min(rankREUWSRes),~max(rankREUWSRes)),
                 label = 'REUWSResponseRank', values = ~rankREUWSRes),
            list(range = c(~min(rankAvgShowerDailyVol),~max(rankAvgShowerDailyVol)),
                 label = 'RankAvgEventWiseShowerUse', values = ~rankAvgShowerDailyVol)
          )
  )

q



