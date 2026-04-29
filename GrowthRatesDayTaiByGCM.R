# Script to calculate monthly growth rates for each location closest to Tai's farms, RCP and decade
# go through all GCM output files, pick up all daily growth rates for each month 
# separately and calculate mean and variability stats. Growth rates in 
# kg DM/day. One file for each farm and rcp, containing all stats for each decade


library(tictoc)
library(dplyr)
library(lubridate)
library(matrixStats)
library(moments)
library(ggplot2)

tic()
ff <- c(39, 28, 28, 39, 37, 31, 24, 38, 61, 72, 60, 56) # Matamata facts and figures PGR

PM <- c(175.675, -37.525, "PiakoMatamata" , 60) # Piako/Matamata farm location
H <- c(175.525, -37.225, "Hauraki", 40) # Hauraki farm location
TK <- c(175.175, -37.425, "TeKauwhata" , 55) # Te Kauwhata farm location
Ot <- c(175.225, -38.175, "Otorohanga" , 60) # Otorohanga farm location
To <- c(175.825, -38.175, "Tokoroa" , 65) # Tokoroa farm location

TK <- c(175.175, -37.375, "TeKauwhata", 57)  # -37.399735 ,  175.159312            
Ot <- c(175.225, -38.175, "Otorohanga", 62)  # -38.155292 ,   175.171078        
To <- c(175.825, -38.175, "Tokoroa", 50)     # -38.240302,    175.84355076        
PM <- c(175.675, -37.525, "Matamata", 66)    # -37.526226,    175.655988            
H  <- c(175.525, -37.225, "Hauraki", 43) # -37.210558,     175.544438     

# Tai's rerun locations
PMn <- c(175.675, -37.625, "MatamataNew", 60)    # -37.526226,    175.655988            
Hn  <- c(175.525, -37.325, "HaurakiNew", 40) # -37.210558,     175.544438 

# 

Gl <- c(168.77501, -46.275, "Gleneden", 50) # Farm in Edendale
CO2 <- 475 #385 #350 # #  

Tai_farms <- rbind(TK, Ot, To, PM, H) 
Tai_farms <- rbind(Hn, PMn)
Tai_farms <- rbind(Gl, Gl)
colnames(Tai_farms) <- c("Longitude", "Latitude", "FarmID", "PAW")
Tai_farms <- as.data.frame(Tai_farms)

longs <- Tai_farms$Longitude
lats <- Tai_farms$Latitude
longlats <- paste0(longs, "_", lats)

models <- c("BCC-CSM1.1", "HadGEM2-ES", "GFDL-CM3", "GISS-E", "NorESM1-M", "CESM1-CAM5")
slice_starts <- c(2010) #, 2040, 2090) #1980, 1990, 2010, 2040, 2070, 2090) # 
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
rcps <- c("RCP8.5", "RCP4.5") 

##################delete############
models <- c("BCC-CSM1.1", "NorESM1-M", "CESM1-CAM5", "GFDL-CM3", "GISS-E", "HadGEM2-ES") 
slice_starts <- c(2090) #, 2090) # 2010) #,  # run one decade at a time (otherwise needs fixing)
rcps <- c("RCP4.5", "RCP8.5") 
##########################

simnum <- 0 # simulation number
normally_distributed_data <- c()
          
for (j in 1:dim(Tai_farms)[1]){
  
  longlat <- longlats[j]
  PAW <- Tai_farms$PAW[j]
  
  for (rcp in rcps){
    rcp_name <- rcp
    
    # data frame to hold monthly averages and other stats of each location 
    stats <- data.frame(matrix(ncol = 6, nrow = 135))
    
    for (y in 1:length(slice_starts)){
      
      slice_start <- slice_starts[y]
      
      cat(file = stderr(), slice_start, " ")
      
      # set time slice length and time slice range
      slice_length <- 10
      slice_ends <- slice_start + slice_length
      year_range <- paste0(slice_start, "-", slice_ends)
      first_five <- paste0(slice_start, "-", slice_start+4)
      
      if ((slice_start==1980) | (slice_start==1990)){
        rcp <- "RCPpast"
      } else {
        rcp <- rcp_name
      }
    
     
      
      
      for (i in 1:length(models)) {
        
        model <- models[i]
        
        # read the basgra output file and extract "year", "doy" and "DRM" columns
        #basgra_output_file <- paste("D:/BASGRA Output New/", model, "/", rcp, "/", year_range, "/output_", model, "_", rcp, "_", year_range, "_", longlat, "PAW", PAW, ".csv", sep = "")
        basgra_output_file <- paste("I:/Andrea/BASGRAY/Output for Tai/Output - for Tai/", model, "/", rcp, "/", year_range, "/output_", model, "_", rcp, "_", year_range, "_", longlat, "PAW", PAW, ".csv", sep = "")
        basgra_output_file <- paste("I:/Andrea/BASGRA/Output for Tai/", Tai_farms$FarmID[j], "/", model, "/", rcp, "/", year_range, "/output_", model, "_", rcp, "_", year_range, "_", longlat, "PAW", PAW, ".csv", sep = "")
        
        if (Tai_farms$FarmID[j] == "Gleneden"){
          basgra_output_file <- paste("I:/Andrea/BASGRA/Output for Edendale/", rcp, "/", model, "/", year_range, "/output_", model, "_", rcp, "_", year_range, "_", longlat, "_PAW", PAW, ".csv", sep = "")
          basgra_output_file <- paste("I:/Andrea/BASGRA/Output for Tai/", Tai_farms$FarmID[j], "/output_", model, "_", rcp, "_", year_range, "_", longlat, "_PAW", PAW, "_CO2", CO2, ".csv", sep = "")
        }
        
        
        basgra_output_filtered <- read.csv(file = basgra_output_file) %>%
          as_tibble %>%
          select("year", "doy","RDM") # "HARVFR", "DM",
        
        # filter out first five years
        basgra_output_filteredA <- filter(basgra_output_filtered, year %in% seq(slice_start, slice_start+4))
        basgra_output_filteredB <- filter(basgra_output_filtered, year == (slice_start+5) , doy<122)
        basgra_output_filtered <- rbind(basgra_output_filteredA, basgra_output_filteredB)
        
        # data frame to hold all daily growth rates
        daily <- data.frame(matrix(ncol = 16, nrow = 0))
        
        # monthly growth rate data
        jan_monthly_data <- data.frame(matrix(ncol = 3, nrow = 0))
        feb_monthly_data <- data.frame(matrix(ncol = 3, nrow = 0))
        mar_monthly_data <- data.frame(matrix(ncol = 3, nrow = 0))
        apr_monthly_data <- data.frame(matrix(ncol = 3, nrow = 0))
        may_monthly_data <- data.frame(matrix(ncol = 3, nrow = 0))
        jun_monthly_data <- data.frame(matrix(ncol = 3, nrow = 0))
        jul_monthly_data <- data.frame(matrix(ncol = 3, nrow = 0))
        aug_monthly_data <- data.frame(matrix(ncol = 3, nrow = 0))
        sep_monthly_data <- data.frame(matrix(ncol = 3, nrow = 0))
        oct_monthly_data <- data.frame(matrix(ncol = 3, nrow = 0))
        nov_monthly_data <- data.frame(matrix(ncol = 3, nrow = 0))
        dec_monthly_data <- data.frame(matrix(ncol = 3, nrow = 0))
        
        # filter out rates by month (accounting for leap years )
        jan_daily_data <- na.omit(filter(basgra_output_filtered, doy %in% (1:31)))
        feb_daily_data <- na.omit(rbind(filter(basgra_output_filtered, doy %in% (32:59) & !(leap_year(year))), filter(basgra_output_filtered, doy %in% (32:60) & leap_year(year))))
        mar_daily_data <- na.omit(rbind(filter(basgra_output_filtered, doy %in% (60:90) & !(leap_year(year))), filter(basgra_output_filtered, doy %in% (61:91) & leap_year(year))))
        apr_daily_data <- na.omit(rbind(filter(basgra_output_filtered, doy %in% (91:120) & !(leap_year(year))), filter(basgra_output_filtered, doy %in% (92:121) & leap_year(year))))
        may_daily_data <- na.omit(rbind(filter(basgra_output_filtered, doy %in% (121:151) & !(leap_year(year))), filter(basgra_output_filtered, doy %in% (122:152) & leap_year(year))))
        jun_daily_data <- na.omit(rbind(filter(basgra_output_filtered, doy %in% (152:181) & !(leap_year(year))), filter(basgra_output_filtered, doy %in% (153:182) & leap_year(year))))
        jul_daily_data <- na.omit(rbind(filter(basgra_output_filtered, doy %in% (182:212) & !(leap_year(year))), filter(basgra_output_filtered, doy %in% (183:213) & leap_year(year))))
        aug_daily_data <- na.omit(rbind(filter(basgra_output_filtered, doy %in% (213:243) & !(leap_year(year))), filter(basgra_output_filtered, doy %in% (214:244) & leap_year(year))))
        sep_daily_data <- na.omit(rbind(filter(basgra_output_filtered, doy %in% (244:273) & !(leap_year(year))), filter(basgra_output_filtered, doy %in% (245:274) & leap_year(year))))
        oct_daily_data <- na.omit(rbind(filter(basgra_output_filtered, doy %in% (274:304) & !(leap_year(year))), filter(basgra_output_filtered, doy %in% (275:305) & leap_year(year))))
        nov_daily_data <- na.omit(rbind(filter(basgra_output_filtered, doy %in% (305:334) & !(leap_year(year))), filter(basgra_output_filtered, doy %in% (306:335) & leap_year(year))))
        dec_daily_data <- na.omit(rbind(filter(basgra_output_filtered, doy %in% (335:365) & !(leap_year(year))), filter(basgra_output_filtered, doy %in% (336:366) & leap_year(year))))
        
        # extend daily_data matrix so all matrices are the same length (and can thus be stored together in a larger matrix)
        nas <- rep(c(NA), (310-length(jan_daily_data$RDM))*3)
        navec <- matrix(nas, nrow=310-length(jan_daily_data$RDM), ncol=3) 
        colnames(navec) <- c("year", "doy","RDM")
        jan_daily_data_filled <- rbind(jan_daily_data, navec)
        
        nas <- rep(c(NA), (310-length(feb_daily_data$RDM))*3)
        navec <- matrix(nas, nrow=310-length(feb_daily_data$RDM), ncol=3) 
        colnames(navec) <- c("year", "doy","RDM")
        feb_daily_data_filled <- rbind(feb_daily_data, navec)
        
        nas <- rep(c(NA), (310-length(mar_daily_data$RDM))*3)
        navec <- matrix(nas, nrow=310-length(mar_daily_data$RDM), ncol=3) 
        colnames(navec) <- c("year", "doy","RDM")
        mar_daily_data_filled <- rbind(mar_daily_data, navec)
        
        nas <- rep(c(NA), (310-length(apr_daily_data$RDM))*3)
        navec <- matrix(nas, nrow=310-length(apr_daily_data$RDM), ncol=3) 
        colnames(navec) <- c("year", "doy","RDM")
        apr_daily_data_filled <- rbind(apr_daily_data, navec)
        
        nas <- rep(c(NA), (310-length(may_daily_data$RDM))*3)
        navec <- matrix(nas, nrow=310-length(may_daily_data$RDM), ncol=3) 
        colnames(navec) <- c("year", "doy","RDM")
        may_daily_data_filled <- rbind(may_daily_data, navec)
        
        nas <- rep(c(NA), (310-length(jun_daily_data$RDM))*3)
        navec <- matrix(nas, nrow=310-length(jun_daily_data$RDM), ncol=3) 
        colnames(navec) <- c("year", "doy","RDM")
        jun_daily_data_filled <- rbind(jun_daily_data, navec)
        
        nas <- rep(c(NA), (310-length(jul_daily_data$RDM))*3)
        navec <- matrix(nas, nrow=310-length(jul_daily_data$RDM), ncol=3) 
        colnames(navec) <- c("year", "doy","RDM")
        jul_daily_data_filled <- rbind(jul_daily_data, navec)
        
        nas <- rep(c(NA), (310-length(aug_daily_data$RDM))*3)
        navec <- matrix(nas, nrow=310-length(aug_daily_data$RDM), ncol=3) 
        colnames(navec) <- c("year", "doy","RDM")
        aug_daily_data_filled <- rbind(aug_daily_data, navec)
        
        nas <- rep(c(NA), (310-length(sep_daily_data$RDM))*3)
        navec <- matrix(nas, nrow=310-length(sep_daily_data$RDM), ncol=3) 
        colnames(navec) <- c("year", "doy","RDM")
        sep_daily_data_filled <- rbind(sep_daily_data, navec)
        
        nas <- rep(c(NA), (310-length(oct_daily_data$RDM))*3)
        navec <- matrix(nas, nrow=310-length(oct_daily_data$RDM), ncol=3) 
        colnames(navec) <- c("year", "doy","RDM")
        oct_daily_data_filled <- rbind(oct_daily_data, navec)
        
        nas <- rep(c(NA), (310-length(nov_daily_data$RDM))*3)
        navec <- matrix(nas, nrow=310-length(nov_daily_data$RDM), ncol=3) 
        colnames(navec) <- c("year", "doy","RDM")
        nov_daily_data_filled <- rbind(nov_daily_data, navec)
        
        nas <- rep(c(NA), (310-length(dec_daily_data$RDM))*3)
        navec <- matrix(nas, nrow=310-length(dec_daily_data$RDM), ncol=3) 
        colnames(navec) <- c("year", "doy","RDM")
        dec_daily_data_filled <- rbind(dec_daily_data, navec)
        
        # form an intermediate matrix with model in first column, years in next column and daily growth rates in subsequent columns
        model_daily <- cbind(jan_daily_data_filled$RDM, feb_daily_data_filled$RDM, mar_daily_data_filled$RDM, apr_daily_data_filled$RDM, may_daily_data_filled$RDM, jun_daily_data_filled$RDM, jul_daily_data_filled$RDM, aug_daily_data_filled$RDM, sep_daily_data_filled$RDM, oct_daily_data_filled$RDM, nov_daily_data_filled$RDM, dec_daily_data_filled$RDM)
        
        ### # append intermediate model_daily matrix onto daily matrix (which holds daily growth rates for all the models)
        ### daily <- rbind(daily, model_daily) 
        
        colnames(model_daily) <- months
  
        # calculate average daily growth rates and other variability stats
        for (k in 1:(length(months))){
          stats[k,i] <- mean(as.numeric(as.character(model_daily[,k])), na.rm = TRUE)
          stats[k+12,i] <- median(as.numeric(as.character(model_daily[,k])), na.rm = TRUE)
          stats[k+24,i] <- min(as.numeric(as.character(model_daily[,k])), na.rm = TRUE)
          stats[k+36,i] <- max(as.numeric(as.character(model_daily[,k])), na.rm = TRUE)
          stats[k+48,i] <- var(as.numeric(as.character(model_daily[,k])), na.rm = TRUE)
          stats[k+60,i] <- sd(as.numeric(as.character(model_daily[,k])), na.rm = TRUE)
          stats[k+72,i] <- quantile(as.numeric(as.character(model_daily[,k])),probs = 0.05, na.rm = TRUE, names = TRUE, type = 7)
          stats[k+84,i] <- quantile(as.numeric(as.character(model_daily[,k])),probs = 0.25, na.rm = TRUE, names = TRUE, type = 7)
          stats[k+96,i] <- quantile(as.numeric(as.character(model_daily[,k])),probs = 0.75, na.rm = TRUE, names = TRUE, type = 7)
          stats[k+108,i] <- quantile(as.numeric(as.character(model_daily[,k])),probs = 0.95, na.rm = TRUE, names = TRUE, type = 7)
          stats[k+120,i] <- shapiro.test(as.numeric(as.character(model_daily[,k])))$p.value
          if (stats[k+120,i]>0.05){
            norm <- c(longlat, first_five, PAW, rcp, months[k], stats[k+122,i])
            normally_distributed_data <- rbind(normally_distributed_data, norm)
          }
          stats[k+132,i] <- kurtosis(as.numeric(as.character(model_daily[,k])), na.rm = TRUE)
        }
        simnum <- simnum + 1
        if (simnum%%10==0){
          cat(file = stderr(), simnum, " ")
        }
      }
      
      
      
      # not possible to calculate covariance matrix due to varying data amounts between months
    }
    
    
    statnames <- c("Mean", "Med", "Min", "Max", "Var", "SD", "Q05", "Q25", "Q75", "Q95", "SW", "Kurt")
    

    colnamevec <- as.character(models)

    
    stats <- rbind(colnamevec, stats)
    
    rownamevec <- rbind("kg DM/ha/day")
    for (stat in statnames){
      for (month in c(months)){
        rowname <- paste0(month, stat)
        rownamevec <- c(rownamevec, rowname)
      }
    }
    
    rownames(stats) <- rownamevec
    
    file_name <- paste("I:/Andrea/BASGRA/basgra_nz-2021/DeepSouth/RDMstats/RDMstatsDay_",  Tai_farms$FarmID[j], "Farm_", longlat, "_", rcp,"_", slice_start, "_PAW", PAW,  "_CO2", CO2, ".csv", sep="")
    write.table(stats, file=file_name, sep = ",", row.names = TRUE, col.names = FALSE)
    cat(file = stderr(), file_name, "\n")
    
    # file_name <- paste("I:/Andrea/BASGRA/basgra_nz-2021/DeepSouth/RDMstats/RDMstatsDay_",  Tai_farms$FarmID[j], "Farm_", longlat, "_", rcp, "_PAW", PAW,  ".csv", sep="")
    # #write.table(stats, file=file_name, sep = ",", row.names = TRUE, col.names = FALSE)
    # ylimval <- c(-5,70)
    # plot(stats[2:13,1], main=rcp, col="yellow", ylim=ylimval)
    # lines(stats[2:13,1], col="yellow", ylim=ylimval)
    # par(new=TRUE)
    # plot(stats[2:13,2], col="orange", ylim=ylimval)
    # lines(stats[2:13,2], col="orange", ylim=ylimval)
    # par(new=TRUE)
    # plot(stats[2:13,3], col="red", ylim=ylimval)
    # lines(stats[2:13,3], col="red", ylim=ylimval)
    # par(new=TRUE)
    # plot(stats[2:13,4], col="green", ylim=ylimval)
    # lines(stats[2:13,4], col="green", ylim=ylimval)
    # par(new=TRUE)
    # plot(stats[2:13,5], col="blue", ylim=ylimval)
    # lines(stats[2:13,5], col="blue", ylim=ylimval)
    # par(new=TRUE)
    # plot(stats[2:13,6], col="purple", ylim=ylimval)
    # lines(stats[2:13,6], col="purple", ylim=ylimval)
    # par(new=TRUE)
    # plot(ff, col="black", ylim=ylimval)
    # lines(ff, col="black", ylim=ylimval)
  }
}



# #colnames(normally_distributed_data) <- c("location", "year", "PAW30", "RCP", "month", "Shapiro p-value" )
# file_name <- paste("I:/Andrea/basgra_nz-2021/DeepSouth/RDMstats/NormalMonthlyPGR.csv", sep="")
# #write.table(normally_distributed_data, file=file_name, sep = ",", row.names = FALSE, col.names = TRUE)
# 
# cat(file = stderr(), "total number of simulations:", simnum, "\n")
# 
# # plot PDF of daily data
# plot(density(na.omit(daily$Jan)), xlim=c(-50, 125))
# abline(v=mean(na.omit(daily$Jan)), col="blue")
# abline(v=median(na.omit(daily$Jan)), col="red")
# par(new=TRUE)
# hist(na.omit(daily$Jan), breaks=100, xlim=c(-50, 125))#, xlim=c(min(density(na.omit(daily$Jan))),max(density(na.omit(daily$Jan))))) 
# 
# 
# par(new=FALSE)
# plot(density(na.omit(daily$Feb)))
# plot(density(na.omit(daily$Mar)))
# plot(density(na.omit(daily$Apr)))
# plot(density(na.omit(daily$May)))
# plot(density(na.omit(daily$Jun)))
# plot(density(na.omit(daily$Jul)))
# plot(density(na.omit(daily$Aug)))
# plot(density(na.omit(daily$Sep)))
# plot(density(na.omit(daily$Oct)))
# plot(density(na.omit(daily$Nov)))
# plot(density(na.omit(daily$Dec)))

toc()