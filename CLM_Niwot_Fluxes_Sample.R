# CLM_Niwot_Fluxes_Sample.R
# Script created by Will Wieder on July 2016 with modifications on Jan 2017
# Modified by Danica Lombardozzi on July 2019
# Sample code to read in .nc files and plots data

# Reads in .nc file with half hourly data from CLM4.5 simulation for T-Van tower at Niwot Ridge
# Data published in Wieder, W. et al. (2017). JGResearch-Biogeosciences, 122(4), 825–845.
# plots mean annual cycle of GPP

# USER REQUIRED CHANGES:
# define the directory (see 'dir' variable) and path where files are located on local machine !! 

# USER OPTIONAL CHANGES:
# Data files are available for 5 different vegetation patches (see 'vegtype' variable)
# For each vegetation patch, there are three treatment options (see 'case' variable)
# User can change variable of interest (see 'GPP' variable)


#-------------- Setting up environment -------------------------------

# Note: if you don't already have the ncdf4 package installed, 
# you will need to type "install.packages("ncdf4")" in the command line
remove(list=ls())
library(ncdf4)


#---------------read in CLM variables----------------------------------
# Note: Change the directory to match the location of where you downloaded the data files
dir      <- ('/Users/dll/Desktop/clm_niwot_data_archive/Model_output/')
vegtype  <- 'Snowbed'
# Options: DryMeadow, Fellfield, MoistMeadow, Snowbed, WetMeadow
path     <- paste(dir,vegtype,'/',sep='')
print(path)

case     <- c('Control')
# Options: Control, Warming, EarlyMelt
suf      <- ('.nc')
infile   <- paste(path,vegtype,'_',case,suf,sep='')
print(infile)

Data.clm <- nc_open(infile) 

#--- List  variables and metadata ---
# Note: This prints all the variables in the file, as well as the metadata (description, units, etc.)
print(paste("The file has",Data.clm$nvars,"variables"))
print(Data.clm)


#--- Get the variables from the file ---
# Note: Here we will use GPP as an example
#       You can select any of the "float" variables from the file for analysis (from "print(Data.clm") above)
# Also note that you will need to update the Y-axis title if you select a different variable
nsteps   <- 48 * (365*6 + 2)
MCDATE   <- ncvar_get(Data.clm, "mcdate")[1:nsteps]       #current date
MCSEC    <- ncvar_get(Data.clm, "mcsec")[1:nsteps]        #current seconds of current date
GPP      <- ncvar_get(Data.clm, "GPP")[1:nsteps]          #gross primiary productivity
GPPunits <- ncatt_get(Data.clm, "GPP", "units")           #getting units of variable
GPPunits
mean(GPP) * 3600 * 24 * 365    #gC/m^s/y

nc_close(Data.clm)

nsteps        <- length(MCDATE)
MCDATE[nsteps]
MCSEC[2]
MCDATE[nsteps-48]
n1 <- 365*48  #for no-leap   data
n2 <- n1 + 48 #for leap year data (leap)

# -- adjust CLM time to MST -- 

shift      <- 15  
mcdate     <- MCDATE[1:(nsteps-shift+1)]
mcsec      <- MCSEC[1:(nsteps-shift+1)]
mchour     <- mcsec/3600
gpp        <- GPP[1+shift:nsteps-1]      #omit last day (Jan 1, 2013)
nstep2     <- length(mcdate)

mcdate[1]
mcdate[nstep2]
day <- as.factor(mcdate)

DATE_mean    <- tapply(mcdate, day, mean)
ndaily       <- length(DATE_mean)

#-----------------------------------------------------------------------
# Set up date / time resources
#-----------------------------------------------------------------------
hour    <- seq(0,23.5,0.5)
days    <- c( 31  ,  28 ,  31 ,  30 ,   31,   30,   31,   31,   30,   31,   30,  31 ) 
month   <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
MONTH   <- c('01','02','03','04','05','06','07','08','09','10','11','12')
nMONTH  <- length(MONTH)
year    <- seq(2008,2013,1) 
nyear   <- length(year)

paste(year[1],'-',MONTH[1],'-01',sep='')
clmdate  <- as.Date(as.character(mcdate),format='%Y%m%d')
clmyear  <- as.Date(as.character(mcdate),format='%Y')
clmdate[1]
clmyear[1]
mcdate[1]

#-----------------------------------------------------------------------
# plot annual GPP values
#-----------------------------------------------------------------------
month   <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","")
days    <- c( 31  ,  28 ,  31 ,  30 ,   31,   30,   31,   31,   30,   31,   30,  31 ) 
totdays <- rep(0, 13)
for (m in 2:13) { totdays[m] <- sum(days[1:m-1]) }

par(mfrow=c(1,1),mar=c(0,0,0,0), oma=c(5,6,1,1))
clmday      <- (as.character(clmdate, format = "%m-%d")) 
clmyear     <- as.numeric(as.character(clmdate, format = "%Y")) 
years       <- seq(2008,2013,1)

temp <- gpp
GPP_mean <- tapply(temp, clmday, mean, na.rm=T) * 3600 * 24    #gC/m2/d
GPP_sd   <- tapply(temp, clmday, sd, na.rm=T)   * 3600 * 24    #gC/m2/d
x        <- seq(1,length(GPP_mean),1)
xx       <- c(x, rev(x))
yy       <- c((GPP_mean+GPP_sd),rev(GPP_mean-GPP_sd))

plot(GPP_mean, type="l", ylim=c(-3,6),lwd=3, xaxt='n')
polygon(xx,yy,col = rgb(0, 0, 0,0.5), border = NA)
axis(1, tck = 0.05, at=totdays, labels=NA)
mtext('GPP', side = 2, line = 4)
mtext(expression(paste("(gC ",m^-2," ",d^-1,")")), 
      side = 2, line = 2, cex=0.8) 
abline(h=0, lty=2) 
axis(1, tck = 0.05, at=totdays, labels=month, cex=1.2)

#----------------  END  ----------------------------------
#---------------------------------------------------------

