
#-----------------------------------------------------------------------------#
#                          SS HO MODEL - Data analysis                        #
#-----------------------------------------------------------------------------#

#### Based on datafile SS HO model_first&second order.R ####
		
test2 = HOmodel(waste_type="urea",
				Pcarb=0.85,
				Pprot=0.10,
				Pfat=1 - 0.85 - 0.10,
				M=50,
				bTc=37,
				Pw=0.60, 
				d18Oew=-10,
				eTc=20,
				Rh=0.50,				
				BMR=NULL,
				BMR_O2=NULL,
				FMR=NULL,
				TWF=NULL,
				Fdw=NULL,
				Fvw=NULL,
				d2Hew=NULL,
				d2Hleafw=NULL,
				d2Hcarb=NULL,
				d2Hprot=NULL, 
				d2Hfat=NULL,
				d2Hfw=NULL,
				d2Hdw=NULL,
				d18Oleafw=NULL,
				d18Ocarb=NULL,
				d18Oprot=NULL,
				d18Ofat=NULL,
				d18Ofw=NULL,
				d18Odw=NULL)
	
				
#### Sensitivity testing ####


# Sensitivity to change in the proportion of the total water flux derived from drinking (pFdw); flux of drinking water in (Fdw) varies between 1/5 and 5 times its default value (Fdw = 0.39 mol H2O d^-1)
testpFdw = HOmodel(waste_type="urea",
				Pcarb=0.85,
				Pprot=0.10,
				Pfat=1 - 0.85 - 0.10,
				M=50,
				bTc=37,
				Pw=0.60, 
				d18Oew=-10,
				eTc=20,
				Rh=0.50,				
				BMR=NULL,
				BMR_O2=NULL,
				FMR=NULL,
				TWF=NULL,
				Fdw=seq(0.39/5, 0.39*5, 0.01),
				Fvw=NULL,
				d2Hew=NULL,
				d2Hleafw=NULL,
				d2Hcarb=NULL,
				d2Hprot=NULL, 
				d2Hfat=NULL,
				d2Hfw=NULL,
				d2Hdw=NULL,
				d18Oleafw=NULL,
				d18Ocarb=NULL,
				d18Oprot=NULL,
				d18Ofat=NULL,
				d18Ofw=NULL,
				d18Odw=NULL)
	
				
# Sensitivity to change in the proportion of food mass that is in liquid water form (Pw); Pw varies between 0.00 and 0.99
testPw = HOmodel(waste_type="urea",
				Pcarb=0.85,
				Pprot=0.10,
				Pfat=1 - 0.85 - 0.10,
				M=50,
				bTc=37,
				Pw=seq(0.00, 0.99, 0.01), 
				d18Oew=-10,
				eTc=20,
				Rh=0.50,				
				BMR=NULL,
				BMR_O2=NULL,
				FMR=NULL,
				TWF=NULL,
				Fdw=NULL,
				Fvw=NULL,
				d2Hew=NULL,
				d2Hleafw=NULL,
				d2Hcarb=NULL,
				d2Hprot=NULL, 
				d2Hfat=NULL,
				d2Hfw=NULL,
				d2Hdw=NULL,
				d18Oleafw=NULL,
				d18Ocarb=NULL,
				d18Oprot=NULL,
				d18Ofat=NULL,
				d18Ofw=NULL,
				d18Odw=NULL)
	
				
# Sensitivity to change in field metabolic rate (FMR); FMR varies between 1/5 and 5 times its default value (FMR = 0.15 mol O2 d^-1)
testFMR = HOmodel(waste_type="urea",
				Pcarb=0.85,
				Pprot=0.10,
				Pfat=1 - 0.85 - 0.10,
				M=50,
				bTc=37,
				Pw=0.60, 
				d18Oew=-10,
				eTc=20,
				Rh=0.50,				
				BMR=NULL,
				BMR_O2=NULL,
				FMR=seq(0.15/5, 0.15*5, 0.01),
				TWF=NULL,
				Fdw=NULL,
				Fvw=NULL,
				d2Hew=NULL,
				d2Hleafw=NULL,
				d2Hcarb=NULL,
				d2Hprot=NULL, 
				d2Hfat=NULL,
				d2Hfw=NULL,
				d2Hdw=NULL,
				d18Oleafw=NULL,
				d18Ocarb=NULL,
				d18Oprot=NULL,
				d18Ofat=NULL,
				d18Ofw=NULL,
				d18Odw=NULL)	
	
				
# Sensitivity to change in macronutrient proportions in the diet (Pcarb, Pprot, Pfat); Pcarb varies between 0.85 and 0.00, Pprot varies between 0.10 and 0.80, and Pfat is calculated accordingly as 1 - Pcarb - Pprot; the extremes represent herbivore and hyper-carnivore diets 
Pcarb = seq(0.85,0.00,-0.01)
Pprot = seq(0.10,0.80,(0.80-0.10)/(length(Pcarb)-1))
Pfat = 1 - Pcarb - Pprot
testPs = HOmodel(waste_type="urea",
				Pcarb=Pcarb,
				Pprot=Pprot,
				Pfat=Pfat,
				M=50,
				bTc=37,
				Pw=0.60, 
				d18Oew=-10,
				eTc=20,
				Rh=0.50,				
				BMR=NULL,
				BMR_O2=NULL,
				FMR=NULL,
				TWF=NULL,
				Fdw=NULL,
				Fvw=NULL,
				d2Hew=NULL,
				d2Hleafw=NULL,
				d2Hcarb=NULL,
				d2Hprot=NULL, 
				d2Hfat=NULL,
				d2Hfw=NULL,
				d2Hdw=NULL,
				d18Oleafw=NULL,
				d18Ocarb=NULL,
				d18Oprot=NULL,
				d18Ofat=NULL,
				d18Ofw=NULL,
				d18Odw=NULL) 	
				
				
#### Scenario testing ####		


# Scenario 1): animals of varying body size; body mass varies between 10 and 10000 g
testM = HOmodel(waste_type="urea",
				Pcarb=0.85,
				Pprot=0.10,
				Pfat=1 - 0.85 - 0.10,
				M=seq(10, 10000, 10),
				bTc=37,
				Pw=0.60, 
				d18Oew=-10,
				eTc=20,
				Rh=0.50,				
				BMR=NULL,
				BMR_O2=NULL,
				FMR=NULL,
				TWF=NULL,
				Fdw=NULL,
				Fvw=NULL,
				d2Hew=NULL,
				d2Hleafw=NULL,
				d2Hcarb=NULL,
				d2Hprot=NULL, 
				d2Hfat=NULL,
				d2Hfw=NULL,
				d2Hdw=NULL,
				d18Oleafw=NULL,
				d18Ocarb=NULL,
				d18Oprot=NULL,
				d18Ofat=NULL,
				d18Ofw=NULL,
				d18Odw=NULL)
		
				
# Scenario 2): animals living along spatial environmental gradients in North America (NA) and Europe (EU); 2 cases: 
# 1- environmental temperature, relative humidity, and H and O isotope compositions of environmental water vary along transects in NA and EU; data are growing season averages
# 2- hmidity is kept constant 

library(rgdal)
library(maps)
library(mapproj)
library(mapdata)
library(fields)
library(spam)
library(pracma)
library(usdm)
library(SDMTools)
library(RColorBrewer)
library(sp)
library(raster)

# Read in temperature and humidity data (monthly climatologies), calculate growing season averages and convert to raster

Tdata <- read.table("/Users/Sarah/Dropbox/SS HO model/SS HO model ms Oecologia/R code/grid_10min_tmp.dat")
Temps <- Tdata[, c(3:14)]
GSmeanT <- apply(Temps, 1, function(x) mean (x[x > 0]))
Tdata <- cbind(Tdata[,c(1:2)], GSmeanT)
colnames(Tdata)[1:2] <- c("Lat", "Lon")
Tdata[is.na(Tdata)] <- 0

T_spdf <- SpatialPointsDataFrame(Tdata[c("Lon", "Lat")], data=data.frame(Tdata$GSmeanT), proj4string = CRS("+proj=longlat +datum=WGS84") )

T_rast_empty <- raster()
T_rast <- rasterize(T_spdf, T_rast_empty, T_spdf$Tdata.GSmeanT)
T_rast[T_rast <= 0] <- NA


Tdata <- read.table("/Users/Sarah/Dropbox/SS HO model/SS HO model ms Oecologia/R code/grid_10min_tmp.dat")
Rhdata <- read.table("/Users/Sarah/Dropbox/SS HO model/SS HO model ms Oecologia/R code/grid_10min_reh.dat")
Temps <- Tdata[, c(3:14)]
Rhs <- Rhdata[, c(3:14)]
Temps_pos <- replace(Temps, Temps <= 0, 0)
Temps_pos_1s <- replace(Temps_pos, Temps_pos > 0, 1)
Rhs_Temps_pos <- as.data.frame(as.matrix(Rhs) * as.matrix(Temps_pos_1s))
GSmeanRh <- apply(Rhs_Temps_pos, 1, function(x) mean (x[x > 0]))
head(GSmeanRh)
Rhdata <- cbind(Rhdata[,c(1:2)], GSmeanRh)
colnames(Rhdata)[1:2] <- c("Lat", "Lon")
Rhdata[is.na(Rhdata)] <- 0

Rh_spdf <- SpatialPointsDataFrame(Rhdata[c("Lon", "Lat")], data=data.frame(Rhdata$GSmeanRh), proj4string = CRS("+proj=longlat +datum=WGS84") )

Rh_rast_empty <- raster()
Rh_rast <- rasterize(Rh_spdf, Rh_rast_empty, Rh_spdf$Rhdata.GSmeanRh)
Rh_rast[Rh_rast <= 0] <- NA

# Read in growing season precipitation H and O isotope data (waterisotopes.org, Bowen et al. 2005) 
Hgs <- raster("/Users/Sarah/Dropbox/SS HO model/SS HO model ms Oecologia/R code/GSIsotopeMaps/Hgs.asc", gz=FALSE)
projection(Hgs) =  CRS("+proj=longlat +datum=WGS84")

Ogs <- raster("/Users/Sarah/Dropbox/SS HO model/SS HO model ms Oecologia/R code/GSIsotopeMaps/Ogs.asc", gz=FALSE)
projection(Ogs) =  CRS("+proj=longlat +datum=WGS84")

# Extract data for each point along transects in NA and EU  
NA_transect <- readOGR(dsn="/Users/Sarah/Dropbox/SS HO model/SS HO model ms Oecologia/R code", layer="NA_transect")
EU_transect <- readOGR(dsn="/Users/Sarah/Dropbox/SS HO model/SS HO model ms Oecologia/R code", layer="EU_transect")
 
transectpoints <- spsample(NA_transect, 500, "regular")
Tpoints <- extract(T_rast, transectpoints)
Rhpoints <- extract(Rh_rast, transectpoints)
d2Hpoints <- extract(Hgs, transectpoints)
d18Opoints <- extract(Ogs, transectpoints)
pointdata <- as.data.frame(cbind(d2Hpoints, d18Opoints, Tpoints, Rhpoints, coordinates(transectpoints)))
colnames(pointdata)[5:6] <- c("Lon", "Lat")
NA_pointdata <- pointdata 

transectpoints <- spsample(EU_transect, 500, "regular")
Tpoints <- extract(T_rast, transectpoints)
Rhpoints <- extract(Rh_rast, transectpoints)
d2Hpoints <- extract(Hgs, transectpoints)
d18Opoints <- extract(Ogs, transectpoints)
pointdata <- as.data.frame(cbind(d2Hpoints, d18Opoints, Tpoints, Rhpoints, coordinates(transectpoints)))
colnames(pointdata)[5:6] <- c("Lon", "Lat")
EU_pointdata <- pointdata

NA_pointdata$tr = "NA"
EU_pointdata$tr = "EU"
sc2_dataset = rbind(NA_pointdata, EU_pointdata)

# Calculate the distance (in km) of each point along each transect
NA_transectpoints = NA_pointdata
coordinates(NA_transectpoints) = cbind(NA_pointdata$Lon, NA_pointdata$Lat)
projection(NA_transectpoints) <- CRS("+proj=longlat +datum=WGS84")

NA_transectdist <- vector("numeric", length=length(NA_transectpoints))
NA_transectdist[1] <- 0
for (i in 2:length(NA_transectdist)) {
  point1 <- coordinates(NA_transectpoints)[i,]
  point2 <- coordinates(NA_transectpoints)[(i-1),]
  NA_transectdist[i] <- (pointDistance(point1, point2, lonlat=T)/1000) + NA_transectdist[i-1]
}
NA_pointdata$transectdist = NA_transectdist

EU_pointdata = EU_pointdata[order(-EU_pointdata$Lon),]
EU_transectpoints = EU_pointdata
coordinates(EU_transectpoints) = cbind(EU_pointdata$Lon, EU_pointdata$Lat)

EU_transectdist <- vector("numeric", length=length(EU_transectpoints))
EU_transectdist[1] <- 0
for (i in 2:length(EU_transectdist)) {
  point1 <- coordinates(EU_transectpoints)[i,]
  point2 <- coordinates(EU_transectpoints)[(i-1),]
  EU_transectdist[i] <- (pointDistance(point1, point2, lonlat=T)/1000) + EU_transectdist[i-1]
}
EU_pointdata$transectdist = EU_transectdist

# case 1-
testNA.1 = HOmodel(waste_type="urea",
				Pcarb=0.85,
				Pprot=0.10,
				Pfat=1 - 0.85 - 0.10,
				M=50,
				bTc=37,
				Pw=0.60, 
				d18Oew=NA_pointdata$d18Opoints,
				eTc=NA_pointdata$Tpoints,
				Rh=NA_pointdata$Rhpoints/100,				
				BMR=NULL,
				BMR_O2=NULL,
				FMR=NULL,
				TWF=NULL,
				Fdw=NULL,
				Fvw=NULL,
				d2Hew=NA_pointdata$d2Hpoints,
				d2Hleafw=NULL,
				d2Hcarb=NULL,
				d2Hprot=NULL, 
				d2Hfat=NULL,
				d2Hfw=NULL,
				d2Hdw=NULL,
				d18Oleafw=NULL,
				d18Ocarb=NULL,
				d18Oprot=NULL,
				d18Ofat=NULL,
				d18Ofw=NULL,
				d18Odw=NULL)
				
testEU.1 = HOmodel(waste_type="urea",
				Pcarb=0.85,
				Pprot=0.10,
				Pfat=1 - 0.85 - 0.10,
				M=50,
				bTc=37,
				Pw=0.60, 
				d18Oew=EU_pointdata$d18Opoints,
				eTc=EU_pointdata$Tpoints,
				Rh=EU_pointdata$Rhpoints/100,				
				BMR=NULL,
				BMR_O2=NULL,
				FMR=NULL,
				TWF=NULL,
				Fdw=NULL,
				Fvw=NULL,
				d2Hew=EU_pointdata$d2Hpoints,
				d2Hleafw=NULL,
				d2Hcarb=NULL,
				d2Hprot=NULL, 
				d2Hfat=NULL,
				d2Hfw=NULL,
				d2Hdw=NULL,
				d18Oleafw=NULL,
				d18Ocarb=NULL,
				d18Oprot=NULL,
				d18Ofat=NULL,
				d18Ofw=NULL,
				d18Odw=NULL)
				
# case 2-				
testNA.2 = HOmodel(waste_type="urea",
				Pcarb=0.85,
				Pprot=0.10,
				Pfat=1 - 0.85 - 0.10,
				M=50,
				bTc=37,
				Pw=0.60, 
				d18Oew=NA_pointdata$d18Opoints,
				eTc=NA_pointdata$Tpoints,
				Rh=0.50,				
				BMR=NULL,
				BMR_O2=NULL,
				FMR=NULL,
				TWF=NULL,
				Fdw=NULL,
				Fvw=NULL,
				d2Hew=NA_pointdata$d2Hpoints,
				d2Hleafw=NULL,
				d2Hcarb=NULL,
				d2Hprot=NULL, 
				d2Hfat=NULL,
				d2Hfw=NULL,
				d2Hdw=NULL,
				d18Oleafw=NULL,
				d18Ocarb=NULL,
				d18Oprot=NULL,
				d18Ofat=NULL,
				d18Ofw=NULL,
				d18Odw=NULL)
				
testEU.2 = HOmodel(waste_type="urea",
				Pcarb=0.85,
				Pprot=0.10,
				Pfat=1 - 0.85 - 0.10,
				M=50,
				bTc=37,
				Pw=0.60, 
				d18Oew=EU_pointdata$d18Opoints,
				eTc=EU_pointdata$Tpoints,
				Rh=0.50,				
				BMR=NULL,
				BMR_O2=NULL,
				FMR=NULL,
				TWF=NULL,
				Fdw=NULL,
				Fvw=NULL,
				d2Hew=EU_pointdata$d2Hpoints,
				d2Hleafw=NULL,
				d2Hcarb=NULL,
				d2Hprot=NULL, 
				d2Hfat=NULL,
				d2Hfw=NULL,
				d2Hdw=NULL,
				d18Oleafw=NULL,
				d18Ocarb=NULL,
				d18Oprot=NULL,
				d18Ofat=NULL,
				d18Ofw=NULL,
				d18Odw=NULL)					
	
				
# Scenario 3): animals feeding at trophic levels 1-4 within a local food web; 2 cases: 
# 1- only HO isotopic compositions of food and food water are varied; HO isotopic composotions of food water for TL = n equal those of body water predicted for TL = n-1; HO isotopic compositions of dietary protein for TL = n equal those of keratin predicted for TL = n-1; those of carbohydrate and lipid are derived from protein 
# 2- M and Pcarb, Pprot, Pfat are also varied; M between 10 and 10000 g, Pcarb between 0.85 and 0.00, Pprot between 0.10 and 0.80, and Pfat is calculated accordingly as 1 - Pcarb - Pprot 

# case 1- 
testTL1.1 = HOmodel(waste_type="urea",
				Pcarb=0.85,
				Pprot=0.10,
				Pfat=1 - 0.85 - 0.10,
				M=50,
				bTc=37,
				Pw=0.60, 
				d18Oew=-10,
				eTc=20,
				Rh=0.50,				
				BMR=NULL,
				BMR_O2=NULL,
				FMR=NULL,
				TWF=NULL,
				Fdw=NULL,
				Fvw=NULL,
				d2Hew=NULL,
				d2Hleafw=NULL,
				d2Hcarb=NULL,
				d2Hprot=NULL, 
				d2Hfat=NULL,
				d2Hfw=NULL,
				d2Hdw=NULL,
				d18Oleafw=NULL,
				d18Ocarb=NULL,
				d18Oprot=NULL,
				d18Ofat=NULL,
				d18Ofw=NULL,
				d18Odw=NULL)
				
testTL2.1 = HOmodel(waste_type="urea",
				Pcarb=0.85,
				Pprot=0.10,
				Pfat=1 - 0.85 - 0.10,
				M=50,
				bTc=37,
				Pw=0.60, 
				d18Oew=-10,
				eTc=20,
				Rh=0.50,				
				BMR=NULL,
				BMR_O2=NULL,
				FMR=NULL,
				TWF=NULL,
				Fdw=NULL,
				Fvw=NULL,
				d2Hew=NULL,
				d2Hleafw=NULL,
				d2Hcarb=testTL1.1$d2Hker + 40,
				d2Hprot=testTL1.1$d2Hker, 
				d2Hfat=testTL1.1$d2Hker - 53.42365,
				d2Hfw=testTL1.1$d2Hbw,
				d2Hdw=NULL,
				d18Oleafw=NULL,
				d18Ocarb=testTL1.1$d18Oker + 8.063528,
				d18Oprot=testTL1.1$d18Oker,
				d18Ofat=testTL1.1$d18Oker - 6,
				d18Ofw=testTL1.1$d18Obw,
				d18Odw=NULL)
				
testTL3.1 = HOmodel(waste_type="urea",
				Pcarb=0.85,
				Pprot=0.10,
				Pfat=1 - 0.85 - 0.10,
				M=50,
				bTc=37,
				Pw=0.60, 
				d18Oew=-10,
				eTc=20,
				Rh=0.50,				
				BMR=NULL,
				BMR_O2=NULL,
				FMR=NULL,
				TWF=NULL,
				Fdw=NULL,
				Fvw=NULL,
				d2Hew=NULL,
				d2Hleafw=NULL,
				d2Hcarb=testTL2.1$d2Hker + 40,
				d2Hprot=testTL2.1$d2Hker, 
				d2Hfat=testTL2.1$d2Hker - 53.42365,
				d2Hfw=testTL2.1$d2Hbw,
				d2Hdw=NULL,
				d18Oleafw=NULL,
				d18Ocarb=testTL2.1$d18Oker + 8.063528,
				d18Oprot=testTL2.1$d18Oker,
				d18Ofat=testTL2.1$d18Oker - 6,
				d18Ofw=testTL2.1$d18Obw,
				d18Odw=NULL)

testTL4.1 = HOmodel(waste_type="urea",
				Pcarb=0.85,
				Pprot=0.10,
				Pfat=1 - 0.85 - 0.10,
				M=50,
				bTc=37,
				Pw=0.60, 
				d18Oew=-10,
				eTc=20,
				Rh=0.50,				
				BMR=NULL,
				BMR_O2=NULL,
				FMR=NULL,
				TWF=NULL,
				Fdw=NULL,
				Fvw=NULL,
				d2Hew=NULL,
				d2Hleafw=NULL,
				d2Hcarb=testTL3.1$d2Hker + 40,
				d2Hprot=testTL3.1$d2Hker, 
				d2Hfat=testTL3.1$d2Hker - 53.42365,
				d2Hfw=testTL3.1$d2Hbw,
				d2Hdw=NULL,
				d18Oleafw=NULL,
				d18Ocarb=testTL3.1$d18Oker + 8.063528,
				d18Oprot=testTL3.1$d18Oker,
				d18Ofat=testTL3.1$d18Oker - 6,
				d18Ofw=testTL3.1$d18Obw,
				d18Odw=NULL)								

# case 2-
testTL1.2 = HOmodel(waste_type="urea",
				Pcarb=0.85,
				Pprot=0.10,
				Pfat=1 - 0.85 - 0.10,
				M=10,
				bTc=37,
				Pw=0.60, 
				d18Oew=-10,
				eTc=20,
				Rh=0.50,				
				BMR=NULL,
				BMR_O2=NULL,
				FMR=NULL,
				TWF=NULL,
				Fdw=NULL,
				Fvw=NULL,
				d2Hew=NULL,
				d2Hleafw=NULL,
				d2Hcarb=NULL,
				d2Hprot=NULL, 
				d2Hfat=NULL,
				d2Hfw=NULL,
				d2Hdw=NULL,
				d18Oleafw=NULL,
				d18Ocarb=NULL,
				d18Oprot=NULL,
				d18Ofat=NULL,
				d18Ofw=NULL,
				d18Odw=NULL)
				
testTL2.2 = HOmodel(waste_type="urea",
				Pcarb=0.57,
				Pprot=0.33,
				Pfat=1 - 0.57 - 0.33,
				M=100,
				bTc=37,
				Pw=0.60, 
				d18Oew=-10,
				eTc=20,
				Rh=0.50,				
				BMR=NULL,
				BMR_O2=NULL,
				FMR=NULL,
				TWF=NULL,
				Fdw=NULL,
				Fvw=NULL,
				d2Hew=NULL,
				d2Hleafw=NULL,
				d2Hcarb=testTL1.2$d2Hker + 40,
				d2Hprot=testTL1.2$d2Hker, 
				d2Hfat=testTL1.2$d2Hker - 53.42365,
				d2Hfw=testTL1.2$d2Hbw,
				d2Hdw=NULL,
				d18Oleafw=NULL,
				d18Ocarb=testTL1.2$d18Oker + 8.063528,
				d18Oprot=testTL1.2$d18Oker,
				d18Ofat=testTL1.2$d18Oker - 6,
				d18Ofw=testTL1.2$d18Obw,
				d18Odw=NULL)
				
testTL3.2 = HOmodel(waste_type="urea",
				Pcarb=0.28,
				Pprot=0.57,
				Pfat=1 - 0.28 - 0.57,
				M=1000,
				bTc=37,
				Pw=0.60, 
				d18Oew=-10,
				eTc=20,
				Rh=0.50,				
				BMR=NULL,
				BMR_O2=NULL,
				FMR=NULL,
				TWF=NULL,
				Fdw=NULL,
				Fvw=NULL,
				d2Hew=NULL,
				d2Hleafw=NULL,
				d2Hcarb=testTL2.2$d2Hker + 40,
				d2Hprot=testTL2.2$d2Hker, 
				d2Hfat=testTL2.2$d2Hker - 53.42365,
				d2Hfw=testTL2.2$d2Hbw,
				d2Hdw=NULL,
				d18Oleafw=NULL,
				d18Ocarb=testTL2.2$d18Oker + 8.063528,
				d18Oprot=testTL2.2$d18Oker,
				d18Ofat=testTL2.2$d18Oker - 6,
				d18Ofw=testTL2.2$d18Obw,
				d18Odw=NULL)

testTL4.2 = HOmodel(waste_type="urea",
				Pcarb=0.00,
				Pprot=0.80,
				Pfat=1 - 0.00 - 0.80,
				M=10000,
				bTc=37,
				Pw=0.60, 
				d18Oew=-10,
				eTc=20,
				Rh=0.50,				
				BMR=NULL,
				BMR_O2=NULL,
				FMR=NULL,
				TWF=NULL,
				Fdw=NULL,
				Fvw=NULL,
				d2Hew=NULL,
				d2Hleafw=NULL,
				d2Hcarb=testTL3.2$d2Hker + 40,
				d2Hprot=testTL3.2$d2Hker, 
				d2Hfat=testTL3.2$d2Hker - 53.42365,
				d2Hfw=testTL3.2$d2Hbw,
				d2Hdw=NULL,
				d18Oleafw=NULL,
				d18Ocarb=testTL3.2$d18Oker + 8.063528,
				d18Oprot=testTL3.2$d18Oker,
				d18Ofat=testTL3.2$d18Oker - 6,
				d18Ofw=testTL3.2$d18Obw,
				d18Odw=NULL)
				
				
## plots 
library(viridis)
library(scales)
colors = viridis_pal()(20)
#show_col(viridis_pal()(20))
bw_col = colors[8]
ker_col = colors[4]
leafw_col = colors[16]
f_col = "darkgoldenrod3"
o2_col = "grey"

# Fig. 1
testpFdw$pFdw = testpFdw$Fdw / testpFdw$TWFact
pFdw = testpFdw$pFdw
d2Hbw = testpFdw$d2Hbw
d18Obw = testpFdw$d18Obw
d2Hker = testpFdw$d2Hker
d18Oker = testpFdw$d18Oker
d2Hew = testpFdw$d2Hew
d18Oew = testpFdw$d18Oew
d2Hleafw = testpFdw$d2Hleafw
d18Oleafw = testpFdw$d18Oleafw
d2Hf = testpFdw$d2Hf
d18Of = testpFdw$d18Of
d18Oo2 = testpFdw$d18Oo2 

pdf("/Users/Sarah/Dropbox/SS HO model/SS HO model ms PNAS/Figures/Fig.1.pdf", height=6, width=8, encoding="WinAnsi.enc")
par(mfrow=c(1,2), oma=c(0,1.5,0,0), mgp=c(2.5,1,0), mar=c(5,4,2,2), las=1)

plot(pFdw, d2Hbw, ylim=round(c(min(d2Hbw, d2Hker, d2Hew, d2Hleafw, d2Hf)-1, max(d2Hbw, d2Hker, d2Hew, d2Hleafw, d2Hf)+1)), xlab="Prop. of water flux that comes from drinking", ylab=expression(paste(delta^{2}, "H (\u2030)")), type="l", lty=1, lwd=2, col=bw_col, cex.axis=0.8)
text(min(pFdw)+0.01, max(d2Hbw)-18, "Body water", pos=4, col=bw_col, cex=0.8)

points(pFdw, d2Hker, type="l", lty=1, lwd=2, col=ker_col)
text(min(pFdw)+0.01, max(d2Hker)+3, "Keratin", pos=4, col=ker_col, cex=0.8)

abline(h=d2Hew, lty=2)
text(min(pFdw)+0.01, min(d2Hew)+2, "Environmental water", pos=4, cex=0.8)

abline(h=d2Hleafw, lty=2, col=leafw_col)
text(min(pFdw)+0.01, min(d2Hleafw)+2, "Food water", pos=4, col=leafw_col, cex=0.8)

abline(h=d2Hf, lty=2, col=f_col)
text(min(pFdw)+0.01, min(d2Hf)-3, "Food", pos=4, col=f_col, cex=0.8)

plot(pFdw, d18Obw, ylim=round(c(min(d18Obw, d18Oker, d18Oew, d18Oleafw, d18Of, d18Oo2)-1, max(d18Obw, d18Oker, d18Oew, d18Oleafw, d18Of, d18Oo2)+1)), xlab="Prop. of water flux that comes from drinking", ylab=expression(paste(delta^{18}, "O (\u2030)")), type="l", lty=1, lwd=2, col=bw_col, cex.axis=0.8)
text(min(pFdw)+0.01, max(d18Obw)-4, "Body water", pos=4, col=bw_col, cex=0.8)

points(pFdw, d18Oker, type="l", lty=1, lwd=2, col=ker_col)
text(min(pFdw)+0.01, max(d18Oker)-2, "Keratin", pos=4, col=ker_col, cex=0.8)

abline(h=d18Oew, lty=2)
text(min(pFdw)+0.01, min(d18Oew)+1, "Environmental water", pos=4, cex=0.8)

abline(h=d18Oleafw, lty=2, col=leafw_col)
text(min(pFdw)+0.01, min(d18Oleafw)-1, "Food water", pos=4, col=leafw_col, cex=0.8)

abline(h=d18Of, lty=2, col=f_col)
text(min(pFdw)+0.01, min(d18Of)+1, "Food", pos=4, col=f_col, cex=0.8)

abline(h=d18Oo2, lty=2, col=o2_col)
text(min(pFdw)+0.01, min(d18Oo2)+1, expression("Atmospheric O"[2]), pos=4, col=o2_col, cex=0.8)

dev.off()

# Fig. 2
Pw = testPw$Pw
d2Hbw = testPw$d2Hbw
d18Obw = testPw$d18Obw
d2Hker = testPw$d2Hker
d18Oker = testPw$d18Oker
d2Hew = testPw$d2Hew
d18Oew = testPw$d18Oew
d2Hleafw = testPw$d2Hleafw
d18Oleafw = testPw$d18Oleafw
d2Hf = testPw$d2Hf
d18Of = testPw$d18Of
d18Oo2 = testPw$d18Oo2

pdf("/Users/Sarah/Dropbox/SS HO model/SS HO model ms PNAS/Figures/Fig.2.pdf", height=6, width=8, encoding="WinAnsi.enc")
par(mfrow=c(1,2), oma=c(0,1.5,0,0), mgp=c(2.5,1,0), mar=c(5,4,2,2), las=1)

plot(Pw, d2Hbw, ylim=round(c(min(d2Hbw, d2Hker, d2Hew, d2Hleafw, d2Hf)-1, max(d2Hbw, d2Hker, d2Hew, d2Hleafw, d2Hf)+1)), xlab="Prop. of food mass that is water", ylab=expression(paste(delta^{2}, "H (\u2030)")), type="l", lty=1, lwd=2, col=bw_col, cex.axis=0.8)
text(min(Pw)+0.02, min(d2Hbw)-1, "Body water", pos=4, col=bw_col, cex=0.8)

points(Pw, d2Hker, type="l", lty=1, lwd=2, col=ker_col)
text(min(Pw)+0.02, min(d2Hker)-2, "Keratin", pos=4, col=ker_col, cex=0.8)

abline(h=d2Hew, lty=2)
text(min(Pw)+0.02, min(d2Hew)+2, "Environmental water", pos=4, cex=0.8)

abline(h=d2Hleafw, lty=2, col=leafw_col)
text(min(Pw)+0.02, min(d2Hleafw)+2, "Food water", pos=4, col=leafw_col, cex=0.8)

abline(h=d2Hf, lty=2, col=f_col)
text(min(Pw)+0.02, min(d2Hf)+2, "Food", pos=4, col=f_col, cex=0.8)

plot(Pw, d18Obw, ylim=round(c(min(d18Obw, d18Oker, d18Oew, d18Oleafw, d18Of, d18Oo2)-1, max(d18Obw, d18Oker, d18Oew, d18Oleafw, d18Of, d18Oo2)+1)), xlab="Prop. of food mass that is water", ylab=expression(paste(delta^{18}, "O (\u2030)")), type="l", lty=1, lwd=2, col=bw_col, cex.axis=0.8)
text(min(Pw)+0.02, min(d18Obw)-1, "Body water", pos=4, col=bw_col, cex=0.8)

points(Pw, d18Oker, type="l", lty=1, lwd=2, col=ker_col)
text(min(Pw)+0.02, min(d18Oker)-1, "Keratin", pos=4, col=ker_col, cex=0.8)

abline(h=d18Oew, lty=2)
text(min(Pw)+0.02, min(d18Oew)-1.5, "Environmental water", pos=4, cex=0.8)

abline(h=d18Oleafw, lty=2, col=leafw_col)
text(min(Pw)+0.02, min(d18Oleafw)-1.5, "Food water", pos=4, col=leafw_col, cex=0.8)

abline(h=d18Of, lty=2, col=f_col)
text(min(Pw)+0.02, min(d18Of)-1.5, "Food", pos=4, col=f_col, cex=0.8)

abline(h=d18Oo2, lty=2, col=o2_col)
text(min(Pw)+0.02, min(d18Oo2)-1.5, expression("Atmospheric O"[2]), pos=4, col=o2_col, cex=0.8)

dev.off()

# Fig. 3
FMR = testFMR$FMR
d2Hbw = testFMR$d2Hbw
d18Obw = testFMR$d18Obw
d2Hker = testFMR$d2Hker
d18Oker = testFMR$d18Oker
d2Hew = testFMR$d2Hew
d18Oew = testFMR$d18Oew
d2Hleafw = testFMR$d2Hleafw
d18Oleafw = testFMR$d18Oleafw
d2Hf = testFMR$d2Hf
d18Of = testFMR$d18Of
d18Oo2 = testFMR$d18Oo2

pdf("/Users/Sarah/Dropbox/SS HO model/SS HO model ms PNAS/Figures/Fig.3.pdf", height=6, width=8, encoding="WinAnsi.enc")
par(mfrow=c(1,2), oma=c(0,1.5,0,0), mgp=c(2.5,1,0), mar=c(5,4,2,2), las=1)

plot(FMR, d2Hbw, ylim=round(c(min(d2Hbw, d2Hker, d2Hew, d2Hleafw, d2Hf)-1, max(d2Hbw, d2Hker, d2Hew, d2Hleafw, d2Hf)+1)), xlab=expression(paste("Field metabolic rate (mol O"[2]," d"^{-1},")")), ylab=expression(paste(delta^{2}, "H (\u2030)")), type="l", lty=1, lwd=2, col=bw_col, cex.axis=0.8)
text(0.03, min(d2Hbw), "Body water", pos=4, col=bw_col, cex=0.8)

points(FMR, d2Hker, type="l", lty=1, lwd=2, col=ker_col)
text(0.03, min(d2Hker)-1, "Keratin", pos=4, col=ker_col, cex=0.8)

abline(h=d2Hew, lty=2)
text(0.03, min(d2Hew)+2, "Environmental water", pos=4, cex=0.8)

abline(h=d2Hleafw, lty=2, col=leafw_col)
text(0.03, min(d2Hleafw)+2, "Food water", pos=4, col=leafw_col, cex=0.8)

abline(h=d2Hf, lty=2, col=f_col)
text(0.03, min(d2Hf)+2, "Food", pos=4, col=f_col, cex=0.8)

plot(FMR, d18Obw, ylim=round(c(min(d18Obw, d18Oker, d18Oew, d18Oleafw, d18Of, d18Oo2)-1, max(d18Obw, d18Oker, d18Oew, d18Oleafw, d18Of, d18Oo2)+1)), xlab=expression(paste("Field metabolic rate (mol O"[2]," d"^{-1},")")), ylab=expression(paste(delta^{18}, "O (\u2030)")), type="l", lty=1, lwd=2, col=bw_col, cex.axis=0.8)
text(0.03, min(d18Obw), "Body water", pos=4, col=bw_col, cex=0.8)

points(FMR, d18Oker, type="l", lty=1, lwd=2, col=ker_col)
text(0.03, min(d18Oker), "Keratin", pos=4, col=ker_col, cex=0.8)

abline(h=d18Oew, lty=2)
text(0.03, min(d18Oew)+1, "Environmental water", pos=4, cex=0.8)

abline(h=d18Oleafw, lty=2, col=leafw_col)
text(0.03, min(d18Oleafw)+1, "Food water", pos=4, col=leafw_col, cex=0.8)

abline(h=d18Of, lty=2, col=f_col)
text(0.03, min(d18Of)+1, "Food", pos=4, col=f_col, cex=0.8)

abline(h=d18Oo2, lty=2, col=o2_col)
text(0.03, min(d18Oo2)+1, expression("Atmospheric O"[2]), pos=4, col=o2_col, cex=0.8)

dev.off()

# Fig. S1
Pcarb = testPs$Pcarb
Pprot = testPs$Pprot
Pfat = testPs$Pfat
d2Hbw = testPs$d2Hbw
d18Obw = testPs$d18Obw
d2Hker = testPs$d2Hker
d18Oker = testPs$d18Oker
d2Hew = testPs$d2Hew
d18Oew = testPs$d18Oew
d2Hleafw = testPs$d2Hleafw
d18Oleafw = testPs$d18Oleafw
d2Hf = testPs$d2Hf
d18Of = testPs$d18Of
d18Oo2 = testPs$d18Oo2

d2Hcarb=testPs$d2Hcarb
d2Hprot=testPs$d2Hprot
d2Hfat=testPs$d2Hfat
d18Ocarb=testPs$d18Ocarb
d18Oprot=testPs$d18Oprot
d18Ofat=testPs$d18Ofat

pdf("/Users/Sarah/Dropbox/SS HO model/SS HO model ms PNAS/Figures/Fig.S1.pdf", height=6, width=8, encoding="WinAnsi.enc")
par(mfrow=c(1,2), oma=c(0,1.5,0,0), mgp=c(2.5,1,0), mar=c(5,4,2,2), las=1)

plot(Pcarb, d2Hbw, ylim=round(c(min(d2Hbw, d2Hker, d2Hew, d2Hleafw, d2Hf, d2Hcarb,d2Hprot,d2Hfat)-1, max(d2Hbw, d2Hker, d2Hew, d2Hleafw, d2Hf,d2Hcarb,d2Hprot,d2Hfat)+1)), xlab="Proportion of carbohydrate in diet", ylab=expression(paste(delta^{2}, "H (\u2030)")), type="l", lty=1, lwd=2, col=bw_col, cex.axis=0.8)
text(min(Pcarb)+0.01, min(d2Hbw)-3, "Body water", pos=4, col=bw_col, cex=0.8)

points(Pcarb, d2Hker, type="l", lty=1, lwd=2, col=ker_col)
text(min(Pcarb)+0.01, min(d2Hker)-3, "Keratin", pos=4, col=ker_col, cex=0.8)

abline(h=d2Hew, lty=2)
text(min(Pcarb)+0.01, min(d2Hew)-4, "Environmental water", pos=4, cex=0.8)

abline(h=d2Hleafw, lty=2, col=leafw_col)
text(min(Pcarb)+0.01, min(d2Hleafw)-4, "Food water", pos=4, col=leafw_col, cex=0.8)

points(Pcarb, d2Hf, type="l", lty=2, col=f_col)
text(min(Pcarb)+0.01, min(d2Hf)-2, "Food", pos=4, col=f_col, cex=0.8)

abline(h=d2Hcarb, lty=3, lwd=0.5, col="brown1")
text(0.5, min(d2Hcarb)-3, "Carbohydrate", pos=4, col="brown1", cex=0.5)
abline(h=d2Hprot, lty=3, lwd=0.5, col="chocolate")
text(0.5, min(d2Hprot)-3, "Protein", pos=4, col="chocolate", cex=0.5)
abline(h=d2Hfat, lty=3, lwd=0.5, col="dark green")
text(0.5, min(d2Hfat)-3, "Fat", pos=4, col="dark green", cex=0.5)

plot(Pcarb, d18Obw, ylim=round(c(min(d18Obw, d18Oker, d18Oew, d18Oleafw, d18Of, d18Oo2, d18Ocarb,d18Oprot,d18Ofat)-1, max(d18Obw, d18Oker, d18Oew, d18Oleafw, d18Of, d18Oo2, d18Ocarb,d18Oprot,d18Ofat)+1)), xlab="Proportion of carbohydrate in diet", ylab=expression(paste(delta^{18}, "O (\u2030)")), type="l", lty=1, lwd=2, col=bw_col, cex.axis=0.8)
text(min(Pcarb)+0.01, min(d18Obw)-1, "Body water", pos=4, col=bw_col, cex=0.8)

points(Pcarb, d18Oker, type="l", lty=1, lwd=2, col=ker_col)
text(min(Pcarb)+0.01, min(d18Oker)-1, "Keratin", pos=4, col=ker_col, cex=0.8)

abline(h=d18Oew, lty=2)
text(min(Pcarb)+0.01, min(d18Oew)-1, "Environmental water", pos=4, cex=0.8)

abline(h=d18Oleafw, lty=2, col=leafw_col)
text(min(Pcarb)+0.01, min(d18Oleafw)-1, "Food water", pos=4, col=leafw_col, cex=0.8)

points(Pcarb, d18Of, type="l", lty=2, col=f_col)
text(min(Pcarb)+0.01, min(d18Of)+2.5, "Food", pos=4, col=f_col, cex=0.8)

abline(h=d18Oo2, lty=2, col=o2_col)
text(min(Pcarb)+0.01, min(d18Of)+5.5, expression("Atmospheric O"[2]), pos=4, col=o2_col, cex=0.8)

abline(h=d18Ocarb, lty=3, lwd=0.5, col="brown1")
text(0.5, min(d18Ocarb)-0.8, "Carbohydrate", pos=4, col="brown1", cex=0.5)
abline(h=d18Oprot, lty=3, lwd=0.5, col="chocolate")
text(0.5, min(d18Oprot)-0.8, "Protein", pos=4, col="chocolate", cex=0.5)
abline(h=d18Ofat, lty=3, lwd=0.5, col="dark green")
text(0.5, min(d18Ofat)+0.8, "Fat", pos=4, col="dark green", cex=0.5)

dev.off()

# Fig. S2
M = testM$M
d2Hbw = testM$d2Hbw
d18Obw = testM$d18Obw
d2Hker = testM$d2Hker
d18Oker = testM$d18Oker
d2Hew = testM$d2Hew
d18Oew = testM$d18Oew
d2Hleafw = testM$d2Hleafw
d18Oleafw = testM$d18Oleafw
d2Hf = testM$d2Hf
d18Of = testM$d18Of
d18Oo2 = testM$d18Oo2
		
pdf("/Users/Sarah/Dropbox/SS HO model/SS HO model ms PNAS/Figures/Fig.S2.pdf", height=6, width=8, encoding="WinAnsi.enc")
par(mfrow=c(1,2), oma=c(0,1.5,0,0), mgp=c(2.5,1,0), mar=c(5,4,2,2), las=1)

plot(M, d2Hbw, ylim=round(c(min(d2Hbw, d2Hker, d2Hew, d2Hleafw, d2Hf)-1, max(d2Hbw, d2Hker, d2Hew, d2Hleafw, d2Hf)+1)), xlab="Body mass (g)", ylab=expression(paste(delta^{2}, "H (\u2030)")), type="l", lty=1, lwd=2, col=bw_col, cex.axis=0.8)
text(min(M)+5, min(d2Hbw), "Body water", pos=4, col=bw_col, cex=0.8)

points(M, d2Hker, type="l", lty=1, lwd=2, col=ker_col)
text(min(M)+5, min(d2Hker)+3, "Keratin", pos=4, col=ker_col, cex=0.8)

abline(h=d2Hew, lty=2)
text(min(M)+5, min(d2Hew)-2, "Environmental water", pos=4, cex=0.8)

abline(h=d2Hleafw, lty=2, col=leafw_col)
text(min(M)+5, min(d2Hleafw)-2, "Food water", pos=4, col=leafw_col, cex=0.8)

abline(h=d2Hf, lty=2, col=f_col)
text(min(M)+5, min(d2Hf)-2, "Food", pos=4, col=f_col, cex=0.8)

plot(M, d18Obw, ylim=round(c(min(d18Obw, d18Oker, d18Oew, d18Oleafw, d18Of, d18Oo2)-1, max(d18Obw, d18Oker, d18Oew, d18Oleafw, d18Of, d18Oo2)+1)), xlab="Body mass (g)", ylab=expression(paste(delta^{18}, "O (\u2030)")), type="l", lty=1, lwd=2, col=bw_col, cex.axis=0.8)
text(min(M)+5, min(d18Obw)-1, "Body water", pos=4, col=bw_col, cex=0.8)

points(M, d18Oker, type="l", lty=1, lwd=2, col=ker_col)
text(min(M)+5, min(d18Oker)-1, "Keratin", pos=4, col=ker_col, cex=0.8)

abline(h=d18Oew, lty=2)
text(min(M)+5, min(d18Oew)-1, "Environmental water", pos=4, cex=0.8)

abline(h=d18Oleafw, lty=2, col=leafw_col)
text(min(M)+5, min(d18Oleafw)-1, "Food water", pos=4, col=leafw_col, cex=0.8)

abline(h=d18Of, lty=2, col=f_col)
text(min(M)+5, min(d18Of)-1, "Food", pos=4, col=f_col, cex=0.8)

abline(h=d18Oo2, lty=2, col=o2_col)
text(min(M)+5, min(d18Oo2)-1, expression("Atmospheric O"[2]), pos=4, col=o2_col, cex=0.8)

dev.off()

# Fig. S3 
pdf("/Users/Sarah/Dropbox/SS HO model/SS HO model ms PNAS/Figures/Fig.S3.pdf", height=6, width=8, encoding="WinAnsi.enc")

raster <- Hgs

par("fg"=NA)
plot(raster, col=terrain.colors(100), xlim=c(xmin(raster),xmax(raster)), ylim=c(ymin(raster),ymax(raster)), axes=FALSE, legend=FALSE)

par(new=TRUE,"fg"="black")
plot(raster, col=terrain.colors(100), legend.only=TRUE, legend.width=1, legend.shrink=0.50, legend.args=list(text=expression(paste("Precip. ", delta^{2}, "H (\u2030)")), side=3, line=5, cex=1.25)) 

rect(xmin(raster),ymin(raster),xmax(raster),ymax(raster),xpd = TRUE) 
xticks <- (xmin(raster):xmax(raster))[(xmin(raster):xmax(raster)) %% 30 == 0]
segments(xticks,ymin(raster),xticks,ymin(raster)-5, xpd = TRUE)
text(xticks,ymin(raster),xticks,xpd=TRUE,pos=1, cex=1.25) 
yticks <- (ymin(raster):ymax(raster))[(ymin(raster):ymax(raster)) %% 30 == 0]
segments(xmin(raster),yticks,xmin(raster)-5,yticks, xpd = TRUE)
text(xmin(raster),yticks,yticks,xpd=TRUE,pos=2, cex=1.25) 
mtext(side=1, text="Longitude", line=-0.2, cex=1.25)
mtext(side=2, text="Latitude", line=2.5, cex=1.25)  

world(interior=FALSE, add=TRUE, lwd=0.5)

points(NA_pointdata$Lon, NA_pointdata$Lat, pch=16, cex=0.5)
points(EU_pointdata$Lon, EU_pointdata$Lat, pch=16, col="blue", cex=0.5)

dev.off() 

# Fig. 4
pdf("/Users/Sarah/Dropbox/SS HO model/SS HO model ms Oecologia/Figures/Fig.4.pdf", height=6, width=8, encoding="WinAnsi.enc")
par(mfrow=c(1,2), oma=c(0,1.5,0,0), mgp=c(2.5,1,0), mar=c(5,4,2,2), las=1)

plot(testNA.1$d2Hew, testNA.1$d2Hker, xlim=c(min(testNA.1$d2Hew, testEU.1$d2Hew)-1, max(testNA.1$d2Hew, testEU.1$d2Hew)+1), ylim=c(min(testNA.1$d2Hker, testEU.1$d2Hker)-1, max(testNA.1$d2Hker, testEU.1$d2Hker)+1), xlab=expression(paste("Env. water ", delta^{2}, "H (\u2030)")), ylab=expression(paste("Keratin ", delta^{2}, "H (\u2030)")), pch=1, cex.axis=0.75)	#cex=0.8
lines(testNA.1$d2Hew, fitted(lm(testNA.1$d2Hker ~ testNA.1$d2Hew)), lwd=2)
lm_mod = lm(testNA.1$d2Hker ~ testNA.1$d2Hew) 
lm_coef = round(coef(lm_mod),2)
lm_r2 = round(summary(lm_mod)$adj.r.squared,4)
text(-125,-60, "y = 1.03x - 35.93", pos=4) #cex=0.8
text(-125,-65, expression(paste(r^{2}, " = 0.9986")), pos=4) #cex=0.8
# Empirical relationship - 
text(-125,-70, expression(italic(paste("y = 1.07x + ", beta[1]))), pos=4, font=3) #cex=0.8
text(-125,-75, expression(italic(paste(r^{2}, " = 0.86"))), pos=4, font=3) #cex=0.8

points(testEU.1$d2Hew, testEU.1$d2Hker, col="blue", pch=1)
lines(testEU.1$d2Hew, fitted(lm(testEU.1$d2Hker ~ testEU.1$d2Hew)), col="blue", lwd=2)
lm_mod = lm(testEU.1$d2Hker ~ testEU.1$d2Hew)
lm_coef = round(coef(lm_mod),2)
lm_r2 = round(summary(lm_mod)$adj.r.squared,4)
text(-80,-150, "y = 1.23x - 26.22", pos=4, col="blue")
text(-80,-155, expression(paste(r^{2}, " = 0.9058")), pos=4, col="blue")
# Empirical relationship - 
text(-80,-160, "y = 1.28x - 7.20", pos=4, col="blue", font=3)
text(-80,-165, expression(italic(paste(r^{2}, " = 0.65"))), pos=4, col="blue", font=3)

plot(testNA.1$d18Oew, testNA.1$d18Oker, xlim=c(min(testNA.1$d18Oew, testEU.1$d18Oew)-1, max(testNA.1$d18Oew, testEU.1$d18Oew)+1), ylim=c(min(testNA.1$d18Oker, testEU.1$d18Oker)-1, max(testNA.1$d18Oker, testEU.1$d18Oker)+1), xlab=expression(paste("Env. water ", delta^{18}, "O (\u2030)")), ylab=expression(paste("Keratin ", delta^{18}, "O (\u2030)")), pch=1, cex.axis=0.75)
lines(testNA.1$d18Oew, fitted(lm(testNA.1$d18Oker ~ testNA.1$d18Oew)), lwd=2)
lm_mod = lm(testNA.1$d18Oker ~ testNA.1$d18Oew)
lm_coef = round(coef(lm_mod),2)
lm_r2 = round(summary(lm_mod)$adj.r.squared,4)
text(-17.0,18.2, "y = 0.82x + 18.64", pos=4)
text(-17.0,17.5, expression(paste(r^{2}, " = 0.9871")), pos=4)

points(testEU.1$d18Oew, testEU.1$d18Oker, col="blue", pch=1)
lines(testEU.1$d18Oew, fitted(lm(testEU.1$d18Oker ~ testEU.1$d18Oew)), col="blue", lwd=2)
lm_mod = lm(testEU.1$d18Oker ~ testEU.1$d18Oew)
lm_coef = round(coef(lm_mod),2)
lm_r2 = round(summary(lm_mod)$adj.r.squared,4)
text(-11.0,5.0, "y = 1.24x + 21.46", pos=4, col="blue")
text(-11.0,4.3, expression(paste(r^{2}, " = 0.5959")), pos=4, col="blue")

dev.off()

# Fig. S4 
pdf("/Users/Sarah/Dropbox/SS HO model/SS HO model ms Oecologia/Figures/Fig.S4.pdf", height=6, width=8, encoding="WinAnsi.enc")
par(mfrow=c(1,2), oma=c(0,1.5,0,0), mgp=c(2.5,1,0), mar=c(5,4,2,2), las=1)

plot(testNA.2$d2Hew, testNA.2$d2Hker, xlim=c(min(testNA.2$d2Hew, testEU.2$d2Hew)-1, max(testNA.2$d2Hew, testEU.2$d2Hew)+1), ylim=c(min(testNA.2$d2Hker, testEU.2$d2Hker)-1, max(testNA.2$d2Hker, testEU.2$d2Hker)+1), xlab=expression(paste("Env. water ", delta^{2}, "H (\u2030)")), ylab=expression(paste("Keratin ", delta^{2}, "H (\u2030)")), cex.axis=0.75, pch=1)	
lines(testNA.2$d2Hew, fitted(lm(testNA.2$d2Hker ~ testNA.2$d2Hew)), lwd=2)
lm_mod = lm(testNA.2$d2Hker ~ testNA.2$d2Hew)
lm_coef = round(coef(lm_mod),2)
lm_r2 = round(summary(lm_mod)$adj.r.squared,4) 
text(-125,-45, "y = 1.02x - 22.63", pos=4)
text(-125,-50, expression(paste(r^{2}, " = 0.9999")), pos=4)

points(testEU.2$d2Hew, testEU.2$d2Hker, col="blue", pch=1)
lines(testEU.2$d2Hew, fitted(lm(testEU.2$d2Hker ~ testEU.2$d2Hew)), col="blue", lwd=2)
lm_mod = lm(testEU.2$d2Hker ~ testEU.2$d2Hew)
lm_coef = round(coef(lm_mod),2)
lm_r2 = round(summary(lm_mod)$adj.r.squared,4)
text(-80,-143, "y = 1.02x - 21.14", pos=4, col="blue")
text(-80,-148, expression(paste(r^{2}, " = 0.9962")), pos=4, col="blue")

plot(testNA.2$d18Oew, testNA.2$d18Oker, xlim=c(min(testNA.2$d18Oew, testEU.2$d18Oew)-1, max(testNA.2$d18Oew, testEU.2$d18Oew)+1), ylim=c(min(testNA.2$d18Oker, testEU.2$d18Oker)-1, max(testNA.2$d18Oker, testEU.2$d18Oker)+1), xlab=expression(paste(delta^{18}, "O (\u2030) Environmental water")), ylab=expression(paste(delta^{18}, "O (\u2030) Keratin")), cex.axis=0.75, pch=1)	
lines(testNA.2$d18Oew, fitted(lm(testNA.2$d18Oker ~ testNA.2$d18Oew)), lwd=2)
lm_mod = lm(testNA.2$d18Oker ~ testNA.2$d18Oew)
lm_coef = round(coef(lm_mod),2)
lm_r2 = round(summary(lm_mod)$adj.r.squared,6) 
text(-17,20.2, "y = 0.78x + 22.36", pos=4)
text(-17,19.7, expression(paste(r^{2}, " = 0.9999")), pos=4)

points(testEU.2$d18Oew, testEU.2$d18Oker, col="blue", pch=1)
lines(testEU.2$d18Oew, fitted(lm(testEU.2$d18Oker ~ testEU.2$d18Oew)), col="blue", lwd=2)
lm_mod = lm(testEU.2$d18Oker ~ testEU.2$d18Oew)
lm_coef = round(coef(lm_mod),2)
lm_r2 = round(summary(lm_mod)$adj.r.squared,4) 
text(-11.5,9.4, "y = 0.79x + 22.47", pos=4, col="blue")
text(-11.5,8.9, expression(paste(r^{2}, " = 0.9982")), pos=4, col="blue")

dev.off()

# Fig. 5
TL = c(1,2,3,4)

d2Hbw = c(testTL1.2$d2Hbw, testTL2.2$d2Hbw, testTL3.2$d2Hbw, testTL4.2$d2Hbw)
d18Obw = c(testTL1.2$d18Obw, testTL2.2$d18Obw, testTL3.2$d18Obw, testTL4.2$d18Obw)

d2Hker = c(testTL1.2$d2Hker, testTL2.2$d2Hker, testTL3.2$d2Hker, testTL4.2$d2Hker)
d18Oker = c(testTL1.2$d18Oker, testTL2.2$d18Oker, testTL3.2$d18Oker, testTL4.2$d18Oker)

d2Hdw = c(testTL1.2$d2Hdw, testTL2.2$d2Hdw, testTL3.2$d2Hdw, testTL4.2$d2Hdw)
d18Odw = c(testTL1.2$d18Odw, testTL2.2$d18Odw, testTL3.2$d18Odw, testTL4.2$d18Odw)

d2Hfw = c(testTL1.2$d2Hfw, testTL2.2$d2Hfw, testTL3.2$d2Hfw, testTL4.2$d2Hfw)
d18Ofw = c(testTL1.2$d18Ofw, testTL2.2$d18Ofw, testTL3.2$d18Ofw, testTL4.2$d18Ofw)	

d2Hf = c(testTL1.2$d2Hf, testTL2.2$d2Hf, testTL3.2$d2Hf, testTL4.2$d2Hf)
d18Of = c(testTL1.2$d18Of, testTL2.2$d18Of, testTL3.2$d18Of, testTL4.2$d18Of)	

d18Oo2 = c(testTL1.2$d18Oo2, testTL2.2$d18Oo2, testTL3.2$d18Oo2, testTL4.2$d18Oo2)

pdf("/Users/Sarah/Dropbox/SS HO model/SS HO model ms PNAS/Figures/Fig.5.pdf", height=6, width=8, encoding="WinAnsi.enc")
par(mfrow=c(1,2), oma=c(0,1.5,0,0), mgp=c(2.5,1,0), mar=c(5,4,2,2), las=1)

plot(TL, d2Hbw, xlim=c(0.5,4.5), ylim=c(min(d2Hbw,d2Hker,d2Hdw,d2Hfw,d2Hf)-1, max(d2Hbw,d2Hker,d2Hdw,d2Hfw,d2Hf)+1), xaxt="n", xlab="", ylab=expression(paste(delta^{2}, "H (\u2030)")), pch=16, col=bw_col, cex=1.5, cex.axis=0.8, type="b")
text(0.2, min(d2Hbw)-3, "Body water", pos=4, col=bw_col, cex=0.8)
axis(1, at=1:4, labels=c("TL 1","TL 2","TL 3","TL 4"), cex.axis=0.8)

points(TL, d2Hker, pch=16, col=ker_col, cex=1.5, type="b")
text(0.2, min(d2Hker)-3, "Keratin", pos=4, col=ker_col, cex=0.8)

points(TL, d2Hdw, pch=18, type="b")
text(0.2, min(d2Hdw)-3, "Env. water", pos=4, cex=0.8)

points(TL, d2Hfw, pch=18, col=leafw_col, type="b")
text(0.2, max(d2Hfw)+3, "Food water", pos=4, col=leafw_col, cex=0.8)

points(TL, d2Hf, pch=18, col=f_col, type="b")
text(0.2, min(d2Hf)+3, "Food", pos=4, col=f_col, cex=0.8)

plot(TL, d18Obw, xlim=c(0.5,4.5), ylim=c(min(d18Obw,d18Oker,d18Odw,d18Ofw,d18Of,d18Oo2)-1, max(d18Obw,d18Oker,d18Odw,d18Ofw,d18Of,d18Oo2)+1), xaxt="n", xlab="", ylab=expression(paste(delta^{18}, "O (\u2030)")), pch=16, col=bw_col, cex=1.5, cex.axis=0.8, type="b")
text(0.2, max(d18Obw)-5, "Body water", pos=4, col=bw_col, cex=0.8)
axis(1, at=1:4, labels=c("TL 1","TL 2","TL 3","TL 4"), cex.axis=0.8)

points(TL, d18Oker, pch=16, col=ker_col, cex=1.5, type="b")
text(0.2, max(d18Oker)-3, "Keratin", pos=4, col=ker_col, cex=0.8)

points(TL, d18Odw, pch=18, type="b")
text(0.2, min(d18Odw)-1, "Env. water", pos=4, cex=0.8)

points(TL, d18Ofw, pch=18, col=leafw_col, type="b")
text(0.2, max(d18Ofw)-8, "Food water", pos=4, col=leafw_col, cex=0.8)

points(TL, d18Of, pch=18, col=f_col, type="b")
text(0.2, max(d18Of)+1, "Food", pos=4, col=f_col, cex=0.8)

points(TL, d18Oo2, pch=18, col=o2_col, type="b")
text(0.2, max(d18Oo2)-1, expression("Atm. O"[2]), col=o2_col, pos=4, cex=0.8)

dev.off()

# Fig. S5
TL = c(1,2,3,4)

d2Hbw = c(testTL1.1$d2Hbw, testTL2.1$d2Hbw, testTL3.1$d2Hbw, testTL4.1$d2Hbw)
d18Obw = c(testTL1.1$d18Obw, testTL2.1$d18Obw, testTL3.1$d18Obw, testTL4.1$d18Obw)

d2Hker = c(testTL1.1$d2Hker, testTL2.1$d2Hker, testTL3.1$d2Hker, testTL4.1$d2Hker)
d18Oker = c(testTL1.1$d18Oker, testTL2.1$d18Oker, testTL3.1$d18Oker, testTL4.1$d18Oker)

d2Hdw = c(testTL1.1$d2Hdw, testTL2.1$d2Hdw, testTL3.1$d2Hdw, testTL4.1$d2Hdw)
d18Odw = c(testTL1.1$d18Odw, testTL2.1$d18Odw, testTL3.1$d18Odw, testTL4.1$d18Odw)

d2Hfw = c(testTL1.1$d2Hfw, testTL2.1$d2Hfw, testTL3.1$d2Hfw, testTL4.1$d2Hfw)
d18Ofw = c(testTL1.1$d18Ofw, testTL2.1$d18Ofw, testTL3.1$d18Ofw, testTL4.1$d18Ofw)	

d2Hf = c(testTL1.1$d2Hf, testTL2.1$d2Hf, testTL3.1$d2Hf, testTL4.1$d2Hf)
d18Of = c(testTL1.1$d18Of, testTL2.1$d18Of, testTL3.1$d18Of, testTL4.1$d18Of)	

d18Oo2 = c(testTL1.1$d18Oo2, testTL2.1$d18Oo2, testTL3.1$d18Oo2, testTL4.1$d18Oo2)

pdf("/Users/Sarah/Dropbox/SS HO model/SS HO model ms PNAS/Figures/Fig.S5.pdf", height=6, width=8, encoding="WinAnsi.enc")
par(mfrow=c(1,2), oma=c(0,1.5,0,0), mgp=c(2.5,1,0), mar=c(5,4,2,2), las=1)

plot(TL, d2Hbw, xlim=c(0.5,4.5), ylim=c(min(d2Hbw,d2Hker,d2Hdw,d2Hfw,d2Hf)-1, max(d2Hbw,d2Hker,d2Hdw,d2Hfw,d2Hf)+1), xaxt="n", xlab="", ylab=expression(paste(delta^{2}, "H (\u2030)")), pch=16, col=bw_col, cex=1.5, cex.axis=0.8, type="b")
text(0.2, min(d2Hbw)-3, "Body water", pos=4, col=bw_col, cex=0.8)
axis(1, at=1:4, labels=c("TL 1","TL 2","TL 3","TL 4"), cex.axis=0.8)

points(TL, d2Hker, pch=16, col=ker_col, cex=1.5, type="b")
text(0.2, min(d2Hker)-3, "Keratin", pos=4, col=ker_col, cex=0.8)

points(TL, d2Hdw, pch=18, type="b")
text(0.2, min(d2Hdw)-3, "Env. water", pos=4, cex=0.8)

points(TL, d2Hfw, pch=18, col=leafw_col, type="b")
text(0.2, max(d2Hfw)+3, "Food water", pos=4, col=leafw_col, cex=0.8)

points(TL, d2Hf, pch=18, col=f_col, type="b")
text(0.2, min(d2Hf)+3, "Food", pos=4, col=f_col, cex=0.8)

plot(TL, d18Obw, xlim=c(0.5,4.5), ylim=c(min(d18Obw,d18Oker,d18Odw,d18Ofw,d18Of,d18Oo2)-1, max(d18Obw,d18Oker,d18Odw,d18Ofw,d18Of,d18Oo2)+1), xaxt="n", xlab="", ylab=expression(paste(delta^{18}, "O (\u2030)")), pch=16, col=bw_col, cex=1.5, cex.axis=0.8, type="b")
text(0.2, max(d18Obw)-5, "Body water", pos=4, col=bw_col, cex=0.8)
axis(1, at=1:4, labels=c("TL 1","TL 2","TL 3","TL 4"), cex.axis=0.8)

points(TL, d18Oker, pch=16, col=ker_col, cex=1.5, type="b")
text(0.2, max(d18Oker)-3, "Keratin", pos=4, col=ker_col, cex=0.8)

points(TL, d18Odw, pch=18, type="b")
text(0.2, min(d18Odw)-1, "Env. water", pos=4, cex=0.8)

points(TL, d18Ofw, pch=18, col=leafw_col, type="b")
text(0.2, max(d18Ofw)-8, "Food water", pos=4, col=leafw_col, cex=0.8)

points(TL, d18Of, pch=18, col=f_col, type="b")
text(0.2, max(d18Of)+1, "Food", pos=4, col=f_col, cex=0.8)

points(TL, d18Oo2, pch=18, col=o2_col, type="b")
text(0.2, max(d18Oo2)-1, expression("Atm. O"[2]), col=o2_col, pos=4, cex=0.8)

dev.off()



				