library(sp)
library(rgdal)
library(raster)


## global risk map ----
days <- raster("./DaysOfHeatWave/BaseHistory.tif")
pop <- (raster("./IIASA GGI - A2POPgrid/pa1990.tif") + raster("./IIASA GGI - A2POPgrid/pa2000.tif")) / 2
risk <- days * pop
writeRaster(risk, "./RiskOfHeatWave/risk_BaseHistory.tif", options=c("COMPRESS=LZW", "TFW=NO"))

days <- raster("./DaysOfHeatWave/Future_1.tif")
pop <- raster("./IIASA GGI - A2POPgrid/pa2030.tif")
risk <- days * pop
writeRaster(risk, "./RiskOfHeatWave/risk_Future_1.tif", options=c("COMPRESS=LZW", "TFW=NO"))

days <- raster("./DaysOfHeatWave/Future_2.tif")
pop <- raster("./IIASA GGI - A2POPgrid/pa2050.tif")
risk <- days * pop
writeRaster(risk, "./RiskOfHeatWave/risk_Future_2.tif", options=c("COMPRESS=LZW", "TFW=NO"))

days <- raster("./DaysOfHeatWave/Future_3.tif")
pop <- raster("./IIASA GGI - A2POPgrid/pa2070.tif")
risk <- days * pop
writeRaster(risk, "./RiskOfHeatWave/risk_Future_3.tif", options=c("COMPRESS=LZW", "TFW=NO"))

days <- raster("./DaysOfHeatWave/Future_4.tif")
pop <- raster("./IIASA GGI - A2POPgrid/pa2090.tif")
risk <- days * pop
writeRaster(risk, "./RiskOfHeatWave/risk_Future_4.tif", options=c("COMPRESS=LZW", "TFW=NO"))

writeRaster(raster("./RiskOfHeatWave/risk_Future_1.tif") - raster("./RiskOfHeatWave/risk_BaseHistory.tif"),
            "./RiskOfHeatWave/Diff_risk_Future_1_0.tif")
writeRaster(raster("./RiskOfHeatWave/risk_Future_2.tif") - raster("./RiskOfHeatWave/risk_Future_1.tif"),
            "./RiskOfHeatWave/Diff_risk_Future_2_1.tif")
writeRaster(raster("./RiskOfHeatWave/risk_Future_3.tif") - raster("./RiskOfHeatWave/risk_Future_2.tif"),
            "./RiskOfHeatWave/Diff_risk_Future_3_2.tif")
writeRaster(raster("./RiskOfHeatWave/risk_Future_4.tif") - raster("./RiskOfHeatWave/risk_Future_3.tif"),
            "./RiskOfHeatWave/Diff_risk_Future_4_3.tif")


## city level risk ----
CityRisk <- function(city, globalrisk) {
  SingleToMultipart <- function(city) {
    if(length(city) == 1) return(city)
    
    m <- rgeos::gUnaryUnion(city)
    m <- SpatialPolygonsDataFrame(m, data.frame(Id = 0), match.ID = FALSE)
    return(m)
  }
  
  if(length(city) > 1) city <- SingleToMultipart(city)
  
  citysub <- crop(globalrisk, city, snap='out')
  citysubP <- rasterToPolygons(citysub)
  names(citysubP) <- "Value"
  
  intersected <- rgeos::gIntersects(city, citysubP, byid = TRUE)
  citysubP <- citysubP[intersected[,1],]
  intersection <- rgeos::gIntersection(city, citysubP, byid = TRUE)
  intersection <- SpatialPolygonsDataFrame(intersection, data.frame(id=1:length(intersection), citysubP@data), match.ID = FALSE)
  intersection$area <- rgeos::gArea(intersection, byid = TRUE)
  intersection$ratio <- intersection$area / max(0.0625, intersection$area)
  intersection$rValue <- intersection$Value * intersection$ratio
  
  return(list(IntersectRas = citysub, IntersectVec = intersection, TotalRisk = sum(intersection$rValue, na.rm = T)))
}

cityfiles <- list.files("./Cities", ".shp$", full.name = T)
riskfiles <- sprintf("./RiskOfHeatWave/risk_Future_%d.tif", 1:4)

cityTotRisks <- c()
for (f in cityfiles) {
  bname <- gsub(".shp","",basename(f))
  city <- readOGR(dirname(f), bname)
  
  bfolder <- paste0("./RiskOfHeatWave/", bname)
  if(!dir.exists(bfolder)) dir.create(bfolder)
  
  citytot <- c()
  bFirst <- TRUE
  for (rf in riskfiles) {
    risk <- raster(rf)
    cr <- CityRisk(city, risk)
    writeRaster(cr$IntersectRas, file.path(bfolder, basename(rf)), options=c("COMPRESS=LZW","TFW=NO"))
    write.csv(cr$IntersectVec@data, file.path(bfolder, gsub(".tif", ".csv", basename(rf))), row.names = FALSE)
    if(bFirst) {
      cr$IntersectVec@data <- cr$IntersectVec@data[,"id",drop=F]
      writeOGR(cr$IntersectVec, bfolder, "risk", driver = "ESRI Shapefile")
      bFirst <- FALSE
    }
    citytot <- c(citytot, cr$TotalRisk)
  }
  cityTotRisks <- cbind(cityTotRisks, citytot)
}

row.names(cityTotRisks) <- 1:4
colnames(cityTotRisks) <- gsub(".shp", "", basename(cityfiles))
openxlsx::write.xlsx(cityTotRisks, "./RiskOfHeatWave-City.xlsx", row.names = T)
