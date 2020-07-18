library(raster)

## Number of days with high temprature ----
YearAvg <- function(years) {
  r <- raster(sprintf("./NEX-GDDP/historical/Number of days with temperature gt 35/tasmax_day_BCSD_rcp45_r1i1p1_%d.tif", years[1]))
  for (y in years[-1]) {
    r <- r + raster(sprintf("./NEX-GDDP/historical/Number of days with temperature gt 35/tasmax_day_BCSD_rcp45_r1i1p1_%d.tif", y))
  }
  r <- r / length(years)
  return(r)
}
BaseHistory <- YearAvg(1981:2005)
writeRaster(BaseHistory, "./DaysOfHeatWave/BaseHistory.tif", options=c("COMPRESS=LZW"))


YearAvg <- function(years) {
  r <- raster(sprintf("./NEX-GDDP/R4.5/Number of days with temperature gt 35/tasmax_day_BCSD_rcp45_r1i1p1_%d.tif", years[1]))
  for (y in years[-1]) {
    r <- r + raster(sprintf("./NEX-GDDP/R4.5/Number of days with temperature gt 35/tasmax_day_BCSD_rcp45_r1i1p1_%d.tif", y))
  }
  r <- r / length(years)
  return(r)
}
Future_1 <- YearAvg(2021:2040)
Future_2 <- YearAvg(2041:2060)
Future_3 <- YearAvg(2061:2080)
Future_4 <- YearAvg(2081:2100)

writeRaster(Future_1, "./DaysOfHeatWave/Future_1.tif", options=c("COMPRESS=LZW"))
writeRaster(Future_2, "./DaysOfHeatWave/Future_2.tif", options=c("COMPRESS=LZW"))
writeRaster(Future_3, "./DaysOfHeatWaveÊý/Future_3.tif", options=c("COMPRESS=LZW"))
writeRaster(Future_4, "./DaysOfHeatWave/Future_4.tif", options=c("COMPRESS=LZW"))

writeRaster(Future_1 - BaseHistory, "./DaysOfHeatWave/Diff_1_0.tif", options=c("COMPRESS=LZW"))
writeRaster(Future_2 - Future_1, "./DaysOfHeatWave/Diff_2_1.tif", options=c("COMPRESS=LZW"))
writeRaster(Future_3 - Future_2, "./DaysOfHeatWave/Diff_3_2.tif", options=c("COMPRESS=LZW"))
writeRaster(Future_4 - Future_3, "./DaysOfHeatWave/Diff_4_3.tif", options=c("COMPRESS=LZW"))
