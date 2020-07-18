library(raster)
library(rgdal)
library(ggplot2)
library(gridExtra)

rgdal::setCPLConfigOption("GDAL_FILENAME_IS_UTF8", "YES")

## global map of risk ----
cityPath <- "./Cities"
rasPath <- "./RiskOfHeatWave"

cities <- list.files(cityPath, pattern = ".shp$")
citytitles <- openxlsx::read.xlsx("./../fig/Cities.xlsx")$Title

PlotRaster <- function(r, vec, rng=NA, nbreak=5, title=NULL) {
  ras.df <- data.frame(rasterToPoints(r))
  colnames(ras.df) <- c("x","y","v")
  
  if(length(rng) <= 1)
    rng <- c(minValue(r),maxValue(r))
  br <- seq(rng[1], rng[2], len = nbreak)
  
  p  <- ggplot() + 
    coord_equal() + 
    geom_tile(data=ras.df, mapping=aes(x, y, fill=v)) +
    scale_fill_gradient2(low="blue", mid="yellow",high="red",
                         limits=rng, 
                         midpoint=mean(rng),
                         breaks=br,
                         labels=sprintf("%.0f", br),
                         guide=guide_colorbar(title=NULL)
    ) + 
    geom_polygon(data=vec, mapping=aes(x=long, y=lat, group=group), color="gray", fill=NA) +
    labs(x=NULL, y=NULL, title=title) + 
    theme_bw() +
    scale_x_continuous(expand=c(0,0)) + 
    scale_y_continuous(expand=c(0,0))
  
  return(p)
}

sapply(1:length(cities), function(i) {
  city <- cities[i]
  city <- substring(city, 1, nchar(city)-4)
  
  rasFiles <- list.files(file.path(rasPath, city), pattern = "tif$", full.names = T)
  ras <- sapply(rasFiles, function(f) raster(f))
  
  rng <- sapply(ras, function(r) c(minValue(r), maxValue(r)))
  rng <- c(min(rng[1,]), max(rng[2,]))
  
  poly <- rgdal::readOGR(cityPath, city)
  if(length(poly) > 1) {
    p <- rgeos::gUnaryUnion(poly)
    poly <- SpatialPolygonsDataFrame(p, data.frame(id=1))
  }
  poly@data$id <- rownames(poly@data)
  poly.point = fortify(poly, region = "id")
  poly.df <- plyr::join(poly.point, poly@data, by = "id")
  
  lp <- lapply(ras, function(r) {
    stitle <- names(r)
    n <- as.integer(substring(stitle, nchar(stitle)))
    stitle <- sprintf("%s (%s)", citytitles[i], c("2020-2040","2040-2060","2060-2080","2080-2100")[n])
    PlotRaster(r, poly.df, rng, title = stitle)
  })
  
  ggsave(sprintf("./../fig/risk/%s.png", city), grid.arrange(grobs = lp, ncol=4), width = 20, height = 5, type="cairo-png")
  invisible()
})


## Bubble plot ----
pop <- openxlsx::read.xlsx("CityPopulation.xlsx")
pop <- subset(pop, Year %in% c(2030,2050,2070,2090))

bOld <- TRUE

if(bOld) {
  oldratio <- openxlsx::read.xlsx("RiskOfHeatWave-City-forOlder.xlsx", "ratio of older")
  pop[,-1] <- pop[,-1] * oldratio[,-1]
  temprisk <- openxlsx::read.xlsx("RiskOfHeatWave-City-forOlder.xlsx", "risk for older")
} else {
  temprisk <- openxlsx::read.xlsx("RiskOfHeatWave-City.xlsx", "risk")
}

lpop <- reshape2::melt(pop, id="Year", variable.name="city", value.name="pop")
ltemprisk <- reshape2::melt(temprisk, id="Stage", variable.name="city", value.name="temprisk")
ltemprisk$pop <- lpop$pop

continent <- openxlsx::read.xlsx("./../fig/Continent.xlsx")
ltemprisk <- merge(ltemprisk, continent)

if (bOld) {
  legendtitle <- "Elderly population"
  legendbreaks <- c(2,4,6,8,10)*1e6
  legendlabels <- c("2,000,000","4,000,000","6,000,000","8,000,000","10,000,000")
} else {
  legendtitle <- "Population"
  legendbreaks <- c(0.5,1,2,3,4)*1e7
  legendlabels <- c("5,000,000","10,000,000","20,000,000","30,000,000","40,000,000")
}

p <- ggplot(data = ltemprisk, aes(x = Stage, y = log(temprisk))) + 
  geom_point(aes(size = pop, shape = shape, 
                 col = color, fill = fill), 
             alpha = 0.5, stroke = 1.2) +
  geom_smooth(aes(group = group), col = rep(c("#e0201b","#0081c8"), each=4)) + 
  labs(x = "Year", y = "Log(Risk)",
       size = "Population",
       col = "Continent") +
  # scale_colour_manual(values = cols) +
  scale_color_identity() + 
  scale_shape_identity() + 
  scale_fill_identity() + 
  scale_size_continuous(name = legendtitle, 
                        breaks  = legendbreaks,
                        labels = legendlabels) + 
  guides(size = guide_legend(ncol = 1), # size = FALSE,
         colour = guide_legend(ncol = 2, override.aes = list(size=4)),
         fill = guide_legend(ncol = 2, override.aes = list(size=4))) +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=13))

if(bOld) {
  ggsave("./../fig/RiskForOlder_Log.png", p, width = 6.25*1.5, height = 6*1.5, type="cairo-png")
} else {
  ggsave("./../fig/Risk_Log.png", p, width = 6*1.5, height = 6*1.5, type="cairo-png")
}

continent$x <- rep(c(1,3.5), each=15)[1:29]
continent$y <- 31-rep(1:15, 2)[1:29]
lg <- ggplot(data = continent) +
  geom_point(aes(x = x, y = y, 
                 size = 4, shape = shape,
                 col = color, fill = fill), stroke = 1) +
  geom_text(aes(x=x+0.25, y=y-0.1, label=label, size=4), hjust = 0, vjust = 0) + 
  scale_color_identity() + 
  scale_shape_identity() + 
  scale_fill_identity() +
  scale_size_continuous(name = legendtitle, 
                        breaks  = legendbreaks,
                        labels = legendlabels) + 
  guides(size = FALSE) +
  theme_void() + xlim(1,6) + 
  labs(title = "City") + 
  theme(plot.title = element_text(size = 14, hjust = 0.025))

ggsave("./../fig/Legend_Risk.png", lg, width = 3.5*1.5, height = 3.5*1.5, type="cairo-png")

