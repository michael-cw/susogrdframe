#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

## Global Variables with no visible binding, i.e data.table, dplyr etc.
utils::globalVariables(c("CID", "X", "Y", "GRIDID", "V1", "id", "GRD_NEWID"))

## CSS UI styles for style attribute
action_btnred<-function(){return(c("color: #FFFFFF; background-color: #7f0000; border-color: #7f0000;"))}
styleDwlButton<-function(){return(c("color: #FFFFFF;  width: 180px;background-color: #1976D2;
                  border-color: #1976D2;
                  margin:0 20% 0 20%;"))}
smTabDir<-function(){return(list(dom="t", pagelength=500, scrollY="250px", scrollcollapse=TRUE, paging=FALSE))}
action_btn_close <-function(){return(c("color: #FFFFFF; background-color: #0d47a1; border-color: #0d47a1"))}
styleActButton<-function(){return(c("color: #FFFFFF; background-color: #0d47a1;border-color: #0d47a1; margin: 0"))}
styleActButtonActivate<-function(){return(c("color: #FFFFFF; background-color: #009FDA; border-color: #009FDA; margin:0% 20% 0% 0%;"))}
invisibleButton<-function(){return(c("color: #FFFFFF; background-color: #FFFFFF;border-color: #FFFFFF; margin:0% 0% 0% 0%;height:2px;"))}

## BASE CRS
crs_epsg<-3035
crs_wkt<-c(
  '
  PROJCS["ETRS89 / ETRS-LAEA",
    GEOGCS["ETRS89",
        DATUM["European_Terrestrial_Reference_System_1989",
            SPHEROID["GRS 1980",6378137,298.257222101,
                AUTHORITY["EPSG","7019"]],
            TOWGS84[0,0,0,0,0,0,0],
            AUTHORITY["EPSG","6258"]],
        PRIMEM["Greenwich",0,
            AUTHORITY["EPSG","8901"]],
        UNIT["degree",0.01745329251994328,
            AUTHORITY["EPSG","9122"]],
        AUTHORITY["EPSG","4258"]],
    UNIT["metre",1,
        AUTHORITY["EPSG","9001"]],
    PROJECTION["Lambert_Azimuthal_Equal_Area"],
    PARAMETER["latitude_of_center",52],
    PARAMETER["longitude_of_center",10],
    PARAMETER["false_easting",4321000],
    PARAMETER["false_northing",3210000],
    AUTHORITY["EPSG","3035"],
    AXIS["X",EAST],
    AXIS["Y",NORTH]]
  '
)


############################
## other styles
############################
#################################
##  Table formats & styles
## 1. General
smTab<-list(dom="t")

##  2. Info table (no selection, first column is Names)
infoTable<-.%>% DT::formatStyle(1,  color = '#FFFFFF',
                            backgroundColor = '#0d47a1',
                            fontWeight = 'bold')

inputTable<-.%>% DT::formatStyle(2,
                             fontWeight = 'bold',
                             textAlign = 'center')


##  3. View table (no selcetion, all columns the same)
viewTable<-.%>% DT::formatStyle(1,  color = '#FFFFFF',
                            backgroundColor = '#33D2FF',
                            fontWeight = 'bold')

# ###############################
## SET to 20gb for future parallel
options(future.globals.maxSize=70000*1024^2)
#plan(multiprocess)


################################
## Small Helpers
################################
long2UTM <- function(long=NULL) {
  (floor((long + 180)/6) %% 60) + 1
}


################################
## RASTER FUNCTIONS

## 1. Fast raster replace NA (attention Memory!)
ras_NA_to_0<-function(rf=NULL) {
  rfDF <- raster::values(rf)
  rfDF[is.na(rfDF)]<-0
  raster::values(rf)<-rfDF
  rfProj<-raster::projection(rf)
  return(rf)
}

ras_strat_kmeans<-function(rf=NULL, n_strata=10) {
  rstDF <- raster::values(rf)
  km<-stats::kmeans(rstDF, centers = n_strata, iter.max = 50)
  kmClust <- vector(mode = "integer", length = raster::ncell(rf))
  kmClust<- km$cluster
  tmpRstKM <- raster(rf[[1]])
  raster::values(tmpRstKM) <- kmClust
  return(tmpRstKM)
}

ras_strat_clara<-function(rf=NULL, n_strata=10) {
  rstDF <- raster::values(rf)
  km<-cluster::clara(rstDF, k = n_strata, metric = "manhattan")
  kmClust <- vector(mode = "integer", length = raster::ncell(rf))
  kmClust<- km$cluster
  tmpRstKM <- raster(rf[[1]])
  raster::values(tmpRstKM) <- kmClust
  return(tmpRstKM)
}
