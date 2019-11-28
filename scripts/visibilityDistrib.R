library(DBI)
library(fogDec)
library(jsonlite)
library(data.table)
library(knmiR)
library(postGIStools)
library(parallel)
library(ggplot2)





dbConfig <- fromJSON("config.json")
con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = "FOGDB",
                 host = dbConfig[["host"]], port = 9418,
                 user = dbConfig[["user"]], password = dbConfig[["pw"]])
meteotable<-dbReadTable(con, "meteo_features_copy")
meteotable<-data.table(meteotable)

locations<-dbReadTable(con, "locations")
locations<-data.table(locations)
dbDisconnect(con)

locations[location_id %in% unique(meteotable$location_id)]


stationsName<-c(
"501" ="knmi Cabauw locatie A",
"503" ="knmi De Bilt test locatie A",
"505" ="knmi Deelen locatie 20t",
"506" ="knmi Eelde locatie 23t",
"511" ="knmi Herwijnen",
"518" ="knmi Maastricht locatie 22t",
"534" ="knmi Rotterdam Beergat locatie RO04",
"537" ="knmi Rotterdam Caland oost locatie RP32",
"539" ="knmi Rotterdam Geulhaven locatie RP10",
"541" ="knmi Rotterdam Maasvlakter Slufter",
"542" ="knmi Schiphol locatie 18Ct",
"536" ="knmi Rotterdam locatie 24t")




ggplot(data = meteotable[location_id!=511],aes(mor_visibility))+geom_histogram()+facet_wrap(~location_id, ncol=3,scales="free_y", labeller =as_labeller(stationsName) )+scale_x_log10(breaks=c(10,50,100,200,500,1000,10000))+scale_y_log10()












