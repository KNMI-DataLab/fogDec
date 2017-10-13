library(DBI)
library(jsonlite)

dbConfig <- fromJSON("config.json")

connectionSetup <- dbConnect(RPostgreSQL::PostgreSQL(),
                dbname = "FOGDB",
                host = dbConfig[["host"]], port = 9418, # use 9418 within KNMI, default would be 5432. At the moment set to 9418
                user = dbConfig[["user"]], password = dbConfig[["pw"]])



# tableTimeProperties <- dbGetQuery(connectionSetup, "CREATE TABLE time_properties (
#                                                     time_id integer NOT NULL,
#                                                     year integer, 
#                                                     month integer, 
#                                                     day integer, 
#                                                     hour integer, 
#                                                     minute integer,
#                                                     PRIMARY KEY(time_id));")

tableLocations <- dbGetQuery(connectionSetup, "CREATE TABLE locations (
                                               location_id SERIAL,
                                               location_description varchar,
                                               longitude double precision, 
                                               latitude double precision,
                                               unique(longitude, latitude),
                                               PRIMARY KEY(location_id));")

tableCameras <- dbGetQuery(connectionSetup, "CREATE TABLE cameras (
                                             camera_id SERIAL,
                                             location_id integer references locations(location_id),
                                             camera_description varchar,
                                             camera_name varchar,
                                             PRIMARY KEY (camera_id));")

tableDayPhases <- dbGetQuery(connectionSetup, "CREATE TABLE day_phases (
                                              day_phase_id integer NOT NULL,
                                              day_phase_description varchar,  
                                              PRIMARY KEY(day_phase_id));")

tableImages <- dbGetQuery(connectionSetup, "CREATE TABLE images (
                                            image_id SERIAL,
                                            camera_id integer NOT NULL references cameras(camera_id),
                                            timestamp timestamp NOT NULL,
                                            filepath varchar,
                                            day_phase integer NOT NULL references day_phases(day_phase_id),
                                            PRIMARY KEY(image_id));")


tableAnnotation <- dbGetQuery(connectionSetup, "CREATE TABLE manual_annotations (
                                                annotation_id SERIAL,
                                                camera_id integer NOT NULL references cameras(camera_id),
                                                timestamp timestamp NOT NULL,
                                                image_id integer NOT NULL references images(image_id),
                                                visibility_qualitative varchar,
                                                annotation varchar,
                                                PRIMARY KEY(image_id));")

tableImageFeatures <- dbGetQuery(connectionSetup, "CREATE TABLE image_features (
                                                  feature_id SERIAL,
                                                  camera_id integer NOT NULL references cameras(camera_id), 
                                                  timestamp timestamp NOT NULL,
                                                  image_id integer NOT NULL references images(image_id),
                                                  mean_edge double precision, 
                                                  change_point double precision, 
                                                  smoothness double precision, 
                                                  fractal_dim double precision, 
                                                  mean_hue double precision, 
                                                  mean_saturation double precision, 
                                                  mean_brightness double precision,       
                                                  mean_transmission double precision,
                                                  PRIMARY KEY(image_id));")

tableMeteoFeaturesStations <- dbGetQuery(connectionSetup, "CREATE TABLE meteo_features_stations (
                                                  meteo_feature_id SERIAL,
                                                  location_id integer NOT NULL references locations(location_id), 
                                                  timestamp timestamp NOT NULL,
                                                  wind_speed double precision, 
                                                  rel_humidity double precision, 
                                                  air_temp double precision, 
                                                  dew_point double precision,                                                         
                                                  mor_visibility double precision,
                                                  unique(location_id, timestamp),
                                                  PRIMARY KEY(meteo_feature_id));")


tableMeteoStations <- dbGetQuery(connectionSetup, "CREATE TABLE meteo_stations (
                                                  meteo_station_id SERIAL,
                                                  meteo_station_name varchar,
                                                  location_id integer NOT NULL references locations(location_id), 
                                                  knmi_kis_id varchar NOT NULL,
                                                  meteo_station_type varchar,
                                                  meteo_station_location_code varchar,                                                         
                                                  PRIMARY KEY(meteo_station_id));")



viewImagesMeteoMor <- dbGetQuery(connectionSetup, "CREATE VIEW meteo_images AS
                                                   SELECT images.image_id, images.filepath, images.timestamp, images.day_phase, cameras.location_id, cameras.camera_id, meteo_features_stations.mor_visibility
                                                   FROM cameras, images, meteo_features_stations
                                                   WHERE images.camera_id=cameras.camera_id AND cameras.location_id = meteo_features_stations.location_id AND images.timestamp = meteo_features_stations.timestamp;")



                          

dbDisconnect(connectionSetup)




