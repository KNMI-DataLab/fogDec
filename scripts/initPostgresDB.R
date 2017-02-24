require("RPostgreSQL")
require("RJSONIO")


dbConfig<-readJSONStream("config.json")

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
connectionSetup <- dbConnect(drv, dbname = "FOGDB",
                 host = dbConfig[["host"]], port = 5432,
                 user = dbConfig[["user"]], password = dbConfig[["pw"]])



tableTimeProperties <- dbGetQuery(connectionSetup, "CREATE TABLE time_properties (
                                                    time_id integer NOT NULL,
                                                    year integer, 
                                                    month integer, 
                                                    day integer, 
                                                    hour integer, 
                                                    minute integer,
                                                    PRIMARY KEY(time_id));")

tableLocations <- dbGetQuery(connectionSetup, "CREATE TABLE locations (
                                               location_id integer NOT NULL,
                                               location_description varchar,
                                               longitude double precision, 
                                               latitude double precision,
                                               PRIMARY KEY(location_id));")

tableCameras <- dbGetQuery(connectionSetup, "CREATE TABLE cameras (
                                             camera_id integer NOT NULL,
                                             location_id integer references locations(location_id),
                                             camera_description varchar,
                                             camera_name varchar,
                                             PRIMARY KEY (camera_id));")


tableAnnotation <- dbGetQuery(connectionSetup, "CREATE TABLE manual_annotations (
                                                camera_id integer NOT NULL,
                                                time_id integer NOT NULL references time_properties(time_id),
                                                visibility_qualitative varchar,
                                                annotation varchar,
                                                PRIMARY KEY(camera_id, time_id));")

tableImageFeatures <- dbGetQuery(connectionSetup, "CREATE TABLE image_features (
                                                  camera_id integer NOT NULL references cameras(camera_id), 
                                                  time_id integer NOT NULL references time_properties(time_id), 
                                                  mean_edge double precision, 
                                                  change_point double precision, 
                                                  smoothness double precision, 
                                                  fractal_dim double precision, 
                                                  mean_hue double precision, 
                                                  mean_saturation double precision, 
                                                  mean_brightness double precision,       
                                                  PRIMARY KEY(camera_id, time_id));")

tableMeteoFeatures <- dbGetQuery(connectionSetup, "CREATE TABLE meteo_features (
                                                  location_id integer NOT NULL references locations(location_id), 
                                                  time_id integer NOT NULL references time_properties(time_id), 
                                                  wind_speed double precision, 
                                                  rel_humidity double precision, 
                                                  air_temp double precision, 
                                                  dew_point double precision,                                                         
                                                  mor_visibility double precision,
                                                  PRIMARY KEY(location_id, time_id));")

tableDayPhases <- dbGetQuery(connectionSetup, "CREATE TABLE day_phases (
                                              location_id integer NOT NULL references time_properties(time_id),
                                              year integer NOT NULL, 
                                              day integer NOT NULL,
                                              morning_twilight boolean,
                                              evening_twilight boolean,
                                              daylight boolean, 
                                              night boolean,  
                                              PRIMARY KEY(location_id, year, day));")

tableImages <- dbGetQuery(connectionSetup, "CREATE TABLE images (
                                            camera_id integer NOT NULL references cameras(camera_id),
                                            time_id integer NOT NULL references time_properties(time_id),
                                            filepath varchar,
                                            PRIMARY KEY(camera_id, time_id));")
                          






