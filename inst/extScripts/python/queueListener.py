import pika
import json
from jsonpath_rw import parse
import ephem
import subprocess
import datetime
import logging
import re

def readJson(confFile):
    with open(confFile) as data_file:
        data = json.load(data_file)
    return data

#function to compute the phase of the day (day, night, dawn, dusk)
#based on the day of year and latitude and longitude of the location
#an ID corresponding to the phase of the day is returned, the ID is
# based on the mapping decided and put in the DB table day_phase
def computeDayPhase(lon, lat, dateTime):
    obs = ephem.Observer()
    obs.lat = str(lat)
    obs.lon = str(lon)
    #changing date to midday to avoid that previous sunrise and next sunset are in the same day
    dateToUse = dateTime.replace(hour=12, minute=00)
    obs.date = dateToUse
    #obs.date = "2017/5/16 12:00"

    # Civil twilight uses the value -6 degrees
    # Nautical twilight uses the value -12 degrees
    # Astronomical twilight uses the value -18 degrees

    obs.horizon = "-18"
    try:
        astroDawn = obs.previous_rising(ephem.Sun(), use_center=True)
        astroDusk = obs.next_setting(ephem.Sun(), use_center=True)
    except ephem.AlwaysUpError:
        astroDawn = ephem.Date(dateTime - datetime.timedelta(days=1))
        astroDusk = ephem.Date(dateTime + datetime.timedelta(days=1))
        #print("There is no night")
    #astroDusk = obs.next_setting(ephem.Sun(), use_center=True)

    obs.horizon = "-12"
    try:
        nauticalDawn = obs.previous_rising(ephem.Sun(), use_center=True)
        nauticalDusk = obs.next_setting(ephem.Sun(), use_center=True)
    except ephem.AlwaysUpError:
        nauticalDawn = ephem.Date(dateTime - datetime.timedelta(days=1))
        nauticalDusk = ephem.Date(dateTime + datetime.timedelta(days=1))
        #print("There is no astronomical dawn-dusk")

    obs.horizon = "-6"
    try:
        civilDawn = obs.previous_rising(ephem.Sun(), use_center=True)
        civilDusk = obs.next_setting(ephem.Sun(), use_center=True)
    except ephem.AlwaysUpError:
        civilDawn = ephem.Date(dateTime - datetime.timedelta(days=1))
        civilDusk = ephem.Date(dateTime + datetime.timedelta(days=1))
        #print("There is no nautical dawn-dusk")

    #for the setting used refer to:
    #http://rhodesmill.org/pyephem/quick#transit-rising-setting
    obs.horizon = "0"
    try:
        sunrise = obs.previous_rising(ephem.Sun(), use_center=True)
        sunset = obs.next_setting(ephem.Sun(), use_center=True)
    except ephem.AlwaysUpError:
        sunrise = -1
        sunset = -1
        #print("There is no civil dawn-dusk")

    dayPhaseID = -1

    if(dateTime < sunrise.datetime() and dateTime >= civilDawn.datetime()):
        #print "civil dawn"
        dayPhaseID =10
    if (dateTime < civilDawn.datetime() and dateTime >= nauticalDawn.datetime()):
        #print "nautical dawn"
        dayPhaseID = 20
    if (dateTime < nauticalDawn.datetime() and dateTime > astroDawn.datetime()):
        #print "astro dawn"
        dayPhaseID = 30
    if(dateTime <= astroDawn.datetime()):
        #print "night"
        dayPhaseID  = 0
    if(dateTime >= sunrise.datetime() and dateTime <= sunset.datetime()):
        #print "day"
        dayPhaseID = 1
    if (dateTime > sunset.datetime() and dateTime <= civilDusk.datetime()):
        #print "civil dusk"
        dayPhaseID = 11
    if (dateTime > civilDusk.datetime() and dateTime <= nauticalDusk.datetime()):
        #print "nautical dusk"
        dayPhaseID = 21
    if (dateTime > nauticalDusk.datetime() and dateTime < astroDusk.datetime()):
        #print "astro dusk"
        dayPhaseID = 31
    if (dateTime >= astroDusk.datetime()):
        #print "night"
        dayPhaseID = 0
    return(dayPhaseID)


#extract the relevant section of JSON config file
def extractFromJSON(confJson, stringForSearch):
    toSearch = parse(stringForSearch)  # info at http://goessner.net/articles/JsonPath/
    stations = toSearch.find(confJson)[0].value
    return (stations)


#extract the locations from JSON config file
def extractLocations(confJson):
    locations = []
    pathToSearch = parse("$..location")  # info at http://goessner.net/articles/JsonPath/
    for match in pathToSearch.find(confJson):
        locations.append(match.value)
    return(locations)


#extract the cameraID from JSON config file
def extractCameras(confJson):
    cameras = []
    cameraIDs = parse("$..cameraID")
    for match in cameraIDs.find(confJson):
        cameras.append(match.value)
    return cameras


#filters the dayphase and use the appropriate script (at the moment daylight only)
def filterDayPhase(message):
    group = re.findall("_[0-9]*_[0-9]*",message)
    dateAndTime = group[-1]
    splitsDateAndTime = dateAndTime.split("_")

    #nowUTC = datetime.datetime.utcnow()

    completeDateTime = datetime.datetime.strptime(dateAndTime, "_%Y%m%d_%H%M")
    print(completeDateTime)
    #completeDateTime = datetime.datetime.strptime(
     #   str(nowUTC.year) + str(nowUTC.month).zfill(2)  + str(nowUTC.day).zfill(2)  + str(nowUTC.hour).zfill(2)  + str(nowUTC.minute).zfill(2) , "%Y%m%d%H%M")
    dayPhasePic = computeDayPhase(lonDeBilt, latDeBilt, completeDateTime)
    print("day phase"+str(dayPhasePic))
    #dayPhaseNow=11

    # way to call R in python subprocess.call (["/usr/bin/Rscript", "--vanilla", "/pathto/MyrScript.r"])
    callToRScript = {
    1: ["/usr/bin/Rscript", "--vanilla", "/home/pagani/development/fogDec/scripts/executionNoCluster.R"],
    0: ["/usr/bin/Rscript", "--vanilla", "/home/pagani/development/fogDec/scripts/executionNoModelAvailable.R"],
    10:["/usr/bin/Rscript", "--vanilla", "/home/pagani/development/fogDec/scripts/executionNoModelAvailable.R"],
    11:["/usr/bin/Rscript", "--vanilla", "/home/pagani/development/fogDec/scripts/executionNoModelAvailable.R"],
    20:["/usr/bin/Rscript", "--vanilla", "/home/pagani/development/fogDec/scripts/executionNoModelAvailable.R"],
    21:["/usr/bin/Rscript", "--vanilla", "/home/pagani/development/fogDec/scripts/executionNoModelAvailable.R"],
    30:["/usr/bin/Rscript", "--vanilla", "/home/pagani/development/fogDec/scripts/executionNoModelAvailable.R"],
    31:["/usr/bin/Rscript", "--vanilla", "/home/pagani/development/fogDec/scripts/executionNoModelAvailable.R"]
    }

    result = callToRScript.get(dayPhasePic, None)

    return result


#filter the message if belonging to the list of locations approved
def filterMessage(message, locationToProcess, camerasToProcess):
    if any(s for s in locationToProcess if s in message):
        if any(ss for ss in camerasToProcess if ss in message):
            return (message)
        else:
            return(None)
    else:
        return(None)


#function to subscribe to the message queue
#and handle the behavior when a message is received
def subscribeAndConsume():
    confFileQueue = "queueConfig.json"
    dataConfQueue = readJson(confFileQueue)
    user = dataConfQueue["user"]
    pwd = dataConfQueue["pw"]
    ipAddr = dataConfQueue["host"]
    port = dataConfQueue["port"]

    credentials = pika.PlainCredentials(user, pwd)
    parameters = pika.ConnectionParameters(ipAddr, port, credentials=credentials)
    connection = pika.BlockingConnection(parameters)
    channel = connection.channel()

    channel.exchange_declare(exchange='imagesTest', exchange_type='fanout')
    channel.queue_declare(queue='RTfogDec', durable=False)
    channel.queue_bind(exchange='imagesTest', queue='RTfogDec')
    channel.basic_qos(prefetch_count=1)
    channel.basic_consume(callback, queue='RTfogDec')
    channel.start_consuming()


#function called at every message arrival
def callback(ch, method, properties, body):
    print(" [x] Received %r" % body)
    ch.basic_ack(delivery_tag=method.delivery_tag)
    logging.info('message received '+body)


    message = filterMessage(body, locationToProcess, camerasToProcess)
    if (message != None):
        callParams = filterDayPhase(message)
        logging.info("call param created")
        if (callParams != None):
            callParams.append(body)
            logging.info("call param created"+ str(callParams))

            #each process is run in parallel
            subprocess.Popen(callParams)
            logging.info("call param returned for "+ str(callParams))


#exampleMessageBody = "/nas-research.knmi.nl/sensordata/CAMERA/RWS/A2/HM776/ID10912/201806/A2-HM776-ID10912_20180606_0801.jpg"




#at the moment assuming De Bilt as the location where we assess the dayphase
#for running the model for daylight (only model at the moment) or for
#other moment of the day (future work)

lonDeBilt = 5.1809676
latDeBilt = 52.1092717

confFile = "MVPCameras.json"

#read the config file in JSON
camerasMVPConf = readJson(confFile)
locationToProcess = extractLocations(camerasMVPConf)
camerasToProcess = extractCameras(camerasMVPConf)

#setup logging
logging.basicConfig(filename='logFile.log',level=logging.INFO)



subscribeAndConsume()

