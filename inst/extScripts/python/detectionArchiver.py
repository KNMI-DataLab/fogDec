import pika
import json
from jsonpath_rw import parse
import subprocess
import datetime
import logging
from pymongo import *

def readJson(confFile):
    with open(confFile) as data_file:
        data = json.load(data_file)
    return data

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



def writeToMongo(message):

#mongo db connection params
    client = MongoClient('145.23.218.16', 27017)

#access to db
    db = client['fogDetectionArchive']

#collection AKA table in db
    collection = db["collection"]

#getting the collections available
collRetrival= db.collection_names(include_system_collections=False)

print(collRetrival)






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

    channel.queue_bind(exchange='detectionOutput', queue='detectionArchive')
    channel.basic_qos(prefetch_count=1)
    channel.basic_consume(callback, queue='detectionArchive')
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



#read the config file in JSON
camerasMVPConf = readJson(confFile)
locationToProcess = extractLocations(camerasMVPConf)
camerasToProcess = extractCameras(camerasMVPConf)

#setup logging
logging.basicConfig(filename='logFile.log',level=logging.INFO)



subscribeAndConsume()

