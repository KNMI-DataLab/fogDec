import pika
import json
from jsonpath_rw import parse

def readJson(confFile):
    with open(confFile) as data_file:
        data = json.load(data_file)
    return data

confFileQueue = "queueConfig.json"

dataConfQueue = readJson(confFileQueue)
user = dataConfQueue["user"]

# credentials = pika.PlainCredentials(user, pwd)
# parameters = pika.ConnectionParameters(ipAddr, port, credentials=credentials)
# connection = pika.BlockingConnection(parameters)
# channel = connection.channel()
#
# channel.queue_declare(queue='test')

confFile = "MVPCameras.json"

#read the config file in JSON


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





camerasMVPConf = readJson(confFile)

locationToProcess = extractLocations(camerasMVPConf)
camerasToProcess = extractCameras(camerasMVPConf)



print("abc")

#
# def callback(ch, method, properties, body):
#     print(" [x] Received %r" % body)
#     #ch.basic_ack(delivery_tag=method.delivery_tag)
#
# channel.basic_consume(callback, queue='test')
#
# print(' [*] Waiting for messages. To exit press CTRL+C')
# channel.start_consuming()


exampleMessageBody = "/nas-research.knmi.nl/sensordata/CAMERA/RWS/A2/HM776/ID10915/201806/A2-HM776-ID10915_20180606_0801.jpg"

def filterMessage(message, locationToProcess, camerasToProcess):
    if any(s for s in locationToProcess if s in exampleMessageBody):
        if any(ss for ss in camerasToProcess if ss in exampleMessageBody):
            print("ABBB")

#==0 and message.find(camerasToProcess)==0):
#print("location camera should be processed")




filterMessage(exampleMessageBody, locationToProcess, camerasToProcess)