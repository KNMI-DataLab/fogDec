import pika
import json
from jsonpath_rw import parse
import subprocess
import datetime
import logging
from pymongo import *
from bson import json_util


def readJson(confFile):
    with open(confFile) as data_file:
        data = json.load(data_file)
    return data

#extract the relevant section of JSON config file
def extractFromJSON(confJson, stringForSearch):
    toSearch = parse(stringForSearch)  # info at http://goessner.net/articles/JsonPath/
    stations = toSearch.find(confJson)[0].value
    return (stations)




def writeToMongo(message):
    #mongo db connection params
    client = MongoClient('145.23.219.231', 27017)
    #access to db
    db = client['fogDetectionArchive']
    data = json_util.loads(message)
    postedMessage = db["collection"].insert_one(data).inserted_id
    return(postedMessage.acknowledged)





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

    if (body != None):
        status = writeToMongo(body)
        logging.info("message sent to mongo, status"+status)
    else:
        logging.info("message is empty")



#setup logging
logging.basicConfig(filename='logFile.log',level=logging.INFO)



subscribeAndConsume()

