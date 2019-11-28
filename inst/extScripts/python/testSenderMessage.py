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
pwd = dataConfQueue["pw"]
ipAddr = dataConfQueue["host"]
port = dataConfQueue["port"]

credentials = pika.PlainCredentials(user, pwd)
parameters = pika.ConnectionParameters(ipAddr, port, credentials=credentials)
connection = pika.BlockingConnection(parameters)
channel = connection.channel()
channel.queue_declare(queue='RTfogDec')


exampleMessageBody = "/nas-research.knmi.nl/sensordata/CAMERA/RWS/A4/HM636/ID111942/201807/A4-HM636-ID111942_20180704_1320.jpg"

mex2 = "/nas-research.knmi.nl/sensordata/CAMERA/RWS/A15/HM397/ID106985/201807/A15-HM397-ID106985_20180704_1320.jpg"




channel.basic_publish(exchange='',
                      routing_key='RTfogDec',
                      body=mex2)
print("message delivered")

connection.close()
