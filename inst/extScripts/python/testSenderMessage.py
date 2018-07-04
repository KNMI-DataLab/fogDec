import pika

credentials = pika.PlainCredentials('guest', 'guest')
parameters = pika.ConnectionParameters('145.23.219.231', 5672, credentials=credentials)
connection = pika.BlockingConnection(parameters)
channel = connection.channel()
channel.queue_declare(queue='test')


exampleMessageBody = "/nas-research.knmi.nl/sensordata/CAMERA/RWS/A4/HM636/ID111942/201807/A4-HM636-ID111942_20180704_1320.jpg"

mex2 = "/nas-research.knmi.nl/sensordata/CAMERA/RWS/A15/HM397/ID106985/201807/A15-HM397-ID106985_20180704_1320.jpg"




channel.basic_publish(exchange='',
                      routing_key='test',
                      body=exampleMessageBody)
print("message delivered")

connection.close()