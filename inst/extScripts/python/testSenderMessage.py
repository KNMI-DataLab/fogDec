import pika

credentials = pika.PlainCredentials('guest', 'guest')
parameters = pika.ConnectionParameters('145.23.219.231', 5672, credentials=credentials)
connection = pika.BlockingConnection(parameters)
channel = connection.channel()
channel.queue_declare(queue='test')


channel.basic_publish(exchange='',
                      routing_key='test',
                      body='Hello World!55555')
print(" [x] Sent 'Hello World!'")

connection.close()