import pika

connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()
for offset in range(0, 1000000):
    channel.basic_publish(exchange='test', routing_key='', body= 'Test {}'.format(offset))
connection.close()