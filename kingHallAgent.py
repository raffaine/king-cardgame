import pika
import sys
import uuid
import os
import time
import subprocess
import logging as log
import sys

AGENT_PROG = 'kingAgent.py'

class KingHallAgent:

  _tables = {}

  def __init__(self):
    self.commands = dict(filter(lambda (x,y): x.startswith('KH_'),
                                KingHallAgent.__dict__.items()))
    
    self.conn_params = pika.ConnectionParameters('localhost')
    self.connection = pika.BlockingConnection( self.conn_params )
    self.channel = self.connection.channel()

    self.channel.exchange_declare(exchange='hall', exchange_type='topic')

    result = self.channel.queue_declare(queue='', exclusive=True)
    self.channel.queue_bind(exchange='hall',queue=result.method.queue,
                            routing_key='agenthall.*')   

    self.channel.basic_consume(self.callback,
                               queue=result.method.queue)

    log.info('[SETUP] Connection and Bindings established')

  def start(self):
    log.info('[SETUP] Starting Server')
    self.channel.start_consuming()

  def error(self, user, message):
    log.info('[CLIENT ERROR] %r:%r'%(user,message))
    self.reply(user, "ERROR %r"%message)
    

  def callback(self, ch, method, properties, body):
    log.info('[MSG] Received %r:%r'%(method.routing_key, body))

    if not properties.reply_to:
      log.info('[MSG] Invalid: No user to reply')
      return

    key = method.routing_key.split('.')

    command = self.commands.get('KH_%s'%(key[1]), self.error)
    if( command != self.error ):
      command(self, properties.reply_to, body)

    ch.basic_ack(delivery_tag = method.delivery_tag)

  def reply(self, user, message):
    self.channel.basic_publish(exchange='', routing_key=user, body=message)
    log.info("[REPLY] %r:%r"%(user,message,))
    
  def KH_createTable(self, user, message):
    log.info("[create] %r,%r "%(user,message))

    table_name = str(uuid.uuid4()) #must I check for repeated one?
    #Start a new king agent process
    self._tables[table_name] = subprocess.Popen([sys.executable,
                                                 AGENT_PROG,
                                                 user, table_name]).pid
    log.info("[create] Process Started: %r "%(self._tables[table_name]))

  def KH_agentExit(self, user, message):
    log.info('[AGENTEXIT] %r,%r'%(user,message))
    
    tb = filter(lambda (x,y): str(y) == message, self._tables.items())
    if not tb:
      self.reply(user, 'INVALID')
      return

    self._tables.pop(tb[0][0])

  def KH_agentCheckpoint(self, user, message):
    log.info("[CHECKPOINT] %r,%r "%(user,message))

    msg = message.split()
    if len(msg) < 2 :
      self.reply(user, 'INVALID')
      return

    tb = filter(lambda (x,y): str(y) == msg[0], self._tables.items())
    if not tb:
      self.reply(user, 'INVALID')
      return
    
    self.reply(msg[1], tb[0][0])

  def KH_listTable(self, user, message):
    log.info("[list] %r,%r "%(user,message))
    self.reply(user, str(self._tables.keys()))    
    

if __name__ == "__main__":  
  log.basicConfig(stream = sys.stdout, level = log.INFO)
  agent = KingHallAgent()
  try:
    agent.start()
  except KeyboardInterrupt:
    pass
       
    


  
