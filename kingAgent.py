# -*- coding: utf-8 -*-
import pika
import sys
import os
import logging as log

from king import *

def err(self, user, msg):
  self.error(user, msg)

class KingAgent:

  def __init__(self, tablename):
    log.info('[SETUP] Starting new agent %r'%(tablename))
    self.players = {}
    self.commands = dict(filter(lambda (x,y): x.startswith('KA_'),
                                KingAgent.__dict__.items()))
    
    self.conn_params = pika.ConnectionParameters('localhost')
    self.connection = pika.BlockingConnection( self.conn_params )
    self.channel = self.connection.channel()

    self.channel.exchange_declare(exchange='king', exchange_type='topic')

    result = self.channel.queue_declare(queue='', exclusive=True)
    self.queue_name = result.method.queue

    self.table = KingTable(tablename)
    self.channel.basic_qos(prefetch_count=1)
    self.channel.queue_bind(exchange='king', queue=self.queue_name,
                            routing_key='%s.*'%tablename)          

    self.channel.basic_consume(self.callback, queue=self.queue_name)
    log.info('[SETUP] Connection and Bindings established')

  def publishHall(self, command, message):
    props = pika.BasicProperties(reply_to = self.queue_name)
    self.channel.basic_publish( exchange='hall',
                                routing_key='agenthall.%s'%command,
                                properties=props,
                                body=message)

  def start(self, user):
    log.info('[SETUP] Starting King Agent')

    #Just make a checkpoint with the hall agent to assure binds 
    #self.publishHall('agentCheckpoint', '%s %s'%(os.getpid(), user))
    sys.stdout.write('agentCheckpoint\n')
    sys.stdout.flush()
        
    self.channel.start_consuming()

  def error(self, user, message):
    log.info('[CLIENT ERROR] %r:%r'%(user,message))
    self.reply(user, "ERROR %r"%message)

  def end(self, notify=False):
    log.info('[AGENT] Terminating Agent')
    if notify:
      self.publishHall('agentExit', '%s'%(os.getpid(),))
    self.channel.stop_consuming()
    

  def callback(self, ch, method, properties, body):
    log.info('[MSG] Received %r:%r'%(method.routing_key, body))

    if not properties.reply_to:
      log.info('[MSG] Invalid: No user to reply')
      return

    key = method.routing_key.split('.')

    command = self.commands.get('KA_%s'%(key[1]), err)
    command(self, properties.reply_to, body)

    ch.basic_ack(delivery_tag = method.delivery_tag)

  def reply(self, user, message):
    self.channel.basic_publish(exchange='', routing_key=user, body=message)
    log.info("[REPLY] %r:%r"%(user,message,))

  def notifyAll(self, message):
    for player in self.table.players:
      self.channel.basic_publish(exchange='', routing_key=player.queue, body=message)
    log.info("[NOTIFY ALL] %r"%(message,))

  def sendHands(self):
      c_player = self.table.setupHand()
      for p in self.table.players:
        self.reply(p.queue, 'CARDS %s'%(str(p.hand)))
        
      self.reply(c_player.queue, \
                 'CHOOSE %s'%(str(self.table.possibleHands(c_player))))
      
    
  def KA_join(self, user, message):
    log.info("[join] %r,%r "%(user,message))

    name = message.split()
    if not len(name) or \
       filter(lambda x: x.name == name[0], self.table.players):
      self.error(user, 'Invalid name or name already in table')
      return

    c_user = KingPlayer(name[0], user)
    if not self.table.joinTable(c_user):
      self.reply(user, 'FULL TABLE')
      return

    self.players[user] = c_user
    self.reply(user, 'OK')
    
    if self.table.start():
      self.publishHall('agentExit', '%s'%(os.getpid(),))
      self.notifyAll('ORDER %s'%(str([p.name for p in self.table.players])))
      self.sendHands()

  def KA_list(self, user, message):
    log.info("[list] %:,%r "%(user,message))
    self.reply(user, str(self.table.players.keys()))    

  def KA_chooseHand(self, user, message):
    log.info("[choose] %r:%r "%(user,message))

    game = message.split()[0] if len(message) else ''
    
    if not game or not self.players.has_key(user) or \
       not self.table.startHand(self.players[user], game):
        self.reply(user, 'invalid action')
        return

    self.notifyAll('HAND %s'%game)

  def KA_play(self, user, message):
    log.info("[choose] %r:%r "%(user,message))
                              
    card = message.split()[0] if len(message) else ''
       
    if not card or not self.players.has_key(user) or \
       not self.table.playCard(self.players[user], card):
        self.reply(user, 'invalid action')
        return
    
    self.notifyAll('PLAY %s %s'%(self.players[user].name, card))

    # check for round ending
    winner = self.table.endRound()
    if winner:
        self.notifyAll('ENDROUND %s %s'%(winner.name,
                                         str(self.table.partialScore)))
        # check for hand ending
        if self.table.endHand():
            self.notifyAll('ENDHAND %s %s'%(str(self.table.score[-1]),
                                            str(self.table.getFinalScore())))

            # if game is not over, start another hand
            if self.table.endGame():
              score = self.table.getFinalScore()
              winner = self.table.players[score.index(max(score))]
              self.notifyAll('GAMEOVER %s %s'%(winner.name, str(score)))
              self.end()
            else:
              self.sendHands()

    
  def KA_quit(self, user, message):
    log.info("[QUIT] %r"%(user))
    try:
        c_user = self.players.pop(user)
        #TODO remove user from table, reset the table
        self.notifyAll('QUIT %r'%(c_user.name))
        if not self.table.players:
          self.end(True)
          
    except KeyError:
        pass
    

if __name__ == "__main__":
  if(len(sys.argv) < 3):
    print "WRONG USAGE!"
    exit()
  
  log.basicConfig(stream = sys.stderr,
                  level = log.INFO)
  
  agent = KingAgent(sys.argv[2])
  agent.start(sys.argv[1])
