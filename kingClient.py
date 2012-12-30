# -*- coding: utf-8 -*-
import pika
import time
import sys
import random

import logging as log

ref = ['2','3','4','5','6','7','8','9','10','J','Q','K','A']

def handSort(hand):
    return [ ref[v]+s for s,v in sorted(map(lambda u: (u[-1],ref.index(u[:-1])),hand))]            

class Game:
    def __init__(self):
        credentials = pika.PlainCredentials('guest', 'guest')
        parameters = pika.ConnectionParameters('localhost',5672,'/',credentials)
        
        self.connection = pika.BlockingConnection(parameters)
        self.channel = self.connection.channel()

        self.result = self.channel.queue_declare('',exclusive=True)
        self.queue_name = self.result.method.queue

        self.channel.basic_qos(prefetch_count=1)
        self.channel.basic_consume(self.callback,
                                    queue=self.queue_name)

        self.table_name = ''
        self.numhands = 0
        self.possibleHands = []
        self.process = self.listTables
        log.info('[SETUP] Up and Running')
    
    def callback(self, ch, method, properties, body):
        log.info("[CALLBACK] Received %r"%(body,))
        self.process( body )
        ch.basic_ack(delivery_tag = method.delivery_tag)

    def getInput(self, action='', *params):
        raise NotImplementedError('getInput must be implemented by derived classes')

    def publishHall(self, command, message='empty'):
        props = pika.BasicProperties(reply_to = self.queue_name)
        self.channel.basic_publish( exchange='hall',
                                    routing_key=command,
                                    properties=props,
                                    body=message)
        log.info('[SENTHALL] %s(%s)'%(command,message))

    def publishTable(self, command, message='empty'):
        command = '%s.%s'%(self.table_name,command)
        props = pika.BasicProperties(reply_to = self.queue_name)
        self.channel.basic_publish(exchange='king',
                                       routing_key=command,
                                       properties=props,
                                       body=message)
        log.info('[SENT] %s(%s)'%(command, message))

    def joinTable(self, message):
        log.info('[AGENT] JOIN %s'%(message))
        self.process = self.startGame
        
        if message != "OK":
            self.process = self.listTables
            self.publishHall('agenthall.listTable','open')
            return
        
    def createTable(self,message):
        self.table_name = message
        self.publishTable('join', self.name)
        self.process = self.joinTable        

    def listTables(self, message):
        log.info('[HALL] LIST %s'%(message))
        self.table_name = ''
        lTables = eval(message)
        num = len(lTables)
        if not num:
            log.info("no table available, creating a new one.")
            self.publishHall('agenthall.createTable')
            self.process = self.createTable
        else:
            while not self.table_name:
                choice = int(self.getInput('TABLE', zip(range(num),lTables)) or 0)
                self.table_name = lTables[choice] if choice < num else ''
            self.publishTable('join', self.name)
            self.process = self.joinTable

    def startGame(self, message):
        log.info('[AGENT] START GAME %s'%(message))
        self.order = eval(message.lstrip('ORDER'))
        self.position = self.order.index(self.name)
        self.starter = 0
        self.process = self.setupHand

    def chooseHand(self, message):
        log.info('[AGENT] CHOOSE HAND %s'%(message))
        self.possibleHands = eval(message.lstrip('CHOOSE'))        
        game_name = ''
        while not self.possibleHands.count(game_name):
            game_name = self.getInput('HAND') or ''
            
        self.process = self.startRound
        self.publishTable('chooseHand', game_name)
        

    def setupHand(self, message):
        log.info('[AGENT] SETUP HAND %s'%(message))
        self.hand = handSort(eval(message.lstrip('CARDS')))
        
        self.match = 0
        self.process = self.startRound \
                       if self.starter != self.position \
                       else self.chooseHand
            

    def startRound(self, message):
        log.info('[PLAYER] START ROUND %s'%(message))
        self.process = self.waitRound
        self.table = []
        self.round = 0
        self.playRound()

    def playRound(self):
        if self.starter == self.position:
            while True:
                card = self.getInput('CARD', self.table, self.hand)
                if self.hand.count(card):
                    break           

            self.publishTable('play', card)

    def waitRound(self, message):
        log.info('[AGENT] ROUND %s'%(message))
        if message.startswith('ENDHAND'):
            self.endHand(message)
            return
        
        if message.startswith('PLAY'):
            self.table.append( message.split()[-1] )  

            if self.starter == self.position:
                self.hand.remove( self.table[-1] )    
          
            self.starter = (self.starter + 1)%4
            self.round += 1
            if self.round == 4:
                self.process = self.endRound
                return

        self.playRound()

    def endRound(self, message):
        log.info('[AGENT] END ROUND %s'%(message))
        if not message.startswith('ENDROUND'):
            raise KeyboardInterrupt # TODO estado inválido, sei lá o que fazer

        message = message.split()
        score = ''.join(message[2:])

        self.match += 1
        if self.match < 13:  
            self.starter = (self.order.index(message[1]))
            self.startRound('')
        else:
            self.process = self.endHand

    def endHand(self, message):
        log.info('[AGENT] END HAND %s'%(message))
        if not message.startswith('ENDHAND'):
            raise KeyboardInterrupt # TODO estado inválido, sei lá o que fazer
        
        self.numhands += 1
        self.starter = self.numhands % 4
        self.process = self.setupHand if self.numhands < 10 else self.gameEnd

    def gameEnd(self, message):
        log.info('[AGENT] END GAME %s'%(message))
        if not message.startswith('GAMEOVER'):
            raise KeyboardInterrupt # TODO estado inválido, sei lá o que fazer
        self.channel.stop_consuming()
                
    def start(self, single=False):
        self.name = self.getInput('NAME') or 'empty'
        if single:
            self.publishHall('agenthall.createTable','single')
            self.process = self.createTable
        else:
            self.publishHall('agenthall.listTable','open')
        
        try:
           self.channel.start_consuming()
        except KeyboardInterrupt:
            log.info("Quiting Game")
            self.publishTable('quit')        
        except:
            log.info("Unexpected error: %s"%(sys.exc_info()[0]))
            raise






