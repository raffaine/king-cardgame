# -*- coding: cp1252 -*-
from kingClient import *

import os
import sys
import time

def clear_screen():
    os.system({'win32': 'cls'}.get(sys.platform, 'clear'))

class HumanClient(Game):
    def __init__(self):
        Game.__init__(self)
        self.table_order = []
        self.actual_hand = ''
        self.game_score = ''
        self.actual_score = ''
    
    def prompt(self, print_list):
        clear_screen()
        print "KING, THE CARD GAME"
        if self.table_order:
            print "Table order is: ", self.table_order
        if self.game_score:
            print "Game score is: ", self.game_score
        if self.actual_score:
            print "Round score is: ", self.actual_score
        for elem in print_list:
            print elem
            
    def getInput(self, action='', *args):
        if action == 'CARD':
            self.prompt(["Your turn to play, Game is: ", self.actual_hand,
                         "Table is: ", args[0],
                         "Choose one card from your hand:", args[1]])
            return raw_input().upper()
        elif action == 'NAME':
            self.prompt(["Before start playing you must choose your name:"])
            return raw_input() or 'safado_que_nao_escolheu_o_nome'
        elif action == 'TABLE':
            self.prompt(["There are some available tables", args[0],
                         "Choose table number:"])
            return raw_input() or '0'
        elif action == 'HAND':
            self.prompt(["You have the following cards",
                         self.hand,
                         "You must decide for your hand",
                         self.possibleHands,
                         "Choose one hand from the list:"])
            return raw_input().upper()
        else:
            Game.getInput(self, action, args)
    
    def joinTable(self, message):
        Game.joinTable(self, message)
        if self.process == self.startGame:
            print "You have successfully joined the table. waiting full table"
        
    def startGame(self, message):
        Game.startGame(self, message)
        self.table_order = self.order                

    def startRound(self, message):
        if message == 'HAND:
            self.actual_hand = ' '.join(message.split()[1:])
        Game.startRound(self, message)

    def endRound(self, message):
        print "Final Table: ",self.table
        n_message = message.split()
        self.actual_score = ''.join(n_message[2:])
        print "%s take this round."%(n_message[1])
        time.sleep(2)
        
        Game.endRound(self, message)

    def endHand(self, message):
        self.game_score = message
        Game.endHand(self, message)

    def gameEnd(self, message):
        Game.gameEnd(self, message)
        print "That is the end of the game, press any key to exit"
        raw_input()

if __name__ == "__main__":
    log.basicConfig(filename='human.log', filemode='w', level = log.INFO)
    cGame = HumanClient()
    cGame.start()
    







