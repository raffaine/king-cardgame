# -*- coding: cp1252 -*-
from kingClient import *

import os
import sys
import time

def _raise():
    raise NotImplementedError('platform not supported')

clear_screen = lambda:\
{
    'win32': os.system('cls'),
    'linux2': os.system('clear'),
    'mac': os.system('clear')
}.get(sys.platform, _raise)

table_order = []
actual_hand = ''
game_score = ''
actual_score = ''

def prompt(print_list):
    clear_screen()
    print "KING, THE CARD GAME"
    if table_order:
        print "Table order is: ", table_order
    if game_score:
        print "Game score is: ", game_score
    if actual_score:
        print "Round score is: ", actual_score
    for elem in print_list:
        print elem

class HumanClient(Game):
    def __init__(self):
        Game.__init__(self)
        print "KING, THE CARD GAME"
    
    def getInput(self, action='', *args):
        if action == 'CARD':
            prompt(["Your turn to play, Game is: ", actual_hand,
                    "Table is: ", args[0],
                    "Choose one card from your hand:", args[1]])
            return raw_input().upper()
        elif action == 'NAME':
            prompt(["Before start playing you must choose your name:"])
            return raw_input() or 'safado_que_nao_escolheu_o_nome'
        elif action == 'TABLE':
            prompt(["There are some available tables", args[0],
                    "Choose table number:"])
            return raw_input() or '0'
        elif action == 'HAND':
            prompt(["You must decide for your hand",
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
        table_order = self.order                

    def startRound(self, message):
        Game.startRound(self, message)
        actual_hand = message

    def endRound(self, message):
        Game.endRound(self, message)

        message = message.split()
        actual_score = ''.join(message[2:])
        print "%s take this round."%(message[1])
        time.sleep(2)

    def endHand(self, message):
        Game.endHand(self, message)
        game_score = message

    def gameEnd(self, message):
        Game.gameEnd(self, message)
        print "That is the end of the game, press any key to exit"
        raw_input()

if __name__ == "__main__":
    log.basicConfig(stream = sys.stdout, level = log.INFO)
    cGame = HumanClient()
    cGame.start()
    







