#!/usr/bin/env python3
"""
    Console client for the King Game
"""
import sys
import os
import time
import client

USR_NAME = ''
CLR_CMD = 'cls' if sys.platform == 'win32' else 'clear'

def clear_screen():
    _ = os.system(CLR_CMD)

def print_hand(player):
    print("Your Hand:", player.game.hand, "\n")

class HumanPlayer(client.GamePlayer):
    """ Implements a Console based human player """
    def choose_game(self, choices):
        clear_screen()
        print("A new hand is about to start, you must choose a game.")
        print_hand(self)
        print("Your choices:", choices, "\n")
        choice = ''
        while not choice:
            res = input('>')
            choice = res if res in choices else ''

        return choice

    def bid(self):
        print("You have a chance to bid, inform value:")
        bid = -1
        while bid < 0:
            try:
                bid = int(input('>'))
            except ValueError:
                pass

        return bid

    def game_selected(self):
        clear_screen()
        print("A new hand started. The game will be", self.game.game)
        time.sleep(2)

    def play_card(self):
        clear_screen()
        print("It's your turn, choose card. (Game is %s)\n"%(self.game.game))
        print_hand(self)
        print("Table is:", self.game.table)
        card = ''
        while not card:
            res = input('>')
            card = res if res in self.game.hand else ''

        return card

    def card_played(self):
        name = self.game.turn if self.game.turn != USR_NAME else 'You'
        print(name, "played", self.game.table[-1])
        print("Table now is: ", self.game.table)
        time.sleep(2)

    def end_round(self, winner):
        name, verb = (winner, "takes") if winner != USR_NAME else ('You', 'took')
        print(name, verb, "the round")
        time.sleep(4)
        clear_screen()

    def end_hand(self):
        time.sleep(1)
        clear_screen()

def choose_table(server):
    """ Allows user to choose a game of his choice:
        Create Table, Join Open, Single Player (spawns some bots) """
    # TODO Implement user IO for selection
    return server.hunt_table()

if __name__ == "__main__":

    if len(sys.argv) > 1:
        USR_NAME = sys.argv[1]

    client.run(USR_NAME, HumanPlayer(), choose_table)
    