#!/usr/bin/env python3

import sys
import client

USR_NAME = ''

class HumanPlayer(client.GamePlayer):
    """ Implements a Console based human player """
    def choose_game(self, choices):
        pass

    def play_card(self):
        pass

def choose_table(server):
    """ Allows user to choose a game of his choice:
        Create Table, Join Open, Single Player (spawns some bots) """
    # TODO Implement user IO for selection
    return server.hunt_table()

if __name__ == "__main__":

    if len(sys.argv) > 1:
        USR_NAME = sys.argv[1]

    client.run(USR_NAME, HumanPlayer(), choose_table)
    