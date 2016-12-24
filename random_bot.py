#!/usr/bin/env python3

import sys
import random
import client

class RandomPlayer(client.GamePlayer):
    def choose_game(self, choices):
        return random.choice(choices)

    def play_card(self):
        return random.choice(self.game.hand)

if __name__ == "__main__":
    usr = ''

    if len(sys.argv) > 1:
        usr = sys.argv[1]

    random.seed()
    client.run(usr, RandomPlayer())
