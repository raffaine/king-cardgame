#!/usr/bin/env python3

import sys
import random
import client

class RandomPlayer(client.GamePlayer):
    def choose_game(self, choices):
        return random.choice(choices)

    def play_card(self):
        return random.choice(self.game.hand)

    def bid(self):
        # Always forfeit bidding
        return 0

    def decide(self):
        # Randomly accepts or refuses it
        return random.choice([True, False])

    def choose_trample(self):
        # Chooses some random suit if player won the bidding
        return random.choice(list("SCHD"))

if __name__ == "__main__":
    usr = ''

    if len(sys.argv) > 1:
        usr = sys.argv[1]

    random.seed()
    client.run(usr, RandomPlayer())
