#!/usr/bin/env python3

import sys
import random
import client

def random_choice(game, choices):
    return random.choice(choices)

def random_card(game):
    return random.choice(game.hand)

if __name__ == "__main__":
    usr = ''

    if len(sys.argv) > 1:
        usr = sys.argv[1]

    random.seed()
    client.run(usr, random_choice, random_card)
