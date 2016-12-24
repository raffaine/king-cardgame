#!/usr/bin/env python3
import json
import zmq
from king import *

port = 5555

tables = dict()
players = dict()

flatten = lambda l: [item for sublist in l for item in sublist]

def pool_req(handler_map, timeout=1000):
    """Pool Request handler, just to keep it of with game logic"""
    socks = poller.poll(timeout)
    if not len(socks):
        return False

    msg = action_server.recv_string()
    print('received: %s'%msg)

    msg = msg.split(' ')
    cmd = handler_map.get(msg[0], lambda _: 'ERROR: Invalid Message')
    try:
        action_server.send_string(cmd(*msg[1:]))
    except TypeError:
        action_server.send_string('ERROR Invalid Call')

    return True

def create_table(table_name):
    """Manager Logic: TABLE - Creates a new table with the given parameters"""
    if table_name in tables:
        return 'ERROR Table already exists'

    tables[table_name] = KingTable(table_name)
    return 'ACK'

def list_tables():
    """Manager Logic: LIST - List all available tables"""
    return json.dumps(list(map(lambda item: item[0], \
                           filter(lambda item: len(item[1].players) < 4, \
                           tables.items()))))

def leave_table(usr_name, secret, table_name):
    """Game Logic: LEAVE - Used to remove players from a waiting table"""
    player = KingPlayer(usr_name, secret)
    if player in players:
        return 'ERROR Cannot leave running table ... yet'

    table = tables.get(table_name, None)
    if not table:
        return 'ERROR Table does not exist'

    if player in table.players:
        table.players.remove(player)
    else:
        return 'ERROR Player not in table'

    return 'ACK'

def join_table(usr_name, secret, table_name):
    """Game Logic: JOIN - Used to add new players to an existing table"""
    player = KingPlayer(usr_name, secret)
    if player in players or player in flatten(map(lambda t: t.players, iter(tables.values()))):
        return 'ERROR Player already in table'

    table = tables.get(table_name, None)
    if not table:
        return 'ERROR Table does not exist'

    if not table.join_table(player):
        return 'ERROR Table is already full'

    if table.start():
        # Game ready to start, handle main protocol
        # Quick access to game from player's info
        for player in table.players:
            players[player] = table

        # Send message for players signaling game start, send also players list
        status_publisher.send_string('%s START %s'%(table_name, ' '.join([s.name for s in table.players])))
        start_hand(table)

    return 'ACK'

def start_hand(table):
    """ Messages used in hand start """
    # Distribute cards and inform players the starter and avilable game choices
    starter = table.setup_hand()
    choices = table.possible_hands(starter)
    status_publisher.send_string('%s STARTHAND %s %s'%(table.name, \
                                 starter.name, ' '.join(choices)))

def get_hand(usr_name, secret):
    """Game Logic: GetHand - Used to get a players hand"""
    player = KingPlayer(usr_name, secret)
    table = players.get(player, None)

    if not table:
        return 'ERROR Invalid Player'

    return json.dumps(table.get_player(player).hand)

def get_turn(usr_name, secret):
    """Game Logic: GetTurn - Used to get whose turn this is"""
    player = KingPlayer(usr_name, secret)
    table = players.get(player, None)

    if not table:
        return 'ERROR Invalid Player'

    return table.players[table.turn].name

def choose_game(usr_name, secret, game):
    """Game Logic: ChooseGame - Used to choose the game for the current hand"""
    player = KingPlayer(usr_name, secret)
    table = players.get(player, None)

    if not table:
        return 'ERROR Invalid Player'

    if not table.start_hand(table.get_player(player), game):
        return 'ERROR Invalid action'

    # Inform players the chosen game
    status_publisher.send_string('%s GAME %s'%(table.name, game))

    # Inform the Turn
    inform_turn(table)

    return 'ACK'

def inform_turn(table):
    status_publisher.send_string('%s TURN %s'%(table.name, table.players[table.turn].name))

def play_card(usr_name, secret, card):
    """Game Logic: PlayCard - Handles logic for card playing and turn management"""
    player = KingPlayer(usr_name, secret)
    table = players.get(player, None)

    if not table:
        return 'ERROR Invalid Player'

    if not table.play_card(table.get_player(player), card):
        return 'ERROR Invalid Action'

    # Inform players the card played
    status_publisher.send_string('%s PLAY %s'%(table.name, card))

    # Check for end of round
    rnd_winner = table.end_round()
    if rnd_winner:
        # Check for end of hand
        if table.end_hand():
            # Check for a game over
            if table.end_game():
                status_publisher.send_string('%s GAMEOVER'%(table.name))
                # TODO Setup some logic for table clean up
            else:
                # Sets up new hand
                start_hand(table)

            return 'ACK'

        status_publisher.send_string('%s ENDROUND %s'%(table.name, rnd_winner.name))

    # If we got here, game is on and so we inform the turn
    inform_turn(table)

    return 'ACK'


### ZMQ Initialization ###
ctx = zmq.Context()

action_server = ctx.socket(zmq.REP)
action_server.bind("tcp://127.0.0.1:%d"%port)

status_publisher = ctx.socket(zmq.PUB)
status_publisher.bind("tcp://127.0.0.1:%d"%(port+1))

poller = zmq.Poller()
poller.register(action_server, zmq.POLLIN)

#This is used to hold all possible options for handling
#   protocol messages
handlers = {
    'JOIN': join_table,
    'LEAVE': leave_table,
    'TABLE': create_table,
    'LIST': list_tables,
    'GETHAND': get_hand,
    'GETTURN': get_turn,
    'GAME': choose_game,
    'PLAY': play_card
}

# Main Game Loop
try:
    while True:
        # Wait a few for some message and do it all again
        while pool_req(handlers, 100):
            pass

except KeyboardInterrupt:
    for tbl in tables:
        status_publisher.send_string('%s GAMEOVER'%(tbl))


