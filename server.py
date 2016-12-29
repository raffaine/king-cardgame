#!/usr/bin/env python3
import atexit
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

def close_table(table_name):
    """ Helper function for table removal """
    # Remove table from tables
    table = tables.pop(table_name)
    # Optionally I can have a different message for game aborted
    status_publisher.send_string('%s GAMEOVER'%(table.name))
    # Remove players from quick access dict
    for player in table.players:
        players.pop(player, None)

def list_tables():
    """Manager Logic: LIST - List all available tables"""
    return json.dumps(list(map(lambda item: item[0], \
                           filter(lambda item: len(item[1].players) < 4, \
                           tables.items()))))

def leave_table(usr_name, secret):
    """Game Logic: LEAVE - Used to remove players from a table"""
    player = KingPlayer(usr_name, secret)
    table = players.pop(player, None)

    # Remove player from table and abort if it was running
    if table and table.state is not GameState.NOT_STARTED:
        close_table(table.name)
    elif table:
        table.players.remove(player)
    else:
        return 'ERROR Player not in table'

    return 'ACK'

def join_table(usr_name, secret, table_name):
    """Game Logic: JOIN - Used to add new players to an existing table"""
    player = KingPlayer(usr_name, secret)
    if player in players:
        return 'ERROR Player already in table'

    table = tables.get(table_name, None)
    if not table:
        return 'ERROR Table does not exist'

    if not table.join_table(player):
        return 'ERROR Table is already full'

    # Quick access to game from player's info
    players[player] = table

    # If game ready to start, handles main protocol
    if table.start():
        # Send message for players signaling game start, send also players list
        status_publisher.send_string('%s START %s'%(table_name, \
                                                    ' '.join([s.name for s in table.players])))
        # Send info regarding the start of a hand
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

def inform_turn(table):
    status_publisher.send_string('%s TURN %s'%(table.name, table.players[table.turn].name))

def choose_game(usr_name, secret, game):
    """Game Logic: ChooseGame - Used to choose the game for the current hand"""
    player = KingPlayer(usr_name, secret)
    table = players.get(player, None)

    if not table:
        return 'ERROR Invalid Player'

    # Positivas start a biding process
    if game == str(Positiva()) and table.start_positiva(player):
        status_publisher.send_string('%s BID %s'%(table.name, table.players[table.bid_turn].name))
        return 'ACK'

    if not table.start_hand(table.get_player(player), game):
        return 'ERROR Invalid action'

    # Inform players the chosen game
    status_publisher.send_string('%s GAME %s'%(table.name, game))

    # Inform the Turn
    inform_turn(table)

    return 'ACK'

def get_bid(usr_name, secret, value):
    """ Game Logic: Bid - Handles player bid """
    player = KingPlayer(usr_name, secret)
    table = players.get(player, None)

    if not table:
        return 'ERROR Invalid Player'

    try:
        if not table.bid(player, int(value)):
            raise ValueError
    except ValueError:
        return 'ERROR Invalid Action'

    status_publisher.send_string('%s BIDS %s'%(table.name, value))

    # Check for decision time
    if table.state is GameState.DECIDE_BID:
        status_publisher.send_string('%s DECIDE %s'%(table.name, table.players[table.turn].name))
    elif table.state is GameState.CHOOSE_TRAMPLE:
        status_publisher.send_string('%s CHOOSETRAMPLE %s'%(table.name, \
                                                            table.players[table.turn].name))
    else:
        status_publisher.send_string('%s BID %s'%(table.name, table.players[table.bid_turn].name))

    return 'ACK'

def get_decision(usr_name, secret, decision):
    """ Game Logic: Bid - Handles decision to take or not the winning bid """
    player = KingPlayer(usr_name, secret)
    table = players.get(player, None)

    if not table:
        return 'ERROR Invalid Player'

    bid_winner = table.decide(player, decision == 'True')
    if not bid_winner:
        return 'ERROR Invalid Action'

    status_publisher.send_string('%s CHOOSETRAMPLE %s'%(table.name, bid_winner))

    return 'ACK'

def get_trample(usr_name, secret, *trample):
    player = KingPlayer(usr_name, secret)
    table = players.get(player, None)

    if not table:
        return 'ERROR Invalid Player'

    if not table.choose_trample(player, trample[0] if trample else ''):
        return 'ERROR Invalid action'

    # Inform players the chosen game
    game_str = '%s GAME %s'%(table.name, str(Positiva()))
    if trample:
        game_str += ' %s'%(trample[0])
    status_publisher.send_string(game_str)

    # Inform the Turn
    inform_turn(table)

    return 'ACK'


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
        status_publisher.send_string('%s ENDROUND %s'%(table.name, rnd_winner.name))
        # Check for end of hand
        if table.end_hand():
            #TODO Inform scores on Hand Over (Partial Hand Score) and Game Over (Full Game Score)
            status_publisher.send_string('%s ENDHAND'%(table.name))
            # Check for a game over
            if table.end_game():
                close_table(table.name)
            else:
                # Sets up new hand
                start_hand(table)

            return 'ACK'

    # If we got here, game is on and so we inform the turn
    inform_turn(table)

    return 'ACK'

def cleanup(_tables):
    for tbl in _tables:
        status_publisher.send_string('%s GAMEOVER'%(tbl))

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
    'BID': get_bid,
    'DECIDE': get_decision,
    'TRAMPLE': get_trample,
    'PLAY': play_card
}

atexit.register(cleanup, tables)

try:
    # Main Game Loop
    while True:
        # Wait a few for some message and do it all again
        pool_req(handlers, 100)
except KeyboardInterrupt:
    pass
