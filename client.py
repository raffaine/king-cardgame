"""
 Module client:
  Implements basic infrastructure for a client to the King game server
"""

import sys
import json
import uuid
import zmq

class Server:
    def __init__(self, usr):
        self.usr = usr
        self.table = ''
        self.secret = str(uuid.uuid4())

        ### ZMQ Initialization ###
        ctx = zmq.Context()

        self.pubsrv = ctx.socket(zmq.SUB)
        self.pubsrv.connect('tcp://127.0.0.1:5556')
        self.pubsrv.setsockopt_string(zmq.SUBSCRIBE, '')

        self.srv = ctx.socket(zmq.REQ)
        self.srv.connect("tcp://127.0.0.1:5555")
        
    def resubscribe(self, table):
        self.pubsrv.setsockopt_string(zmq.UNSUBSCRIBE, '')
        self.pubsrv.setsockopt_string(zmq.SUBSCRIBE, table)

    def poll_sub(self, handler):
        while self.pubsrv.poll(1):
            _, _, data = (self.pubsrv.recv_string()).partition(' ')
            handler(data)

    def list_table(self):
        self.srv.send_string("LIST")
        return self.srv.recv_string()

    def create_table(self, name):
        self.srv.send_string("TABLE %s"%(name))
        return self.srv.recv_string()

    def join_table(self, name):
        self.srv.send_string("JOIN %s %s %s"%(self.usr, self.secret, name))
        return self.srv.recv_string()

    def hunt_table(self):
        while not self.table:
            msg = self.list_table()
            if msg.startswith('ERROR'):
                msg = '[]'

            tables = json.loads(msg)

            while tables:
                tmp = tables.pop()
                self.resubscribe(tmp)

                msg = self.join_table(tmp)
                if not msg.startswith('ERROR'):
                    self.table = tmp
                    return True

            msg = self.create_table(str(uuid.uuid4()))
            if msg.startswith('ERROR'):
                self.resubscribe('')
                return False

    def get_turn(self):
        self.srv.send_string("GETTURN %s %s"%(self.usr, self.secret))
        return self.srv.recv_string()

    def get_hand(self):
        self.srv.send_string("GETHAND %s %s"%(self.usr, self.secret))
        return self.srv.recv_string()

    def choose_game(self, game):
        self.srv.send_string("GAME %s %s %s"%(self.usr, self.secret, game))
        return self.srv.recv_string()

    def play_card(self, card):
        self.srv.send_string("PLAY %s %s %s"%(self.usr, self.secret, card))
        return self.srv.recv_string()


class Game:
    """ Keeps track of game information """
    def __init__(self, server, choice_fn, play_fn):
        self.hand = []
        self.players = []
        self.round = 0
        self.turn = 0
        self.table = []
        self.is_over = False
        self.game = ''

        self.handlers = {k[len('H_'):]:v for k, v in Game.__dict__.items() if k.startswith('H_')}
        self.server = server
        self.choice_fn = choice_fn
        self.play_fn = play_fn

    def handle_msg(self, content):
        """ Invokes the handler for the message received """
        print("Handling ", content)
        content = content.split(' ')
        cmd = self.handlers.get(content[0], lambda *args: print("Invalid message"))
        cmd(self, *content[1:])

    def H_START(self, *players):
        """ Handles the start of a new game """
        self.players = list(players)

    def H_STARTHAND(self, start_player, *choices):
        """ Handles the start of a new hand """
        self.turn = self.players.index(start_player)
        self.hand = json.loads(self.server.get_hand())

        # Use choice function if its user's turn
        if self.server.usr == start_player:
            msg = 'ERROR'
            while msg.startswith('ERROR'):
                msg = self.server.choose_game(self.choice_fn(self, list(choices)))

    def H_GAME(self, game):
        """ Handles the selection of a game for the hand """
        self.game = game

    def H_TURN(self, player):
        """ Handles the Turn information """
        if self.server.usr == player:
            msg = 'ERROR'
            card = ''
            while msg.startswith('ERROR'):
                card = self.play_fn(self)
                msg = self.server.play_card(card)

            self.hand.remove(card)

    def H_PLAY(self, card):
        """ Handles the Card playing information """
        self.table.append(card)

    def H_ENDROUND(self, winner):
        """ Handles the end of a round """
        self.table.clear()

    def H_GAMEOVER(self):
        """ Handles the end of the game """
        self.is_over = True

def run(user, choice_fn, play_fn):
    """ Helper function that establishes a table and run the game loop  """
    srv = Server(user)
    if not srv.hunt_table():
        print("Failed to join a table, exiting client")
        return False

    game = Game(srv, choice_fn, play_fn)
    while not game.is_over:
        srv.poll_sub(game.handle_msg)

    return True

if __name__ == "__main__":
    usr = ''

    if len(sys.argv) > 1:
        usr = sys.argv[1]

    def nop(*args):
        pass

    run(usr, nop, nop)