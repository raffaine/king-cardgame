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

    def bid(self, value):
        self.srv.send_string("BID %s %s %d"%(self.usr, self.secret, value))
        return self.srv.recv_string()

    def decide(self, value):
        self.srv.send_string("DECIDE %s %s %s"%(self.usr, self.secret, value))
        return self.srv.recv_string()

    def choose_trample(self, value):
        self.srv.send_string("TRAMPLE %s %s %s"%(self.usr, self.secret, value))
        return self.srv.recv_string()

    def play_card(self, card):
        self.srv.send_string("PLAY %s %s %s"%(self.usr, self.secret, card))
        return self.srv.recv_string()

class GamePlayer:
    """ Interface for Game Playing callbacks """
    game = None

    def set_game(self, game):
        """ Game is the current game state """
        self.game = game
        return self

    def choose_game(self, choices):
        """ Return one of the choices given """
        pass

    def bid(self):
        """ Callback to collect ammount player wants to bid """
        pass

    def inform_bid(self):
        """ Callback to inform last bid in the auction """
        pass

    def decide(self):
        """ Callback to get decision if player is the auctioner """
        pass

    def choose_trample(self):
        """ Callback to get trample suit if player won the bid (or is the auctioner and declined)"""
        pass

    def game_selected(self):
        """ Callback after game is set on the table """
        pass

    def play_card(self):
        """ Return one of the cards in self.game.hand (Plays that card) """
        pass

    def card_played(self):
        """ Callback after card is played on the table """
        pass

    def end_round(self, winner):
        """ Callback after round is ended, winner is player that wins the round """
        pass

    def end_hand(self):
        """ Callback after hand is over """
        pass

class Game:
    """ Keeps track of game information """
    def __init__(self, server, game_player):
        self.hand = []
        self.players = []
        self.round = 0
        self.turn = ''
        self.table = []
        self.is_over = False
        self.game = ''
        self.bidder = ''
        self.max_bid = 0
        self.max_bidder = ''
        self.trample = ''

        self.handlers = {k[len('H_'):]:v for k, v in Game.__dict__.items() if k.startswith('H_')}
        self.server = server
        self.player = game_player.set_game(self)

    def handle_msg(self, content):
        """ Invokes the handler for the message received """
        print("Handling ", content)
        content = content.split(' ')
        cmd = self.handlers.get(content[0], lambda *args: print("Invalid message"))
        cmd(self, *content[1:])

    def H_START(self, *players):
        """ Handles the start of a new game """
        self.players = list(players)

    def H_BID(self, bidder):
        """ Handles the bid turn """
        self.bidder = bidder
        if self.bidder == self.server.usr:
            msg = 'ERROR'
            while msg.startswith('ERROR'):
                msg = self.server.bid(self.player.bid())

    def H_BIDS(self, value):
        """ Handles the bidding value info """
        self.max_bid = int(value) # I can assume that server only inform valid bids
        self.max_bidder = self.bidder
        self.player.inform_bid()

    def H_DECIDE(self, player):
        """ Handles the auctioner decision """
        if player == self.server.usr:
            msg = 'ERROR'
            while msg.startswith('ERROR'):
                msg = self.server.decide(self.player.decide())

    def H_CHOOSETRAMPLE(self, player):
        """ Handles the trample suit choice """
        if player == self.server.usr:
            msg = 'ERROR'
            while msg.startswith('ERROR'):
                msg = self.server.choose_trample(self.player.choose_trample())

    def H_STARTHAND(self, start_player, *choices):
        """ Handles the start of a new hand """
        self.turn = start_player
        self.hand = json.loads(self.server.get_hand())
        self.max_bid = 0

        # Use choice function if its user's turn
        if self.server.usr == start_player:
            msg = 'ERROR'
            while msg.startswith('ERROR'):
                msg = self.server.choose_game(self.player.choose_game(list(choices)))

    def H_GAME(self, game, *trample):
        """ Handles the selection of a game for the hand """
        self.game = game
        self.trample = '' if not trample else trample[0]
        self.player.game_selected()

    def H_TURN(self, player):
        """ Handles the Turn information """
        self.turn = player
        if self.server.usr == player:
            msg = 'ERROR'
            card = ''
            while msg.startswith('ERROR'):
                card = self.player.play_card()
                msg = self.server.play_card(card)

            self.hand.remove(card)

    def H_PLAY(self, card):
        """ Handles the Card playing information """
        self.table.append(card)
        self.player.card_played()

    def H_ENDROUND(self, winner):
        """ Handles the end of a round """
        self.table.clear()
        self.player.end_round(winner)

    def H_ENDHAND(self):
        """ Handles the end of a round """
        self.player.end_hand()

    def H_GAMEOVER(self):
        """ Handles the end of the game """
        self.is_over = True

def run(user, game_player, table_decision_fn=lambda s: s.hunt_table()):
    """ Helper function that establishes a table and run the game loop  """
    srv = Server(user)
    if not table_decision_fn(srv):
        print("Failed to join a table, exiting client")
        return False

    game = Game(srv, game_player)
    while not game.is_over:
        srv.poll_sub(game.handle_msg)

    return True

if __name__ == "__main__":
    usr = ''

    if len(sys.argv) > 1:
        usr = sys.argv[1]

    run(usr, GamePlayer())
