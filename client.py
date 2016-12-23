import sys
import zmq
import json
import uuid

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

    def list_table(self):
        self.srv.send_string("LIST")
        return self.srv.recv_string()

    def create_table(self, name):
        self.srv.send_string("TABLE %s"%(name))
        return self.srv.recv_string()

    def join_table(self, name):
        self.srv.send_string("JOIN %s %s %s"%(self.usr, self.secret, name))
        return self.srv.recv_string()

    def get_hand(self):
        self.srv.send_string("GETHAND %s %s"%(self.usr, self.secret))
        return self.srv.recv_string()

    def get_turn(self):
        self.srv.send_string("GETTURN %s %s"%(self.usr, self.secret))
        return self.srv.recv_string()

    def resubscribe(self, table):
        self.pubsrv.setsockopt_string(zmq.UNSUBSCRIBE, '')
        self.pubsrv.setsockopt_string(zmq.SUBSCRIBE, table)

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

    def poll_sub(self, handler):
        while self.pubsrv.poll(1):
            _, _, data = (self.pubsrv.recv_string()).partition(' ')
            handler(data)

class Game:
    """ Keeps track of game information """
    def __init__(self, server):
        self.hand = []
        self.players = []
        self.round = 0
        self.turn = 0
        self.table = []
        self.is_over = False
        self.handlers = {k[len('H_'):]:v for k, v in Game.__dict__.items() if k.startswith('H_')}
        self.server = server

    def handle_msg(self, content):
        """ Invokes the handler for the message received """
        print("Handling ", content)
        content = content.split(' ')
        cmd = self.handlers.get(content[0], lambda *args: print("Invalid message"))
        cmd(self, *content[1:])

    def H_START(self, *players):
        """ Handles the start of a new game """
        self.players = list(players)

    def H_CHOOSEGAME(self, player, *choices):
        """ Handles the choice of a game for the hand """
        if self.server.usr == player:
            pass #TODO choose a game

    def H_STARTHAND(self, start_player):
        """ Handles the start of a new hand """
        self.turn = self.players.index(start_player)
        self.hand = json.loads(self.server.get_hand())

    def H_TURN(self, player):
        """ Handles the Turn information """
        if self.server.usr == player:
            pass #TODO choose a card to play

    def H_GAMEOVER(self):
        """ Handles the end of the game """
        self.is_over = True



if __name__ == "__main__":
    usr = ''

    if len(sys.argv) > 1:
        usr = sys.argv[1]

    srv = Server(usr)
    if not srv.hunt_table():
        print("Failed to join a table, exiting client")
        exit()

    game = Game(srv)
    while not game.is_over:
        srv.poll_sub(game.handle_msg)
