from random import shuffle

ref = ['2','3','4','5','6','7','8','9','10','J','Q','K','A']

class BaseGame:        
        def __init__(self):
                self.round = 0
                self.score = 1

        #Check for a final round
        def lastRound(self):
                return self.round == 13

        # Check the round and increment it
        # @param table must have 4 entries and each entry is terminated with its suit
        # @return the pair position of table winner and score
        def playRound(self, table):
                self.round += 1
                table = map( lambda x: (ref.index(x[:-1]),x[-1]), table )
                pivot = table[0]
                return (table.index(max(filter(lambda x : x[-1] == pivot[-1],
                                               table ))),self.score)
                
        # Player must play one card of the same suit that started the table
        # @param table is the current table state
        # @param hand is the player hand
        # @param play is the card player has chose
        def handConstraint(self, table, hand, play):
                if not len(table): return True
                possible = filter( lambda x: x[-1] == table[0][-1], hand )
                return ( not len(possible) ) or ( possible.count(play) )

class KingPlayer:
    def __init__(self, name, queue):
        self.name = name
        self.queue = queue
        self.hand = []
        self.games = []

    def setHand(self, hand):
        self.hand = hand
        return self

    def addGame(self, game):
        self.games.append(game)
        
    # Test if the card played is in the players hand
    def cardInHand(self, card):
        return self.hand.count(card) == 1

    def __eq__(self, other):
        if isinstance(other, KingPlayer):
                return self.name == other.name and self.queue == other.queue
        return False


class KingTable:
        def __init__(self, name):
                self.name = name
                self.players = []
                self.score = []
                self.gameCount = 0
                self.game = None
                self.deck = [ x+y for x in ref for y in ['S','C','H','D'] ]

                self.running = False
                self.partialScore = 4*[0]
                self.table = []
                self.turn = 0
                
        def joinTable(self, player):
                if len(self.players) == 4:
                        return False
                
                self.players.append(player)
                return True

        def numPlayers(self):
                return len(self.players)

        def start(self):
                if len(self.players) == 4:
                        shuffle(self.players)
                        return True

                return False

        def setupHand(self):
                shuffle(self.deck)

                start, stop = 0, 13
                for player in self.players:
                    player.setHand(self.deck[start:stop])
                    start, stop = stop, stop + 13

                self.turn = self.gameCount % 4
                return self.players[self.turn]

        def startHand(self, player, game):
                #check if its time to choose the game
                if self.running:
                        return False
                
                #Check player exists and if it is his turn
                if not self.players.count(player) or \
                   self.players.index(player) != self.turn:
                        return False
                        
                #TODO check if game is possible
                #TODO Positives needs to come with Trample Suit
                self.game = BaseGame() #TODO create correct game based on choice
                self.partialScore = 4*[0]
                self.running = True 
                
                player.addGame(game)
                
                return True

        def playCard(self, player, card):
                if not self.running or self.game.lastRound():
                        return False

                #Check player exists and if it is his turn
                if not self.players.count(player) or self.players.index(player) != self.turn:
                        return False
                
                if not self.game.handConstraint(self.table, player.hand, card):
                        return False

                self.table.append(card)
                player.hand.remove(card)
                self.turn = (self.turn + 1) % 4 
                return True

        def endRound(self):
                if not self.running or len(self.table) < 4:
                        return None

                (winner, pts) = self.game.playRound(self.table)

                self.turn = (self.turn + winner)%4
                self.table = []
                self.partialScore[self.turn] += pts 
                        
                return self.players[self.turn]

        def endHand(self):
                if self.running and self.game.lastRound():
                        self.gameCount += 1
                        self.score.append(self.partialScore)
                        self.running = False
                        return True
                
                return False

        def endGame(self):
                return not self.running and self.gameCount == 10

        def getFinalScore(self):
                return reduce(lambda a,b: map(lambda x,y: x+y, a, b), self.score, 4*[0])
        

if __name__ == "__main__":
        print "Testing simple table evaluation"
        assert(BaseGame().playRound([ '2H', '5H', 'AS', '4H' ]) == (1,1))
        assert(BaseGame().playRound([ '10D', '5C', 'AS', '4H' ]) == (0,1))
        assert(BaseGame().playRound([ '3C', '3H', 'AC', '4H' ]) == (2,1))
        assert(BaseGame().playRound([ '5S', '6S', 'KS', 'AS' ]) == (3,1))

        print "Testing simple hand constraint"
        assert(BaseGame().handConstraint([],['2C','3H'],'3H')) #empty hand
        assert(BaseGame().handConstraint(['2C'],['2H','3C'],'3C')) #play the table naipe
        assert(not BaseGame().handConstraint(['2C'],['2H','3C'],'2H')) #invalid play
        assert(BaseGame().handConstraint(['2C'],['2H','3C','5C','AS','AC'],'5C')) #more than one option
        assert(BaseGame().handConstraint(['AD','JD','QH'],['2C','3C'],'2C')) #discard

        print "Testing playing cards in your hand"
        assert(KingPlayer('t',None).setHand(['2C','4H']).cardInHand('4H'))
        assert(not KingPlayer('t',None).setHand(['3C','5D']).cardInHand('8S'))
        assert(KingPlayer('t',None).setHand(['5H','6S','8D']).cardInHand('6S'))
