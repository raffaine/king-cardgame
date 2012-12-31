# -*- coding: utf-8 -*-
from random import shuffle

_ref = ['2','3','4','5','6','7','8','9','10','J','Q','K','A']

#This class has the basic rule of the game,
#     and the basic table evaluation when the round ends
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
        def playRound(self, table, pivot_index = 0):
                self.round += 1
                table = map( lambda x: (_ref.index(x[:-1]),x[-1]), table )
                pivot = table[pivot_index]
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

# English No Tricks
# Just like the base game, but score is negative 25 each round you make
class Vaza(BaseGame):
        def __init__(self):
                BaseGame.__init__(self)
                self.score = -25

# English No Last 2 Tricks
# Just like the base game, but last 2 rounds worth negative 90 points each
class DuasUltimas(BaseGame):
        def __init__(self):
                BaseGame.__init__(self)
                self.score = 0

        # As BaseGame increments round check last rounds are 11 and 12
        def playRound(self, table):
                if self.round > 10:
                        self.score = -90
                return BaseGame.playRound(self, table)
        
# English No Queens
# Just like the base game, but each queen in table is worth -50
class Mulheres(BaseGame):
        def __init__(self):
                BaseGame.__init__(self)
                self.score = 0

        # Count the number of woman cards in table (Queens),
        # each one is worth -50 points
        def playRound(self, table):
                self.score = -50*len(filter(lambda x: x[0]=='Q',table))
                return BaseGame.playRound(self, table)

# English No Kings or Jacks
# Just like the base game, but each king or jack in table is worth -30
class Homens(BaseGame):
        def __init__(self):
                BaseGame.__init__(self)
                self.score = 0

        # Count the number of man cards in table (Kings and Jacks),
        # each one is worth -30 points
        def playRound(self, table):
                self.score = -30*len(filter(lambda x: ['K','J'].count(x[0]),table))
                return BaseGame.playRound(self, table)

# English No Hearts
# Just like the base game, but each card with hearts suit is worth -20
#       Also you cannot start with Hearts unless you only have Hearts
class Copas(BaseGame):
        def __init__(self):
                BaseGame.__init__(self)
                self.score = 0

        # Count the number of hearts suit cards in table,
        # each one is worth -20 points
        def playRound(self, table):
                self.score = -20*len(filter(lambda x: x[-1] == 'H',table))
                return BaseGame.playRound(self, table)

        # Player cannot play Hearts unless he only have Hearts
        #       or table started with Hearts
        def handConstraint(self, table, hand, play):
                if not len(table) and play[-1] == 'H':
                        return not len(filter(lambda x: x[-1] != 'H',hand))
                return BaseGame.handConstraint(self, table, hand, play)

# English No King of Hearts
# Same hand constraint of No Hearts, but King of Hearts ends the hand and
#   is worth -160 points
class King(BaseGame):
        def __init__(self):
                BaseGame.__init__(self)
                self.score = 0
                self.kingOfHearts = False

        #Check for hand end
        def lastRound(self):
                return self.kingOfHearts or self.round == 13

        # Check for the King of Hearts
        def playRound(self, table):
                self.score = -160*table.count('KH')
                self.kingOfHearts = (self.score == -160)
                return BaseGame.playRound(self, table)

        # Player cannot play Hearts unless he only have Hearts
        #       or table started with Hearts
        # If Player has the King of Hearts and don't have the suit on table,
        #       HE MUST DISCARD THE KING OF HEARTS
        def handConstraint(self, table, hand, play):
                if not len(table):
                        return play[-1] != 'H' or \
                               not len(filter(lambda x: x[-1] != 'H',hand))
                
                possible = filter( lambda x: x[-1] == table[0][-1], hand )
                if not len(possible):
                        return play == 'KH' or not hand.count('KH')
                
                return possible.count(play)

# English Positives
# Just like the base game, where each round score 25
# In adition we have:
#   - Trample Suit: Even when discarded, this suit takes the round
class Positiva(BaseGame):
        def __init__(self, trample = ''):
                BaseGame.__init__(self)
                self.trample = trample
                self.score = 25
                
        # Check for Trample Suit and if found use it index for pivot suit
        def playRound(self, table):
                elem = filter(lambda x: x[-1] == self.trample, table)
                index = table.index(elem[0]) if elem else 0
                return BaseGame.playRound(self, table, index)


_HANDS = \
{
        'VAZA': Vaza,
        '2ULTIMAS': DuasUltimas,
        'MULHERES': Mulheres,
        'HOMENS': Homens,
        'COPAS': Copas,
        'KING': King,
        'POSITIVA': Positiva
}

# Simple class to store player information
# Mainly store players hand and list of choosen hands
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
                self.deck = [ x+y for x in _ref for y in ['S','C','H','D'] ]

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

        def possibleHands(self, player):
                games = _HANDS.keys()
                for p in self.players:
                        played = filter(lambda x: p == player or \
                                         x!='POSITIVA', p.games)
                        for c in played:
                                games.remove(c)
                return games or ['POSITIVA']
                        

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

                game = game.split() or [game]
                #Check if game is possible
                if not _HANDS.has_key(game[0]) or \
                   not self.possibleHands(player).count(game[0]):
                        return False
                
                #Positives could come with Trample Suit
                #Start the game
                self.game = _HANDS[game[0]]() if len(game) == 1 else \
                            _HANDS[game[0]](game[1])
                        
                self.partialScore = 4*[0]
                self.running = True 
                
                player.addGame(game[0])
                
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

        print "Testing game evaluation: Vaza"
        assert(Vaza().playRound([ '2H', '5H', 'AS', '4H' ]) == (1,-25))
        assert(Vaza().playRound([ '10D', '5C', 'AS', '4H' ]) == (0,-25))
        assert(Vaza().playRound([ '3C', '3H', 'AC', '4H' ]) == (2,-25))
        assert(Vaza().playRound([ '5S', '6S', 'KS', 'AS' ]) == (3,-25))

        print "Testing game evaluation: Duas Últimas"
        assert(DuasUltimas().playRound(['5H','KS','AC','10H']) == (3,0))
        du = DuasUltimas()
        du.round = 11
        assert(du.playRound(['5H','QS','JS','2H']) == (0,-90))
        assert(du.playRound(['10C','QS','QC','5H']) == (2,-90))
        
        print "Testing game evaluation: Mulheres"
        assert(Mulheres().playRound(['5H','KS','AC','10H']) == (3,0))
        assert(Mulheres().playRound(['5H','QS','JS','2H']) == (0,-50))
        assert(Mulheres().playRound(['10C','QS','QC','5H']) == (2,-100))
        assert(Mulheres().playRound(['2H','QH','QC','QD']) == (1,-150))
        assert(Mulheres().playRound(['QD','QS','QC','QH']) == (0,-200))

        print "Testing game evaluation: Homens"
        assert(Homens().playRound(['5H','AS','AC','10H']) == (3,0))
        assert(Homens().playRound(['5H','QS','JS','2H']) == (0,-30))
        assert(Homens().playRound(['10C','KS','JC','5H']) == (2,-60))
        assert(Homens().playRound(['2H','KH','JH','KD']) == (1,-90))
        assert(Homens().playRound(['KD','JS','KC','JD']) == (0,-120))

        print "Testing game evaluation: Copas"
        assert(Copas().playRound(['5H','AS','AC','10H']) == (3,-40))
        assert(Copas().playRound(['5D','6H','AD','10C']) == (2,-20))
        assert(Copas().playRound(['5H','10H','AD','2H']) == (1,-60))
        assert(Copas().playRound(['AH','6H','KH','10H']) == (0,-80))
        assert(Copas().playRound(['5D','6S','AD','10C']) == (2,0))

        print "Testing game evaluation: King"
        k = King()
        assert(King().playRound(['5H','AS','AC','10H']) == (3,0))
        assert(not k.lastRound())
        k = King()
        assert(k.playRound(['AH','6C','KH','10H']) == (0,-160)) 
        assert(k.lastRound())

        print "Testing game evaluation: Positiva"
        assert(Positiva().playRound(['5H','AS','AC','10H']) == (3,25)) #no trample
        assert(Positiva('H').playRound(['5D','6H','AD','10D']) == (1,25)) #hearts
        assert(Positiva('C').playRound(['5H','10H','2C','2D']) == (2,25)) #clubs
        assert(Positiva('D').playRound(['AD','6D','KD','10D']) == (0,25)) #diamonds
        assert(Positiva('S').playRound(['5D','6S','AH','10C']) == (1,25)) #spades
        assert(Positiva('H').playRound(['5C','2S','AC','10C']) == (2,25)) #trample but no trample

        print "Testing simple hand constraint"
        assert(BaseGame().handConstraint([],['2C','3H'],'3H')) #empty hand
        assert(BaseGame().handConstraint(['2C'],['2H','3C'],'3C')) #play the table naipe
        assert(not BaseGame().handConstraint(['2C'],['2H','3C'],'2H')) #invalid play
        assert(BaseGame().handConstraint(['2C'],['2H','3C','5C','AS','AC'],'5C')) #more than one option
        assert(BaseGame().handConstraint(['AD','JD','QH'],['2C','3C'],'2C')) #discard
        
        print "Testing game hand constraint: Copas"
        assert(Copas().handConstraint([],['2C','3H'],'2C')) #empty table
        assert(not Copas().handConstraint([],['2C','3H'],'3H')) #wrong start
        assert(Copas().handConstraint(['2H'],['2C','3H'],'3H')) #play the suit
        assert(Copas().handConstraint(['2C'],['2H','3H'],'3H')) #burn on discard
        
        print "Testing game hand constraint King"
        assert(King().handConstraint([],['2C','KH'],'2C')) #empty table
        assert(not King().handConstraint([],['2C','KH'],'KH')) #wrong start
        assert(King().handConstraint(['2H'],['3H','KH'],'3H')) #play the suit
        assert(King().handConstraint(['2C'],['2H','KH','3D'],'KH')) #must discard

        print "Testing playing cards in your hand"
        assert(KingPlayer('t',None).setHand(['2C','4H']).cardInHand('4H'))
        assert(not KingPlayer('t',None).setHand(['3C','5D']).cardInHand('8S'))
        assert(KingPlayer('t',None).setHand(['5H','6S','8D']).cardInHand('6S'))

        print "Checking for Hand Instances"
        assert(isinstance(_HANDS['COPAS'](), Copas))

        print "Checking Possible Hands"
        k = KingTable('')
        for i in range(4):
                k.joinTable(KingPlayer(str(i),''))

        valid = _HANDS.keys()
        assert( k.possibleHands(k.players[0]) == valid )
        k.players[0].games.append(valid.pop(valid.index('COPAS')))        
        assert( k.possibleHands(k.players[0]) == valid )
        k.players[0].games.append(valid.pop(valid.index('KING')))       
        assert( k.possibleHands(k.players[0]) == valid )
        k.players[0].games.append(valid.pop(valid.index('2ULTIMAS')))       
        assert( k.possibleHands(k.players[0]) == valid )
        k.players[0].games.append(valid.pop(valid.index('POSITIVA')))
        assert( k.possibleHands(k.players[0]) == valid )       
        assert( k.possibleHands(k.players[0]).count('POSITIVA') == 0)
        k.players[0].games.append(valid.pop(valid.index('HOMENS')))       
        assert( k.possibleHands(k.players[0]) == valid )
        k.players[0].games.append(valid.pop(valid.index('MULHERES')))       
        assert( k.possibleHands(k.players[0]) == valid )
        k.players[0].games.append(valid.pop(valid.index('VAZA')))
        assert( k.possibleHands(k.players[0]).count('POSITIVA') == 1)
        k.players[0] = KingPlayer('','')
        
