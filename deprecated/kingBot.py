# -*- coding: utf-8 -*-
import uuid

from kingClient import *

def handTakeAny(table, hand):
    suit = table[0][-1] if len(table) else ''
    possible = filter( lambda x: x[-1] == suit, hand )
    return random.sample(hand,1)[0] \
           if ( not len(possible) ) else random.choice(possible)

class BasicBot(Game):
    def __init__(self):
        Game.__init__(self)
    
    def getInput(self, action='', *args):
        if action == 'CARD':
            return handTakeAny(args[0], args[1]) #take any possible card
        elif action == 'NAME':
            return sys.argv[1] if len(sys.argv) > 1 else str(uuid.uuid4())
        elif action == 'TABLE':
            return '0'
        elif action == 'HAND':
            return random.choice(self.possibleHands) #take any possible game
        else:
            raise NotImplementedError('Action not recognized')
    
    def endRound(self, message):
        Game.endRound(self, message)
        #Useful to decide some action based on round winning

    def endHand(self, message):
        Game.endHand(self, message)
        #Useful to decide some action based on hand winning

    def start(self, p_name = None, p_table = None):
        if not p_name or not p_table:
            Game.start(self, True)
            return
        
        self.name = p_name
        self.createTable(p_table)
        
        try:
           self.channel.start_consuming()
        except KeyboardInterrupt:
            log.info("Quiting Game")
            self.publishTable('quit')        
        except:
            log.info("Unexpected error: %s"%(sys.exc_info()[0]))
            raise

if __name__ == "__main__":
    log.basicConfig(stream = sys.stdout, level = log.INFO)
    cGame = BasicBot()
    if len(sys.argv) < 3:
        cGame.start()
    else:
        cGame.start(sys.argv[1], sys.argv[2])
    







