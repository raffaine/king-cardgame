
var GameStates = {
    NOT_STARTED: "NOT STARTED",
    PENDING_GAME: "STARTED BUT NO GAME",
    RUNNING: "RUNNING",
    WAITING_GAME: "WAITING FOR GAME",
    WAITING_BID: "WAITING FOR A BID",
    WAITING_DECISION: "WAITING FOR DECISION",
    WAITING_TRAMPLE: "WAITING FOR TRAMPLE SUIT",
    WAITING_PLAY: "WAITING FOR PLAY",
    GAME_OVER: "GAME OVER"
};

function Game(user) {

    this.table = new Table;
    this.user = user;
    this.secret = 'whatever';
    this.hand = new Hand;
    this.state = GameStates.NOT_STARTED;
    this.choices = new Array;
    this.cur_game = '';

    this.sendAction = function(action, params, fnResponse) {
        socket.once('response', fnResponse);
        var cmd = `${action} ${this.user} ${this.secret}`;
        if (params !== '') {
            cmd += ' ' + params;
        }
        socket.emit('action', cmd);
    };

    // Define the set of information that can be received from the server
    this.info = {
                    'START': function(game, players) {
                        game.table.setPlayers(players);
                    },
                    'STARTHAND': function(game, params) {
                        //params[0] is starter
                        //params[1:-1] is list of possible games as strings
                        //TODO: Set the current_turn to point to params[0] table position
                        //THE TIMEOUT IS TO FIX A RACE CONDITION IF YOU ARE THE LAST ONE TO JOIN
                        setTimeout( function() {
                            // Attempt to get the player's hand
                            game.sendAction('GETHAND', '', function(response) {
                                if (!response.startsWith('ERROR')) {
                                    console.log(response);
                                    game.hand.setCards(JSON.parse(response));
                                }
                            });
                        }, 1000);

                        // Set the state of the game according to whom is the starter
                        if(params[0] === game.user) {
                            game.state = GameStates.WAITING_GAME;
                            game.choices = params.slice(1);
                        } else {
                            game.state = GameStates.PENDING_GAME;
                        }
                    },
                    'GAME': function(game, choice) {
                        //Receive info about what are the current rules
                        game.cur_game = choice[0];
                        game.state = GameStates.RUNNING;
                    },
                    'TURN': function(game, player) {
                        //No action unless you are the player
                        if (player[0] == game.user) {
                            game.state = GameStates.WAITING_PLAY;
                        }
                    }
                };

    // Start listening on subscription channel
    socket.game = this;
    socket.on('info', function(message) {
        var args = message.split(' ');
        //TODO: Fix the server to stop sending space at the start (when this happen, change from 1 to 0 and from 2 to 1)
        if (fn = this.game.info[args[1]]) {
            fn(this.game, args.slice(2));
        }
    });
}

function Table() {
    this.cards = new Array;
    this.players = new Array;
    this.current_turn = 0;

    this.addCard = function(card) {
        this.cards.append(card); //TODO: Not sure how to properly animate this stuff
    }

    this.setPlayers = function(players) {
        //TODO: This must be rearranged so Current Player is always on the bottom of the screen and next turn is counter clockwise
        this.players = players;
    }
}

function Hand() {
    this.cards = new Array;

    this.setCards = function(cards) {
        this.cards = cards; //TODO: Create Nodes on Hands area and so
    }

    this.playCard = function(index) {
        this.cards.splice(index, 1); //TODO: Card must have a way of knowing its index
    }
}