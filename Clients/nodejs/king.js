
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

var PlayerPosition = {
    BOTTOM: 0,
    LEFT: 1,
    TOP: 2,
    RIGHT: 3,
    CAPACITY: 4
};

function Game(user) {

    this.table = new Table(user);
    this.user = user;
    this.secret = 'whatever'; //TODO: I think it's obvious
    this.hand = new Hand(this);
    this.state = GameStates.NOT_STARTED;
    this.choices = new Array;
    this.cur_game = '';

    this.playCard = function(card, player) {
        var position = this.table.players[player];
        if ( position === PlayerPosition.BOTTOM) {
            this.hand.playCard(card);
        }
    }

    // Generic function used to Send some action to server
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
                        show_message("The game has started");
                        game.table.setPlayers(players);
                    },
                    'STARTHAND': function(game, params) {
                        //params[0] is starter, params[1:-1] is list of possible games as strings
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
                        show_message("The game is " + choice);
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
        // Strip the message header and use it to call the proper handle
        if (fn = this.game.info[args[0]]) {
            fn(this.game, args.slice(1));
        }
    });
}

function Table(user) {
    this.cards = new Array;
    this.players = {};
    this.user = user;
    this.current_turn = 0;
    this.hand_area = document.getElementById('tableCards');

    this.setPlayers = function(players) {
        // This must be rearranged so Current Player is always on
        // the bottom of the screen and next turn is counter clockwise
        var ind = players.indexOf(this.user);
        if (ind === -1) {
            // This should never happen ... user is not part of the table
            return;
        }
        for(var i = PlayerPosition.BOTTOM; i < PlayerPosition.CAPACITY; i++ ) {
            var pos = (ind + i) % PlayerPosition.CAPACITY;
            this.players[players[pos]] = i;
        }
    };

    this.setStarter = function(starter) {
        this.current_turn = this.players[starter];this.hand_area
    };
    
    this.addCard = function(card) {
        this.cards.append(card); //TODO: Not sure how to properly animate this stuff
        this.current_turn = (this.current_turn + 1) % 4;
    };
}

function Hand(game) {
    this.cards = new Array;
    this.hand_area = document.getElementById('playerCards');

    this.createClickCallback = function(card) {
        return function(evt) {
            if (game.state === GameStates.WAITING_PLAY) {
                game.playCard(card, game.user);
            }
        };
    }

    this.setCards = function(cards) {
        this.cards = cards;

        // Create cards
        for (var i = 0; i < cards.length; i++) {
            var card = cards[i];
            var node = (new Card(card[0], card[1])).createNode();
            node.onclick = this.createClickCallback(card);
            node.style.transition = "all 3s ease-out";
            if (i == cards.length - 1) {
                node.firstChild.style.visibility = 'hidden';
                node.firstChild.style.transition = "all 3s 0.1s ease-out";
            }
            
            this.hand_area.appendChild(node);
        }

        // Show cards using animation
        for (var i = 0; i < this.hand_area.childNodes.length; i++) {
            var effect = `translateX(${2*i}em)`; //TODO: Make it proportional to the available area
            var node = this.hand_area.childNodes[i];
            if (i === this.hand_area.childNodes.length - 1 ) {
                effect += ' rotateY(180deg)';
                node.firstChild.style.visibility = "visible";
            }
	        node.style.transform = effect;
        }
    }

    this.emptyHand = function() {
        while(this.hand_area.hasChildNodes()) {
            this.hand_area.removeChild(this.hand_area.lastChild);
        }
    }

    this.playCard = function(card) {
        var index = this.cards.indexOf(card);
        
        if (index !== -1) {
            this.cards.splice(index, 1);
            var node = this.hand_area.childNodes[index];
            this.hand_area.removeChild(node);
        }
    }
}

function hunt_table(game) {
    var fnJoinAny = function(msg) {
        if (msg.startsWith('ERROR')) {
            show_message("FAILED TO JOIN TABLE");
            return;
        }

        var tables = JSON.parse(msg);
        if (tables.length > 0) {
            game.sendAction('JOIN', tables[0], function(msg) {
                console.log(msg); //TODO: Once better control of animations, I can show_message here
            });
        } else {
            // TODO: I'm ignoring response here, since I don't really care
            socket.once('response', function(msg){
                socket.once('response', fnJoinAny);
                socket.emit('action', 'LIST');
            });
            // TODO: I should really add support for server handling anonymous tables
            // TODO: Alternative: http://stackoverflow.com/questions/105034/create-guid-uuid-in-javascript
            socket.emit('action', 'TABLE jsTable');
        }
    };

    socket.once('response', fnJoinAny);
    socket.emit('action', 'LIST');
}

function show_message(message) {
    //TODO: Detect animation in progress and put next request on setTimeout
    var box = document.getElementById('infobox');
    var content = document.getElementById('infocontent')
    content.textContent = message;

    box.addEventListener('animationend', function(evt) {
        box.style.animation = "";
    });
    box.style.animation = "showinfo 3s ease-out 0.5s forwards";
}