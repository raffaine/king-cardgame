
var GameStates = {
    NOT_STARTED: "NOT STARTED",
    PENDING_GAME: "STARTED BUT NO GAME",
    RUNNING: "RUNNING",
    ROUND_ENDING: "ENDING THE ROUND",
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
    this.cur_game = '';
    this.turn = '';

    // Receives the command to play a card from player
    // Only plays if it's player's turn
    this.play = function(card) {
        if (this.state === GameStates.WAITING_PLAY) {
            this.sendAction('PLAY', card, function(msg){
                console.log('Played card ' + card + "\nAnswer: " + msg);
            });
        }
    };

    // Used to process all card playing on the table
    this.playCard = function(card) {
        var position = this.table.players[this.turn];
        if ( position === PlayerPosition.BOTTOM) {
            this.hand.playCard(card);
        }

        this.table.addCard(card);
    };

    // Show user what hand options are available and get the choice
    this.chooseGame = function(choices) {
        //TODO: Present choices to user in an overlay screen
        var num = Math.random() * choices.length | 0;
        this.sendAction('GAME', choices[num], function(msg) {
            console.log('Game has been selected, answer: ' + msg);
        });
    };

    // Show user UI so he can select what is his bids
    this.getBid = function() {
        //TODO: Present choice to user, now I'm just forefeiting
        this.sendAction('BID', '0', function(msg) {
            console.log('Player forefeits the bid, answer: ' + msg);
        });
    };

    // Show user UI so he can decide if he accepts or not the winning bid
    this.getDecision = function() {
        //TODO: Present choice to user, now I'm just refusing all
        this.sendAction('DECIDE', 'False', function(msg) {
            console.log('Player refuses the winning bid, answer: ' + msg);
        });
    };

    // Show user UI so he can decide what is the trample suit
    this.getTrample = function() {
        //TODO: Present choice to user, now I'm just choosing Clubs
        this.sendAction('TRAMPLE', 'C', function(msg) {
            console.log('Player chooses clubs, answer: ' + msg);
        });
    };

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
                //TODO: Handle ROUND_ENDING ...
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
                game.table.setStarter(params[0]);
                if(params[0] === game.user) {
                    //game.state = GameStates.WAITING_GAME;
                    game.chooseGame(params.slice(1));

                } /*else {
                    game.state = GameStates.PENDING_GAME;
                }*/
            },
            'GAME': function(game, choice) {
                //Receive info about what are the current rules
                if (game.state === GameStates.ROUND_ENDING) {
                    setTimeout(function() {
                        game.info['GAME'](game, choice);
                    }, 1000);
                } else {
                    game.cur_game = choice[0];
                    game.state = GameStates.RUNNING;
                    show_message("The game is " + choice);
                }
            },
            'TURN': function(game, player) {
                //No action unless you are the player
                if (game.state === GameStates.ROUND_ENDING) {
                    setTimeout(function() {
                        game.info['TURN'](game, player);
                    }, 1000);
                } else {
                    game.turn = player;
                    if (player[0] == game.user) {
                        game.state = GameStates.WAITING_PLAY;
                    } else {
                        game.state = GameStates.RUNNING;
                    }
                }
            },
            'PLAY': function(game, card) {
                if (game.state === GameStates.ROUND_ENDING) {
                    setTimeout(function() {
                        game.info['PLAY'](game, card);
                    }, 1000);
                } else {
                    game.playCard(card[0]);
                }
            },
            'ENDROUND': function(game, winner) {
                game.state = GameStates.ROUND_ENDING;
                setTimeout( function() {
                    game.table.endRound(winner[0]);
                    game.state = GameStates.RUNNING;
                }, 1500);
            },
            'ENDHAND': function(game, empty) {
                //TODO: I'm planning to return the hand score here
            },
            'GAMEOVER': function(game, empty) {
                //TODO: I'm planning to return the final score here
            },
            'BID': function(game, player) {
                //TODO: Setup screen for bidding (on else collect who is bidding now)
                if (player[0] === game.user) {
                    game.state = GameStates.WAITING_BID;
                    game.getBid();
                }
            },
            'BIDS': function(game, value) {
                //TODO: I need to store the current bidder to process the bids
            },
            'DECIDE': function(game, player) {
                // Only action is if I'm the one that needs to decide
                if (player[0] === game.user) {
                    game.state = GameStates.WAITING_DECISION;
                    game.getDecision();
                }
            },
            'CHOOSETRAMPLE': function(game, player) {
                // Only action is if I'm the one that needs to decide
                if (player[0] === game.user) {
                    game.state = GameStates.WAITING_TRAMPLE;
                    game.getTrample();
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
    this.area = document.getElementById('tableCards');

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
        this.current_turn = this.players[starter];
    };
    
    this.addCard = function(card) {
        this.cards.push(card);
        //TODO: Better animation, the card must start from the side
        //      where the player that played it is
        //      It must go to center after it was added to the right area
        var node = (new Card(card[0],card[1])).createNode();
        node.style.transition = "all 3s ease-out";
        this.area.appendChild(node);

        node.style.transform = `translateX(${this.current_turn * 3}em)`;

        this.current_turn = (this.current_turn + 1) % PlayerPosition.CAPACITY;
    };

    this.endRound = function(winner) {
        //TODO: Animate cards going to winners side
        this.cards = new Array;
        
        while(this.area.hasChildNodes()) {
            this.area.removeChild(this.area.lastChild);
        }

        this.setStarter(winner);
    };
}

function Hand(game) {
    this.cards = new Array;
    this.hand_area = document.getElementById('playerCards');

    this.createClickCallback = function(card) {
        return function(evt) {
            game.play(card);
        };
    }

    this.setCards = function(cards) {
        this.cards = cards;
        this.emptyHand();

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
        //TODO: Animate card when leaving hand
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
            // I'm ignoring response here, since I don't really care
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