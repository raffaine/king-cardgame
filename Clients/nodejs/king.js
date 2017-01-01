
var TranslateChoices = {
    '0': 'Forefeit',
    '1': '1 Trick',
    '2': '2 Tricks',
    '3': '3 Tricks',
    '4': '4 Tricks',
    '5': '5 Tricks',
    'C': 'Clubs',
    'S': 'Spades',
    'H': 'Hearts',
    'D': 'Diamonds',
    ' ': 'No Trample',
    'COPAS': 'No Hearts',
    'VAZA': 'No Tricks', 
    'MULHERES': 'No Queens',
    'POSITIVA': 'Positive',
    'HOMENS': 'No Jacks and Kings',
    'KING': 'No King of Hearts',
    '2ULTIMAS': 'No Last 2 Tricks',
    'True': 'Yes, I accept',
    'False': 'No, I will choose'
}

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

    // Helper function used to generate option UI to the user
    this.createChoiceBox = function(title, body, choices, action) {
        var e = $("<div id='choiceArea'></div>").appendTo($('body'))
                .append(`<h2>${title}</h2>`)
                .append(`<span>${body}</span>`)
                .append('<br />');

        for (var i=0; i < choices.length; i++) {
            e.append(
                $('<button></button>')
                .text(TranslateChoices[choices[i]])
                .on('click', function(game, choice){
                    return function(evt) {
                        game.sendAction(action, choice, function(msg) {
                            console.log(`${action} with ${choice} selected, answer: ${msg}`);
                            $('#choiceArea').remove();
                        });
                    };
                }(this, choices[i])));
        }

        e.addClass('active');
    }

    // Show user what hand options are available and get the choice
    this.chooseGame = function(choices) {
        this.createChoiceBox("New Hand, what's the game?",
                        "These are the choices, click the one you want to play.",
                        choices, 'GAME');
    };

    // Show user UI so he can select what is his bids
    this.getBid = function() {
        //TODO Fill the blanks and generate choices based on current max bid
        this.createChoiceBox("You're the current man on, how much will you give?",
                        "The best here would be a story about the bidding.",
                        "012345".split(''), 'BID');
    };

    // Show user UI so he can decide if he accepts or not the winning bid
    this.getDecision = function() {
        //TODO: Fill the blanks
        this.createChoiceBox("We have a winning bid, do you take it?",
                        "Player X offered Y tricks for the choice.",
                        ['True', 'False'], 'DECIDE');
    };

    // Show user UI so he can decide what is the trample suit
    this.getTrample = function() {
        this.createChoiceBox("You've got the choice!",
                        "Choose one of the suits as the trample suit.",
                        "CDHS ".split(''), 'TRAMPLE');
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

        // Create cards and set animation
        for (var i = 0; i < cards.length; i++) {
            var card = cards[i];
            var node = (new Card(card[0], card[1])).createNode();
            node.onclick = this.createClickCallback(card);
            $(node).playKeyframe(`handCard${i} 3s forwards`);
            //node.style.animation = `handCard${i} 3s forwards`
            
            this.hand_area.appendChild(node);
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
    var box = document.getElementById('infobox');

    if (box.classList.contains('animated')) {
        setTimeout(function() {
            show_message(message);
        }, 500);
    } else {
        var content = document.getElementById('infocontent')
        content.textContent = message;

        box.addEventListener('animationend', function(evt) {
            box.style.animation = "";
            box.classList.remove('animated');
        });
        box.classList.add('animated');
        box.style.animation = "showinfo 3s ease-out 0.5s forwards";
    }
}