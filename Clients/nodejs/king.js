
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
                        show_message("The game has started");
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
    this.hand_area = document.getElementById('playerCards');

    this.setCards = function(cards) {
        this.cards = cards;

        var nodes = this.hand_area;
        cards.slice(0,-1).forEach(function(card){
            var node = (new Card(card[0], card[1])).createNode();
            node.style.transition = "all 3s ease-out";
            nodes.appendChild(node);
        });
        //Keep the last element hidden
        var card = cards.slice(-1)[0];
        var node = (new Card(card[0], card[1])).createNode();
        node.firstChild.style.visibility = 'hidden';
        node.style.transition = "all 3s ease-out";
        nodes.appendChild(node);

        //Show cards
        for (var i = 0; i < nodes.childNodes.length; i++) {
            var effect = `translateX(${2*i}em)`;
            var node = nodes.childNodes[i];
            if (i === nodes.childNodes.length - 1 ) {
                effect += ' rotateY(180deg)';
                node.firstChild.style.transition = "all 3s 0.1s ease-out";
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

    this.playCard = function(index) {
         //TODO: Card must have a way of knowing its index
        var card = this.cards.splice(index, 1);
    }
}

function show_message(message) {
    var box = document.getElementById('infobox');
    var content = document.getElementById('infocontent')
    content.textContent = message;

    box.addEventListener('animationend', function(evt) {
        box.style.animation = "";
    });
    box.style.animation = "showinfo 3s ease-out 0.5s forwards";
}