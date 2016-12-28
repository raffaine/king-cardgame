var zmq = require('zmq');

var action = zmq.createSocket('req');
var info = zmq.createSocket('sub');

//TODO Error handling on connection

game = { state: "STOPPED"};

game.actions = {
    'STARTHAND': function(params) {
    },
    'TURN': function(params) {
        action.send(`PLAY ${user} ${secret} ${game.hand[0]}`);
        console.log("taking any turn, hahaha ... but won't harm");
    }
}

action.connect('tcp://127.0.0.1:5555');
action.on('message', function(msg) {
    game.response = msg.toString();
});

action.send('LIST');

//This is bogus but due to async programming with events ... but for the bridge there is no need to dwelve into the logic of it.
game.subscribe('');

