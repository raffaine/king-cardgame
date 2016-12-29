// Include required modules and initialize them
var zmq = require('zmq');
var express = require('express');
var app = express();
var server = require('http').createServer(app);
var io = require('socket.io')(server);

// Initialize the two main sockets (REQ/REP and PUB/SUB) for ZeroMQ
// TODO: Error Handling on connection failure
// TODO: Load URL from config file
var action = zmq.createSocket('req');
action.connect('tcp://127.0.0.1:5555');
var info = zmq.createSocket('sub');
info.connect('tcp://127.0.0.1:5556');

// Subscribe to everything since we handle filters with socket.io rooms
info.subscribe('');

// Setup Subscribe channel handler (very simple, just filter based on rooms)
info.on('message', function(data) {
    // data always follows format TABLE MSG *CONTENTS (contents cardinality is 0..*)
    var args = data.toString().split(' ', 1);
    // TODO: Remove table from message before passing it to client
    io.to(args[0]).emit('info', data.toString());
});

// TODO: Game state is not handled by the server, this should be on client
var game = {
    actions: {
        'STARTHAND': function(params) {
        },
        'TURN': function(params) {
            action.send(`PLAY ${user} ${secret} ${game.hand[0]}`);
            console.log("taking any turn, hahaha ... but won't harm");
        }
    }
}

// Set up client specific behavior during handle of client connection
io.on('connection', function(client){
    console.log(`Client ${client.id} connected ...`);

    // Setup handler for client disconnection
    client.on('disconnect', function(){
        // TODO: Try to identify by name and handle table early exit (LEAVE msg on ZMQ)
        console.log('Client has disconnected.');
    });

    // Handle action messages (REQ/REP)
    client.on('action', function(data) {
        // Here is where ZMQ and Socket.io talk with each other
        console.log(`${client.id} requested ${data}`);

        // SPECIAL HANDLING OF MESSAGE JOIN, I need to join room before knowing it was successfull
        var arr = data.split(' ', 2);
        // User requested a join, if answer is 'ACK', it was successfull
        if (arr.length > 1 && arr[0] == 'JOIN') {
            // Handles special situation where player joins a table (successfully)
            // We temporarily join room so we don't miss any events on that table
            client.table = arr[1];
            client.join(client.table);
            console.log(`${client.id} about joins ${client.table}`);
        }
        // TODO: Handle leaving the table ... client.leave(table);

        // Setup an event listener to handle the ZMQ server response
        action.once('message', function(response) {
            console.log(`Server response was ${response}`);
            if (client.table) {
                // Leave Socket.io room if answer was not ACK
                if (response.toString() !== 'ACK') {
                    console.log(`${client.id} leaves ${client.table}`);
                    client.leave(client.table);
                }

                delete client.table;
            }
            client.emit('response', response.toString());
        });

        // Send the request to the ZMQ server
        action.send(data);
    });
});

// Simple express routing to provide client side page
app.get('/', function(req, res) {
    res.sendFile(__dirname + '/index.html');
});

// Listen on port 8080
server.listen(8086);
