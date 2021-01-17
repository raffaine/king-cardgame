// Include required modules and initialize them
var zmq = require('zeromq');
var express = require('express');
var app = express();
var server = require('http').createServer(app);
var io = require('socket.io')(server);

// Initialize the two main sockets (REQ/REP and PUB/SUB) for ZeroMQ
// TODO: Error Handling on connection failure
// TODO: Load URL from config file
// TODO: Route the channel messages
var action = zmq.socket('req');
action.connect('tcp://127.0.0.1:5555');
var info = zmq.socket('sub');
info.connect('tcp://127.0.0.1:5556');

// Subscribe to everything since we handle filters with socket.io rooms
info.subscribe('');

// Setup Subscribe channel handler (very simple, just filter based on rooms)
info.on('message', function(topic, data) {
    // For some reason, data is empty and all message is on topic
    var msg = topic.toString();
    console.log('[SUBSCRIPTION] '+ msg);
    // data always follows format TABLE MSG *CONTENTS (contents cardinality is 0..*)
    var args = msg.split(' ');
    io.to(args[0]).emit('info', args.slice(1).join(' '));
});

// Set up client specific behavior during handle of client connection
io.on('connection', function(client){
    console.log(`Client ${client.id} connected ...`);

    // Setup handler for client disconnection
    client.on('disconnect', function(){
        if (client.table) {
            // Try to gracefuly leave the table on ZMQ server
            action.once('response', function(response) {
                if (response.toString() !== 'ACK') {
                    console.log('ERROR LEAVING TABLE ON DISCONNECTION!!!');
                }
            });
            action.send(`LEAVE ${client.user} ${client.secret}`);
        }
        console.log(`Client ${client.id} has disconnected.`);
    });

    // Handle action messages (REQ/REP)
    client.on('action', function(data) {
        // Here is where ZMQ and Socket.io talk with each other
        console.log(`${client.id} requested ${data}`);

        // SPECIAL HANDLING OF MESSAGES JOIN and LEAVE
        var arr = data.split(' ', 4);
        // User requested a join, if answer is 'ACK', it was successfull
        if (arr.length > 3 && arr[0] === 'JOIN') {
            // Handles special situation where player attempts to join a table
            // We temporarily join room so we don't miss any events on that table
            client.table = arr[3];
            client.user = arr[1];
            client.joinning = true;

            client.join(client.table);
            console.log(`${client.id} joins ${client.table}`);
        }
        else if (arr.length > 2 && arr[0] === 'LEAVE') {
            client.leaving = true;
        } else if (arr.length == 1 && arr[0] === 'LISTUSERS') {
            // Route the user-list-channel messages
            client.join('user-list-channel')
        }

        // Setup an event listener to handle the ZMQ server response
        action.once('message', function(response) {
            console.log(`Server response was ${response}`);
            // Special handling for JOIN message
            if (client.table && client.joinning) {
                // Leave Socket.io room if answer was not ACK
                if (response.toString().startsWith('ERROR')) {
                    console.log(`${client.id} leaves ${client.table}`);
                    client.leave(client.table);

                    delete client.table;
                } else {
                    client.secret = response.toString()
                }

                delete client.joinning;
            }
            // Final stage of LEAVE message
            else if (client.leaving) {
                client.leave(client.table);
                delete client.leaving;
            }

            // Send the response back to client
            client.emit('response', response.toString());
        });

        // Send the request to the ZMQ server
        action.send(data);
    });
});

// Make static css and js files available
app.use(express.static('public'));
app.use('/graphics', express.static('graphics'));

// Simple express routing to provide client side page
app.get('/', function(req, res) {
    res.sendFile(__dirname + '/index.html');
});

// Listen on port 8080
server.listen(8086);
