const zmq = require("zeromq");
const express = require("express");
const http = require("http");
const { Server } = require("socket.io");

const app = express();
const server = http.createServer(app);
const io = new Server(server);

// ZMQ Connection settings
const ZMQ_REQ_ADDR = "tcp://127.0.0.1:5555";
const ZMQ_SUB_ADDR = "tcp://127.0.0.1:5556";

/**
 * Handle asynchronous subscriptions from ZeroMQ
 */
async function runSubscriptionHandler() {
    const sock = new zmq.Subscriber();
    sock.connect(ZMQ_SUB_ADDR);
    sock.subscribe(""); // Subscribe to all topics

    console.log(`[ZMQ] Subscribed to updates on ${ZMQ_SUB_ADDR}`);

    try {
        for await (const [topic] of sock) {
            const msg = topic.toString();
            console.log(`[SUBSCRIPTION] ${msg}`);
            
            // Format: TABLE MSG *CONTENTS
            const args = msg.split(" ");
            const tableId = args[0];
            const content = args.slice(1).join(" ");
            
            // Broadcast to the specific table room in Socket.io
            io.to(tableId).emit("info", content);
        }
    } catch (err) {
        console.error(`[ZMQ SUB ERROR] ${err}`);
    }
}

/**
 * Socket.io Connection Handler
 */
io.on("connection", (socket) => {
    console.log(`Client ${socket.id} connected`);

    // Each client gets its own REQ socket for ZeroMQ
    const zmqReq = new zmq.Request();
    zmqReq.connect(ZMQ_REQ_ADDR);

    socket.on("disconnect", async () => {
        if (socket.table && socket.user && socket.secret) {
            console.log(`Client ${socket.id} disconnected, leaving table ${socket.table}`);
            try {
                await zmqReq.send(`LEAVE ${socket.user} ${socket.secret}`);
                // We don't strictly wait for response on disconnect to avoid hanging
            } catch (err) {
                console.error(`Error sending LEAVE on disconnect: ${err}`);
            }
        }
        console.log(`Client ${socket.id} disconnected`);
    });

    // Handle action messages (REQ/REP)
    socket.on("action", async (data) => {
        console.log(`${socket.id} requested: ${data}`);

        const args = data.split(" ");
        const command = args[0];

        // Process special commands for room management
        if (command === "JOIN" && args.length > 3) {
            socket.user = args[1];
            socket.table = args[3];
            socket.pendingJoin = true;
            socket.join(socket.table);
            console.log(`${socket.id} joining room ${socket.table}`);
        } else if (command === "LEAVE") {
            socket.pendingLeave = true;
        } else if (command === "LISTUSERS") {
            socket.join("user-list-channel");
        }

        try {
            await zmqReq.send(data);
            const [response] = await zmqReq.receive();
            const respStr = response.toString();
            console.log(`Server response: ${respStr}`);

            // Update room state based on response
            if (socket.pendingJoin) {
                if (respStr.startsWith("ERROR")) {
                    socket.leave(socket.table);
                    delete socket.table;
                } else {
                    socket.secret = respStr;
                }
                delete socket.pendingJoin;
            } else if (socket.pendingLeave) {
                socket.leave(socket.table);
                delete socket.table;
                delete socket.pendingLeave;
            }

            socket.emit("response", respStr);
        } catch (err) {
            console.error(`[ZMQ REQ ERROR] ${err}`);
            socket.emit("response", "ERROR: Server communication failed");
        }
    });
});

// Middleware & Static Assets
app.use(express.static("public"));
app.use("/graphics", express.static("graphics"));

app.get("/", (req, res) => {
    res.sendFile(__dirname + "/index.html");
});

// Start the application
const PORT = 8086;
server.listen(PORT, () => {
    console.log(`NodeJS Proxy listening on http://localhost:${PORT}`);
    runSubscriptionHandler().catch(console.error);
});
