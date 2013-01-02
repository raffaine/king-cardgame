using System;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using RabbitMQ.Client;

namespace WinStoreKing
{
    /// <summary>
    /// This is the main type for your game
    /// </summary>
    public class KingGame : Game
    {
        // RabbitMQ Objects
        const string RABBITMQ_SERVER_HOSTNAME = "localhost";
        private IConnection _connection;
        private IModel _channel;
        private string _queueName;

        // RabbitMQ Table Parameters
        private string _tableName = null;

        // MonoGame Objects
        GraphicsDeviceManager _graphics;
        SpriteBatch _spriteBatch;

        // Game Resources
        private SpriteFont _font;

        Table _table;

        public KingGame()
        {
            _graphics = new GraphicsDeviceManager(this);
            Content.RootDirectory = "Content";
        }

        /// <summary>
        /// Allows the game to perform any initialization it needs to before starting to run.
        /// This is where it can query for any required services and load any non-graphic
        /// related content.  Calling base.Initialize will enumerate through any components
        /// and initialize them as well.
        /// </summary>
        protected override void Initialize()
        {
            // TODO: Add your initialization logic here

            base.Initialize();
        }

        /// <summary>
        /// LoadContent will be called once per game and is the place to load
        /// all of your content.
        /// </summary>
        protected override void LoadContent()
        {
            // Create a new SpriteBatch, which can be used to draw textures.
            _spriteBatch = new SpriteBatch(GraphicsDevice);

            // TODO: use this.Content to load your game content here
            try
            {
                Card.deck = Content.Load<Texture2D>("cards");
                _font = Content.Load<SpriteFont>("Font");
            }
            catch (Exception e)
            {
                // Stop the game ... cannot load resources
                //TODO: Debug this shit!
                string s = e.Message;
            }

            // This is just to show some stuff ... will be removed
            Random r = new Random();

            string[] order = {"D2","3Horas","Regiane","Samurai"};
            _table = new Table(order, "3Horas");

            string[] test_hand = new string[13];
            for (int i = 0; i < test_hand.Length; ++i)
                test_hand[i] = Card.values[r.Next(13)] + Card.suits[r.Next(4)];

            _table.SetupHand(test_hand);

        }

        /// <summary>
        /// UnloadContent will be called once per game and is the place to unload
        /// all content.
        /// </summary>
        protected override void UnloadContent()
        {
            // TODO: Unload any non ContentManager content here
        }

        /// <summary>
        /// Allows the game to run logic such as updating the world,
        /// checking for collisions, gathering input, and playing audio.
        /// </summary>
        /// <param name="gameTime">Provides a snapshot of timing values.</param>
        protected override void Update(GameTime gameTime)
        {
            // TODO: Add your update logic here

            base.Update(gameTime);
        }

        /// <summary>
        /// This is called when the game should draw itself.
        /// </summary>
        /// <param name="gameTime">Provides a snapshot of timing values.</param>
        protected override void Draw(GameTime gameTime)
        {
            GraphicsDevice.Clear(Color.CornflowerBlue);

            // TODO: Add your drawing code here
            _spriteBatch.Begin();

            int _width = GraphicsDevice.PresentationParameters.BackBufferWidth;
            int _height = GraphicsDevice.PresentationParameters.BackBufferHeight;

            _spriteBatch.DrawString(_font, "1,2,3 ... testando",
                                    new Vector2(_width / 2, 0f), Color.Black);

            _table.Draw(_spriteBatch, _width, _height);

            _spriteBatch.End();

            base.Draw(gameTime);
        }

        /// <summary>
        /// This is called at the startup, to initialize the connection to Server
        /// </summary>
        /// <param name="SinglePlayer">Wich form of game will be played?</param>
        private void StartGameServer(bool SinglePlayer)
        {
            ConnectionFactory cf = new ConnectionFactory();
            cf.HostName = RABBITMQ_SERVER_HOSTNAME;

            _connection = cf.CreateConnection();
            _channel = _connection.CreateModel();

            _queueName = _channel.QueueDeclare(); //The default is an exclusive non-durable queue

            // Use methods above to establish a blocking connection
            // The consumer must be declared in Class Scope, for reuse
            //consumer = new QueueingBasicConsumer(channel);
            //channel.BasicConsume(queue_name, false, consumer);

            if (SinglePlayer)
            {
                PublishHall("agenthall.createTable", "single");
                //process = createTable;
            }
            else
            {
                PublishHall("agenthall.listTable", "open");
                //process = ListTables;
            }
        }

        /// <summary>
        /// Use this method to send a message to some AgentHall
        /// </summary>
        /// <param name="routingKey">Define what must be done, and by whom (who.what)</param>
        /// <param name="message">Define the parameters of the action, like who.what(message) </param>
        private void PublishHall(string routingKey, string message)
        {
            byte[] messageBodyBytes = System.Text.Encoding.UTF8.GetBytes(message);
            IBasicProperties props = _channel.CreateBasicProperties();
            props.ContentType = "text/plain";
            props.ReplyTo = _queueName;

            _channel.BasicPublish("hall", routingKey, props, messageBodyBytes);
        }

        /// <summary>
        /// Use this method to send a message to the actual connected Table
        /// </summary>
        /// <param name="command">Define what is the command you want to execute</param>
        /// <param name="message">Define the parameters of the command</param>
        private void PublishTable(string command, string message)
        {
            string routingKey = String.Format("{0}.{1}", _tableName, command);
            byte[] messageBodyBytes = System.Text.Encoding.UTF8.GetBytes(message);
            IBasicProperties props = _channel.CreateBasicProperties();
            props.ContentType = "text/plain";
            props.ReplyTo = _queueName;

            _channel.BasicPublish("king", routingKey, props, messageBodyBytes);
        }
    }
}
