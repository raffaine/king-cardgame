using System;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Microsoft.Xna.Framework.Input;
using RabbitMQ.Client;

namespace WinStoreKing
{
    /// <summary>
    /// This is the main type for your game
    /// </summary>
    public class KingGame : Game
    {
        // Delegates for State Machines (Message Processing and Game State)
        public delegate void DrawingDelegate(GameTime time);
        public delegate void UpdateDelegate(GameTime time);
        public Tuple<DrawingDelegate, UpdateDelegate> gameState;

        private delegate void ProcessDelegate(string msg);
        private ProcessDelegate currentProcess;

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
        Menu _menu;
        bool lbutton_pressed = false;

        int elapsedTime;
        int turn;
        int card;
        string[] order = { "D2", "3Horas", "Regiane", "Samurai" };
        string[] test_hand = new string[13];

        public KingGame()
        {
            _graphics = new GraphicsDeviceManager(this);

            // Enable the default mouse view
            IsMouseVisible = true;
#if WIN7
            _graphics.IsFullScreen = true;
#endif

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
            _menu = new Menu();

            // TODO: use this.Content to load your game content here
            try
            {
                Card.deck = Content.Load<Texture2D>("cards");
                TextManager.Font = Content.Load<SpriteFont>("Font");

                _menu.AddContent( Menu.NovoJogo, Content.Load<Texture2D>("MenuNew"));
                _menu.AddContent( Menu.Regras, Content.Load<Texture2D>("MenuRules"));
            }
            catch (Exception e)
            {
                // Stop the game ... cannot load resources
                //TODO: Debug this shit!
                string s = e.Message;
            }
            
            // This is just to show some stuff ... will be removed
            Random r = new Random();

            _table = new Table(order, "3Horas");

            for (int i = 0; i < test_hand.Length; ++i)
                test_hand[i] = Card.values[r.Next(13)] + Card.suits[r.Next(4)];

            _table.SetupHand(test_hand);

            elapsedTime = 0;
            turn = 0;
            card = 0;

            gameState = new Tuple<DrawingDelegate, UpdateDelegate>(GameMenuDraw, GameMenuUpdate);
            
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
            if (gameState != null)
                gameState.Item2(gameTime);

            base.Update(gameTime);
        }

        /// <summary>
        /// This is called when the game should draw itself.
        /// </summary>
        /// <param name="gameTime">Provides a snapshot of timing values.</param>
        protected override void Draw(GameTime gameTime)
        {
            GraphicsDevice.Clear(Color.CornflowerBlue);

            _spriteBatch.Begin();

            if (gameState != null)
                gameState.Item1(gameTime);

            _spriteBatch.End();

            base.Draw(gameTime);
        }

        protected void GameRunningDraw(GameTime time)
        {
            int _width = GraphicsDevice.PresentationParameters.BackBufferWidth;
            int _height = GraphicsDevice.PresentationParameters.BackBufferHeight;

            TextManager.Draw(_spriteBatch, "1,2,3 ... testando",
                             new Vector2(_width / 2f, _height / 2f), 
                             TextManager.TEXT_ALIGN.CENTER);

            _table.Resize(_width, _height);
            _table.Draw(_spriteBatch);
        }

        protected void GameRunningUpdate(GameTime time)
        {
            //TODO: Temp bullshit, must wait for some real stuff
            TimeSpan e = time.ElapsedGameTime;
            elapsedTime += e.Milliseconds;

            if (elapsedTime > 1000)
            {
                elapsedTime = 0;
                if (turn == 0)
                    _table.EndRound();

                if (_table.GetCardCount(order[turn]) == 0)
                    _table.SetupHand(test_hand);

                _table.PlayCard(test_hand[card], order[turn]);
                turn = (turn + 1) % 4;
                if (turn == 0)
                    card = (card + 1) % 13;
            }
        }

        protected void GameMenuDraw(GameTime time)
        {
            int _width = GraphicsDevice.PresentationParameters.BackBufferWidth;
            int _height = GraphicsDevice.PresentationParameters.BackBufferHeight;

            _menu.Resize(_width, _height);
            _menu.Draw(_spriteBatch);
        }

        protected void GameMenuUpdate(GameTime time)
        {
            MouseState st = Mouse.GetState();
            
            if (lbutton_pressed)
            {
                // If button alreay pressed check for release
                if (st.LeftButton == ButtonState.Released)
                {
                    lbutton_pressed = false;
                    int item = _menu.CheckSelected(new Point(st.X, st.Y));
                    switch (item)
                    {
                        case Menu.NovoJogo:
                            gameState = new Tuple<DrawingDelegate, UpdateDelegate>(GameRunningDraw, GameRunningUpdate);
                            break;
                        case Menu.Regras:
                            var mi = new MenuItemTextVisualizer(king_rules);
                            mi.Image = Content.Load<Texture2D>("TextVisualizer");
                            _menu.Reset();
                            _menu.AddSpecialContent(mi);
                            break;
                        default:
                            break;
                    }
                }
                // If not released, then mouse hover on menu
                else
                    _menu.SetMouseOver(new Point(st.X, st.Y));
            }
            //Otherwise check if it has been pressed right now
            else
                lbutton_pressed = (st.LeftButton == ButtonState.Pressed);
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

        static string king_rules = "Decide-se o primeiro jogador a dar as cartas, atraves de sorteio.\n"
                                   + "A hierarquia das cartas, em ordem decrescente, eh : As, Rei, Dama, Valete, 10 ... ate 2.\n"
                                   + "O jogo desenrola-se sempre pela esquerda, iniciando-se pelo carteador, e ganha a rodada quem jogar a carta mais alta do primeiro naipe jogado nessa rodada, ou o maior trunfo jogado, se tiver sido declarado um trunfo na mao. Sao 13 as rodadas possiveis em cada mao.\n"
                                   + "As cartas sao distribuidas totalmente, uma a uma.\n"
                                   + "Jogo Individual\n"
                                   + "Tambem conhecido como 'King Ind', eh um jogo individual e cada partida eh composta por seis maos ou jogadas negativas e quatro positivas. A contagem dos pontos pode ser feita apenas somando os pontos de cada jogador, sem discriminar em duas colunas. No final quem tiver o melhor saldo positivo sera o ganhador.\n"
                                   + "Maos\nA soma dos pontos atribuidos aos jogadores nas seis maos negativas totaliza -1300. A soma dos pontos atribuidos aos jogadores nas quatro maos positivas (i.e. leiloes) totaliza +1300. No fim do jogo a soma dos pontos dos jogadores deve ser zero.\n"
                                   + "Maos negativas\n"
                                   + "- Vaza: O jogador que distribuiu as cartas joga uma carta qualquer, aberta. Todos os demais deverao servir aquele naipe, soh podendo descartar uma carta de outro naipe qualquer se nao tiver o naipe jogado. Quem ganhar uma rodada dara inicio a seguinte. No final, cada participante devera contar e anunciar, para marcacao, quantas rodadas fez. Cada rodada vale 20 pontos negativos. Para efeito de conferencia, o total desta jogada deve dar 260 pontos.\n"
                                   + "- Copas: O objetivo eh nao receber rodadas que contenham cartas de Copas. Regra importante: eh proibido iniciar uma vaza com carta de Copas, tendo qualquer outro naipe na mao. Quem nao tiver condicoes de acompanhar o naipe jogado, podera descartar Copas ou carta de qualquer outro naipe, que considere mais perigoso. No final, contam-se as cartas de Copas recebidas por cada jogador. Cada carta de Copas vale 20 pontos negativos. O total de pontos da rodada eh 260.\n"
                                   + "- Mulheres: Para cada Dama recolhida, o jogador perdera 50 pontos. O total da mao eh 200 pontos. Esta mao tambem eh conhecida como 'Damas'.\n"
                                   + "- Homens: O objetivo eh nao ganhar vazas que contenham Reis ou Valetes. Cada uma destas cartas vale 30 pontos negativos e o total de pontos da mao eh 240. Esta mao tambem eh conhecida como 'Reis ou Valetes'.\n"
                                   + "- King: Quem receber o Rei de Copas em uma rodada, perdera 160 pontos. Importante: Nenhum jogador podera iniciar rodada com Copas, enquanto tiver carta de qualquer outro naipe. O jogador que possuir o Rei de Copas eh obrigado a joga-lo na primeira oportunidade, seja, na primeira vez que nao possa acompanhar o naipe puxado, seja na primeira puxada de copas.\n"
                                   + "- Duas Ultimas: O objectivo eh nao fazer as duas ultimas rodadas, como o proprio nome da jogada esta indicando. Para cada uma das ultimas rodadas feita, o jogador marca 90 pontos negativos, totalizando 180 pontos na mao.\n"
                                   + "O total de pontos negativos dos quatro jogadores devera somar 1.300.";
    }
}
