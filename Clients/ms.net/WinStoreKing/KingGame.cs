using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
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
        #region Delegates for State Machines (Message Processing and Game State)
        public delegate void DrawingDelegate(GameTime time);
        public delegate void UpdateDelegate(GameTime time);
        public Tuple<DrawingDelegate, UpdateDelegate> gameState;
        
        private delegate void ProcessDelegate(string msg);
        private ProcessDelegate currentProcess;
        #endregion

        #region RabbitMQ Objects
        const string RABBITMQ_SERVER_HOSTNAME = "localhost";
        private IConnection _connection = null;
        private IModel _channel;
        private string _queueName;

        // RabbitMQ Table Parameters
        private string _tableName = null;
        #endregion

        #region MonoGame Objects and Game Resources
        GraphicsDeviceManager _graphics;
        SpriteBatch _spriteBatch;

        // Game Resources
        Table _table;
        Menu _menu;

        int elapsedTime = 0;
        bool lbutton_pressed = false;
        
        string[] order;
        // TODO: Must get from user input!!!
        string player_name = "Bob";
        #endregion

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
                Table._background = Content.Load<Texture2D>("back");
                Table._dealertex = Content.Load<Texture2D>("dealer");
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
            // Establish the Menu as the first gamestate
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

            if (_table == null)
            {
                TextManager.Draw(_spriteBatch, "Aguarde, carregando a mesa",
                                new Vector2(_width / 2f, _height / 2f),
                                TextManager.TEXT_ALIGN.CENTER);
                return;
            }

            _table.Resize(_width, _height);
            _table.Draw(_spriteBatch);
        }

        protected void GameRunningUpdate(GameTime time)
        {
            //TODO: Redesign this Update Loop ... 
            // I need to wait after each card played
            TimeSpan e = time.ElapsedGameTime;
            elapsedTime += e.Milliseconds;

            if (elapsedTime > 1000)
            {
                try
                {
                    BasicGetResult result = _channel.BasicGet(_queueName, false);
                    if (result != null)
                    {
                        //IBasicProperties props = result.BasicProperties;
                        string msg = Encoding.UTF8.GetString(result.Body, 0, result.Body.Length);
                        // Processa a mensagem com o Delegate atual
                        currentProcess(msg);
                        // Envia um ack para o server
                        _channel.BasicAck(result.DeliveryTag, false);
                    }
                }
                //TODO Provide a more clear exception handle
                catch (Exception ex)
                {
                    //TODO Handle trouble here
                    string s = ex.Message;
                }

                //TODO Extend this part to get other user's input
                // Like selection of Table, Hand, Positive Auction and Trunf
                if ((_table != null) && (_table.isPlayerTurn()))
                {
                    var st = Mouse.GetState();
                    Card sel = null;

                    if (lbutton_pressed && (st.LeftButton == ButtonState.Released))
                    {
                        sel = _table.GetClicked(new Point(st.X, st.Y));
                        lbutton_pressed = false;
                    }
                    else if (st.LeftButton == ButtonState.Pressed)
                    {
                        _table.SetMouseOver(new Point(st.X, st.Y));
                        lbutton_pressed = true;
                    }

                    if (sel == null)
                        return;

                    PublishTable("play", sel.ToString());
                }

                elapsedTime = 0;
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
                            StartGameServer(true);
                            gameState = new Tuple<DrawingDelegate, UpdateDelegate>(GameRunningDraw, GameRunningUpdate);
                            break;
                        case Menu.Regras:
                            var mi = new MenuItemTextVisualizer(king_rules);
                            mi.Image = Content.Load<Texture2D>("TextVisualizer");
                            _menu.Reset();
                            _menu.AddSpecialContent(mi);
                            _menu.AddContent( Menu.NovoJogo, Content.Load<Texture2D>("MenuNew"));
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
            if (_connection == null)
            {
                ConnectionFactory cf = new ConnectionFactory();
                cf.HostName = RABBITMQ_SERVER_HOSTNAME;

                _connection = cf.CreateConnection();
                _channel = _connection.CreateModel();

                //The default is an exclusive non-durable queue
                _queueName = _channel.QueueDeclare();

                #region General comments
                // Use methods above to establish a blocking connection
                // The consumer must be declared in Class Scope, for reuse
                // Now I'm using unblocking get that I think it's better
                //consumer = new QueueingBasicConsumer(channel);
                //channel.BasicConsume(queue_name, false, consumer);
                #endregion
            }

            elapsedTime = 0;
            if (SinglePlayer)
            {
                PublishHall("agenthall.createTable", "single");
                currentProcess = CreateTable;
            }
            else
            {
                PublishHall("agenthall.listTable", "open");
                currentProcess = ListTables;
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

        private void ListTables(string msg)
        {
            //TODO: Complete Redesign needed
            _tableName = null;

            char[] delim = { ',' };
            string[] tables = msg.Replace('[', ' ').Replace(']', ' ').Trim().Split(delim);

            if (msg != "[]")
            {
                // TODO ... I think that a new gameState will be necessary to handle
                // user input
                while (_tableName == null)
                {
                    uint pos = 0;
                    //UInt32.TryParse(System.Console.ReadLine(), out pos);
                    if (pos < tables.Length)
                        _tableName = tables[pos].Replace("'", "");
                }
                PublishTable("join", player_name);
                currentProcess = JoinTable;
            }
            else
            {
                PublishHall("agenthall.createTable", "empty");
                currentProcess = CreateTable;
            }
        }

        private void CreateTable(string msg)
        {
            _tableName = msg;
            PublishTable("join", player_name);
            currentProcess = JoinTable;
        }

        private void JoinTable(string msg)
        {
            if (msg != "OK")
            {
                //Some kind of failure ... like full table when playing multiplayer
                //reset to listing
                currentProcess = ListTables;
            }
            else
            {
                //Yeah ... let's wait for the start and then play!
                currentProcess = StartGame;
            }
        }

        private void StartGame(string msg)
        {
            //TODO If i'm multiplayer then wait not too much
            order = ParseStringList(msg, "ORDER ");

            _table = new Table(order, "Bob");

            currentProcess = SetupHand;
        }

        private void SetupHand(string msg)
        {
            if (msg.StartsWith("GAMEOVER"))
            {
                //GAME IS OVER ... WHo knows what to do?
                gameState = new Tuple<DrawingDelegate, UpdateDelegate>(GameMenuDraw, GameMenuUpdate);
                return;
            }
            
            string[] hand = ParseStringList(msg, "HAND ");

            _table.SetupHand(hand);

            if (_table.isPlayerTurn())
                currentProcess = ChooseHand;
            else
                currentProcess = DefineHand;
        }

        private void DefineHand(string msg)
        {
            //TODO: Is better to check for error condition
            _table.Hand = msg.Remove(0, "HAND ".Length);
            currentProcess = RoundPlay;
        }

        private void ChooseHand(string msg)
        {
            //TODO: Take user input here
            string[] possible = ParseStringList(msg, "CHOOSE ");
            PublishTable("chooseHand", possible[0]);

            currentProcess = DefineHand;
        }

        private void RoundPlay(string msg)
        { 
            if (msg.StartsWith("ENDHAND"))
            {
                string s1 = msg.Substring(8, msg.IndexOf(']') - 7);
                int[] handscore = ParseIntList(s1);
                int[] totalscore = ParseIntList(msg.Remove(0, 9 + s1.Length));

                _table.EndHand(handscore, totalscore);
                currentProcess = SetupHand;
            }
            else if (msg.StartsWith("ENDROUND"))
            {
                string[] values = msg.Split(' ');
                string nextname = values[1];

                int[] score = ParseIntList(msg.Remove(0, 10 + nextname.Length));

                _table.EndRound(nextname, score);
            }
            else if (msg.StartsWith("PLAY"))
            {
                string[] values = msg.Split(" ".ToCharArray());
                //TODO Values must be 3 values. eg PLAY who 3H
                _table.PlayCard(values[values.Length - 1], values[1]);
            }
            // I'm not treating the error case ... it would be the else
        }

        static string[] ParseStringList(string str, string begin="")
        {
            char[] delim = { ',' };
            string[] values = str.Remove(0, begin.Length).
                                  Replace('[', ' ').Replace(']', ' ').Trim().
                                  Split(delim);

            return Enumerable.Select(values, 
                                     x => x.Trim().Replace("'", "")).ToArray();
        }

        static int[] ParseIntList(string str, string begin = "")
        {
            char[] delim = { ',' };
            string[] values = str.Remove(0, begin.Length).
                                  Replace('[', ' ').Replace(']', ' ').Trim().
                                  Split(delim);

            return Enumerable.Select(values,
                                     x => Int32.Parse(x)).ToArray();
        }

        #region King Rules
        // Oh bullshit ... maybe some easy Rss feeder here would help
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
        #endregion
    }
}
