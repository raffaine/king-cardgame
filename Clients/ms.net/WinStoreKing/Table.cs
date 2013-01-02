using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;

namespace WinStoreKing
{
    interface IPlayer : IEnumerable<Card>
    {
        string Name
        { get; set; }

        int CardCount();

        void PlayCard(string card);
        void SetHand(string[] cards);
    }

    class FakeHand : IEnumerator<Card>
    {

        Card _back = new Card("");
        int maxcounter;
        int counter;

        public FakeHand(int count)
        {
            maxcounter = counter;
            counter = count;
        }

        public Card Current
        {
            get { return _back; }
        }

        object IEnumerator.Current
        {
            get { return Current; }
        }

        public bool MoveNext()
        {
            return (counter--) > 0;
        }

        public void Reset()
        {
            counter = maxcounter;
        }

        public void Dispose()
        {
        }
    }

    class Opponent : IPlayer
    {
        int numCards;
        string _name;

        public string Name
        {
            get { return _name; }
            set { _name = value; }
        }

        public Opponent(string name)
        {
            _name = name;
            SetHand(null);
        }

        public void SetHand(string[] cards)
        {
            numCards = 13;
        }

        public int CardCount()
        {
            return numCards;
        }

        public void PlayCard(string card)
        {
            numCards--;
        }

        public IEnumerator<Card> GetEnumerator()
        {
            return new FakeHand(numCards);
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return (IEnumerator) GetEnumerator();
        }
    }

    class Player : IPlayer
    {
        List<Card> hand;

        string _name;

        public string Name
        {
            get { return _name; }
            set { _name = value; }
        }

        public Player(string name)
        {
            _name = name;
            hand = new List<Card>();
        }

        public int CardCount()
        {
            return hand.Count;
        }

        public void PlayCard(string card)
        {
            hand.Remove(new Card(card));
        }

        public void SetHand(string[] cards)
        {
            hand.Clear();
            foreach( string c in cards)
                hand.Add(new Card(c));
        }

        public IEnumerator<Card> GetEnumerator()
        {
            return hand.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return (IEnumerator) GetEnumerator();
        }
    }

    enum POSITIONS
    {
        PLAYER = 0,
        LEFT,
        TOP,
        RIGHT
    }

    class Table
    {
        IPlayer[] players;
        int dealer;
        int turn;

        public Table(string[] order, string player)
        {
            players = new IPlayer[4];
            int ind = Array.IndexOf(order, player);
            //TODO: Check for a valid index?

            players[(int)POSITIONS.PLAYER] = new Player(player);
            players[(int)POSITIONS.LEFT] = new Opponent(order[(ind+1) % 4]);
            players[(int)POSITIONS.TOP] = new Opponent(order[(ind+2) % 4]);
            players[(int)POSITIONS.RIGHT] = new Opponent(order[(ind+3) % 4]);

            dealer = (4 - ind) % 4;
            turn = dealer;
        }

        public void SetupHand(string[] cards)
        {
            for (int i = 0; i < 4; ++i)
            {
                if (i == (int) POSITIONS.PLAYER)
                    players[i].SetHand(cards);
                else
                    players[i].SetHand(null);
            }
        }

        static Vector2 getIncrement(POSITIONS p, int count, int w, int h)
        {
            switch (p)
            {
                case POSITIONS.PLAYER:
                    return new Vector2(.8f * w / count, 0f);
                case POSITIONS.LEFT:
                case POSITIONS.RIGHT:
                    return new Vector2(0f, .6f * h / count);
                case POSITIONS.TOP:
                    return new Vector2(.6f * w / count, 0f);
                default:
                    throw new InvalidOperationException("invalid position");
            }
        }

        static Vector2 getStartPos(POSITIONS p, int w, int h)
        {
            switch (p)
            {
                case POSITIONS.PLAYER:
                    return new Vector2(.1f * w + Card.card_x_size / 2,
                                       h - (Card.card_y_size / 2));
                case POSITIONS.LEFT: 
                    return new Vector2( 5f + Card.card_y_size / 2f,
                                       .2f * h);
                case POSITIONS.RIGHT:
                    return new Vector2(w - (5f + Card.card_y_size / 2f),
                                       .2f * h);
                case POSITIONS.TOP:
                    return new Vector2(.2f * w + Card.card_x_size / 2,
                                       Card.card_y_size / 2);
                default:
                    throw new InvalidOperationException("invalid position");
            }
        }

        static float getAngle(POSITIONS p)
        {
            switch (p)
            {
                case POSITIONS.PLAYER:
                    return 0f;
                case POSITIONS.LEFT:
                    return (float) Math.PI/2f;
                case POSITIONS.RIGHT:
                    return -(float)Math.PI / 2f;
                case POSITIONS.TOP:
                    return (float)Math.PI;
                default:
                    throw new InvalidOperationException("invalid position");
            }
        }

        public void Draw(SpriteBatch batch, int width, int height)
        {
            for (int i = 0; i < 4; ++i)
            {
                Vector2 diff = getIncrement((POSITIONS) i, 
                                            players[i].CardCount(),
                                            width, height);

                Vector2 pos = getStartPos((POSITIONS) i,
                                          width, height);

                float angle = getAngle((POSITIONS) i);

                foreach (Card c in players[i])
                {
                    c.Position = pos;
                    c.Angle = angle;
                    c.Draw(batch);
                    pos += diff;
                }
            }
        }
    }
}
