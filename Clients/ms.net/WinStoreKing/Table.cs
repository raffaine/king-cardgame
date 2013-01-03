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
        List<Card> table;

        int dealer;
        int turn;

        int width;
        int height;
        Vector2 center;

        public Table(string[] order, string player)
        {
            table = new List<Card>();
            players = new IPlayer[4];
            int ind = Array.IndexOf(order, player);
            //TODO: Check for a valid index?

            players[(int)POSITIONS.PLAYER] = new Player(player);
            players[(int)POSITIONS.LEFT] = new LeftOpponent(order[(ind+1) % 4]);
            players[(int)POSITIONS.TOP] = new TopOpponent(order[(ind+2) % 4]);
            players[(int)POSITIONS.RIGHT] = new RightOpponent(order[(ind+3) % 4]);

            dealer = (4 - ind) % 4;
            turn = dealer;
        }

        //TODO: Useless ... remove later.
        public int GetCardCount(string player)
        {
            return Array.Find(players, e => e.Name == player).CardCount();
        }

        public void Resize(int w, int h)
        {
            width = w;
            height = h;
            center = new Vector2(w / 2f, h / 2f);
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

        public void PlayCard(string card, string player)
        {
            // Must I check for player in array or assume is valid?
            int pos = Array.FindIndex(players, e => e.Name == player);

            Card c = new Card(card);
            switch ((POSITIONS)pos)
            {
                case POSITIONS.PLAYER:
                    c.Position = new Vector2(center.X,
                                             center.Y + Card.card_y_size / 2);
                    break;
                case POSITIONS.LEFT:
                    c.Angle = (float)Math.PI / 2;
                    c.Position = new Vector2(center.X - Card.card_y_size / 2,
                                             center.Y);
                    break;
                case POSITIONS.RIGHT:
                    c.Angle = -(float)Math.PI / 2;
                    c.Position = new Vector2(center.X + Card.card_y_size / 2,
                                             center.Y);
                    break;
                case POSITIONS.TOP:
                    c.Angle = (float)Math.PI;
                    c.Position = new Vector2(center.X,
                                             center.Y - Card.card_y_size / 2);
                    break;
                default:
                    throw new InvalidOperationException("Player is not in table");
            }

            // TODO: Here comes the animation!
            table.Add(c);
            players[pos].PlayCard(card);
        }

        //TODO: Enhance this one
        public void EndRound()
        {
            table.Clear();
        }

        public void Draw(SpriteBatch batch)
        {
            // Draw Player's Hands
            foreach (IPlayer player in players)
            {
                Vector2 diff = player.getIncrement(width, height);
                Vector2 pos = player.getStartPos(width, height);

                foreach (Card c in player)
                {
                    c.Position = pos;
                    c.Draw(batch);
                    pos += diff;
                }
            }

            //Draw Table Cards
            foreach (Card c in table)
                c.Draw(batch);
        }
    }
}
