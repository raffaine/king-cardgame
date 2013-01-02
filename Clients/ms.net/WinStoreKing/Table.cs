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
