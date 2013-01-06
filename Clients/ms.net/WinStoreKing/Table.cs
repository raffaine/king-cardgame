﻿using System;
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

        public static Texture2D _background;
        public static Texture2D _dealertex;
        int _backnx, _backny;

        string hand;
        public string Hand 
        { 
            get { return hand; }
            set { hand = value; }
        }

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

        public bool isPlayerTurn()
        {
            return (turn == (int)POSITIONS.PLAYER);
        }

        public void Resize(int w, int h)
        {
            width = w;
            height = h;
            center = new Vector2(w / 2f, h / 2f);

            _backnx = (int)Math.Ceiling(w / 512.0);
            _backny = (int)Math.Ceiling(h / 512.0);

            foreach (IPlayer p in players)
                p.Resize(w, h);
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

            turn = (turn + 1) % 4;
        }

        //TODO: Enhance this one
        public void EndRound(string next_player)
        {
            table.Clear();

            turn = Array.FindIndex(players, e => e.Name == next_player);
        }

        public void EndHand()
        {
            //TODO: Here I receive some final scores for the hand
            dealer = (dealer + 1) % 4;
            turn = dealer;
        }

        public void SetMouseOver(Point pos)
        {
            IPlayer p = players[(int)POSITIONS.PLAYER];

            Vector2 diff = p.getIncrement();
            Vector2 start = p.getStartPos();
            start.X -= Card.card_x_size / 2f;

            Vector2 curpos = start;
            int base_y = (int)(start.Y - Card.card_y_size/2f);

            foreach (Card c in p)
            {
                Rectangle r = new Rectangle((int)curpos.X,
                                             base_y,
                                             Math.Min((int)diff.X, Card.card_x_size),
                                             Card.card_y_size);
                c.SetMouseOver(r.Contains(pos));
                curpos += diff;
            }
        }

        public Card GetClicked(Point pos)
        {
            IPlayer p = players[(int)POSITIONS.PLAYER];

            Vector2 diff = p.getIncrement();
            Vector2 start = p.getStartPos();
            start.X -= Card.card_x_size / 2f;

            Vector2 curpos = start;
            int base_y = (int)(start.Y - Card.card_y_size / 2f);

            foreach (Card c in p)
            {
                Rectangle r = new Rectangle((int)curpos.X,
                                             base_y,
                                             Math.Min((int)diff.X, Card.card_x_size),
                                             Card.card_y_size);
                if (r.Contains(pos))
                    return c;

                curpos += diff;
            }

            return null;
        }

        public void Draw(SpriteBatch batch)
        {
            // Draw background
            for (int y = 0; y < _backny; ++y)
            {
                for (int x = 0; x < _backnx; ++x)
                    batch.Draw(_background, new Vector2(512f * x, 512f * y), Color.White);
            }

            // Draw Game name
            if (hand != null)
                TextManager.Draw(batch, hand, center, TextManager.TEXT_ALIGN.CENTER);

            // Draw Player's Hands and Names
            foreach (IPlayer player in players)
            {
                TextManager.Draw( batch,
                                  player.Name,
                                  player.getNamePos(),
                                  TextManager.TEXT_ALIGN.CENTER);

                Vector2 diff = player.getIncrement();
                Vector2 pos = player.getStartPos();

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

            // Draw Dealer Button
            var mat = Matrix.CreateRotationZ((float)(Math.PI / 2.0 * (dealer - 3)));
            Vector2 sizes = new Vector2(.5f * width, .45f * height);
            Vector2 basis = new Vector2(1f,-1f);
            basis.Normalize();
            
            Vector2 dpos = Vector2.Transform(basis, mat);
            dpos.X *= sizes.X;
            dpos.Y *= sizes.Y;

            batch.Draw(_dealertex, center+dpos, null, Color.White,
                       (float)(Math.PI/2f)*dealer, new Vector2(50f,50f), 
                       1f, SpriteEffects.None, 0f);
        }

        public static string[] SortCards(string[] cards)
        {
            return cards;    
        }
    }
}
