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
    class Player : IPlayer
    {
        List<Card> hand;
        Rectangle area;
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

        public Vector2 getIncrement(int width, int height)
        {
            return new Vector2(.8f * width / hand.Count, 0f);
        }

        public Vector2 getStartPos(int width, int height)
        {
            return new Vector2(.1f * width + Card.card_x_size / 2,
                               height - (Card.card_y_size / 2));
        }


        public Rectangle Area
        {
            get
            {
                return area;
            }
        }


        public void DefineArea(Vector2 size, float angle, int w, int h)
        {
            double cosA = Math.Cos((double)angle);
            double sinA = Math.Sin((double)angle);

            //int nw = (int)(w * cosA) + (int)(h * sinA);
            //int nh = (int)(h * cosA) + (int)(w * sinA);

            size.X = (int)(size.X * cosA) + (int)(size.Y * sinA);
            size.Y = (int)(size.Y * cosA) + (int)(size.X * sinA);

            int nw = (int)(((0.2*(1.0+cosA)/2.0) + 0.2*((1.0-cosA)/2.0)*cosA) * w);
            int nh = (int)(0.2f * h);
            area = new Rectangle(); 
        }
    }
}
