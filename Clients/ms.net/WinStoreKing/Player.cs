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
            return new Vector2(area.Width / hand.Count, 0f);
        }

        public Vector2 getStartPos(int width, int height)
        {
            return new Vector2(area.X + Card.card_x_size / 2,
                               area.Y + area.Height/2f /*+ (Card.card_y_size / 2)*/);
        }

        public Rectangle Area
        {
            get
            {
                return area;
            }
        }

        static T easy_select<T>(T one, T two, double cosA)
        {
            if (Math.Abs(cosA) > 0.2)
                return one;

            return two;
        }

        public void DefineArea(Vector2 size, float angle, int w, int h)
        {
            double cosA = Math.Cos((double)angle);
            double sinA = Math.Sin((double)angle);

            Vector2 center = new Vector2(w / 2, h / 2);

            Vector2 newSize = new Vector2(easy_select(size.X, size.Y, cosA),
                                          easy_select(size.Y, size.X, cosA));

            Vector2 diff = new Vector2(easy_select(0f ,center.X - size.X/2f, cosA),
                                       easy_select(center.Y - size.Y / 2f, 0f, cosA));

            Vector2 newCenter = center + ((float)easy_select(cosA, -sinA, cosA)) * diff;

            
            area = new Rectangle((int)(newCenter.X - newSize.X/2f),
                                 (int)(newCenter.Y - newSize.Y/2f),
                                 (int) size.X, (int) size.Y ); 
        }
    }
}
