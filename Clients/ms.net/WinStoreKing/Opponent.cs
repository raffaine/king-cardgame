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
    class FakeHand : IEnumerator<Card>
    {

        Card _back = new Card("");
        int maxcounter;
        int counter;

        public FakeHand(int count, float angle)
        {
            maxcounter = counter;
            counter = count;
            _back.Angle = angle;
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

    abstract class BaseOpponent : IPlayer
    {
        protected int numCards;
        protected string _name;
        protected Rectangle area;
        protected float angle;

        public string Name
        {
            get { return _name; }
            set { _name = value; }
        }
        public Rectangle Area
        {
            get { return area; }
            set { area = value; }
        }
        public float Angle
        {
            get { return angle; }
            set { angle = value; }
        }

        public BaseOpponent(string name)
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

        public abstract IEnumerator<Card> GetEnumerator();
        public abstract Vector2 getIncrement(int width, int height);
        public abstract Vector2 getStartPos(int width, int height);

        IEnumerator IEnumerable.GetEnumerator()
        {
            return (IEnumerator) GetEnumerator();
        }


        public void DefineArea(Vector2 size, float angle, int w, int h)
        {
            throw new NotImplementedException();
        }
    }

    class LeftOpponent : BaseOpponent
    {
        public LeftOpponent(string name)
            : base(name)
        {
        }

        public override IEnumerator<Card> GetEnumerator()
        {
            return new FakeHand(numCards, (float)Math.PI/2f);
        }

        public override Vector2 getIncrement(int width, int height)
        {
            return new Vector2(0f, .6f * height / numCards);
        }

        public override Vector2 getStartPos(int width, int height)
        {
            return new Vector2(5f + Card.card_y_size / 2f,
                               .2f * height);
        }
    }

    class RightOpponent : BaseOpponent
    {
        public RightOpponent(string name)
            : base(name)
        {
        }

        public override IEnumerator<Card> GetEnumerator()
        {
            return new FakeHand(numCards, -(float)Math.PI / 2f);
        }

        public override Vector2 getIncrement(int width, int height)
        {
            return new Vector2(0f, .6f * height / numCards);
        }

        public override Vector2 getStartPos(int width, int height)
        {
            return new Vector2(width - (5f + Card.card_y_size / 2f),
                                .2f * height);
        }
    }

    class TopOpponent : BaseOpponent
    {
        public TopOpponent(string name)
            : base(name)
        {
        }

        public override IEnumerator<Card> GetEnumerator()
        {
            return new FakeHand(numCards, (float)Math.PI);
        }

        public override Vector2 getIncrement(int width, int height)
        {
            return new Vector2(.6f * width / numCards, 0f);
        }

        public override Vector2 getStartPos(int width, int height)
        {
            return new Vector2(.2f * width + Card.card_x_size / 2,
                               Card.card_y_size / 2);
        }
    }
}
