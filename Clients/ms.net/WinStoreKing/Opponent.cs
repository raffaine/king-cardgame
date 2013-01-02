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
}
