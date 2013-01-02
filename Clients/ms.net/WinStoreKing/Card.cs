using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;

namespace WinStoreKing
{
    class Card
    {
        // Useful constants for Card Drawing
        public const int card_x_size = 79;
        public const int card_y_size = 123;
        const int back_card_x = card_x_size * 2;
        const int back_card_y = card_y_size * 4;

        // Well ... these ones I cannot made constants ... but they are!
        static readonly public string[] values = { "A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K" };
        static readonly public string[] suits = { "C", "D", "H", "S" };

        // Static Texture representing Deck ...
        static public Texture2D deck;

        // Instance Values
        string card;
        Rectangle deck_pos;
        Vector2 position;
        Vector2 origin;
        float angle;

        //Accessors
        public Vector2 Position
        {
            get { return position; }
            set { position = value; }
        }

        public float Angle
        {
            get { return angle; }
            set { angle = value; }
        }

        // Use empty value to define a back oriented card
        public Card(string value)
        {
            card = value;
            position = new Vector2(0, 0);
            origin = new Vector2(card_x_size / 2, card_y_size / 2);
            angle = 0f;
            //TODO: Check if its a valid combination of Value and suit
            if (value.Length == 0)
                deck_pos = new Rectangle(back_card_x, back_card_y,
                                         card_x_size,card_y_size);
            else
                deck_pos = new Rectangle(card_x_size * Array.IndexOf(values, value.Substring(0, value.Length - 1)),
                                         card_y_size * Array.IndexOf(suits, value.Substring(value.Length - 1)),
                                         card_x_size, card_y_size);
        }

        public void Draw(SpriteBatch batch)
        {
            batch.Draw(deck, position, deck_pos, Color.White,
                       angle, origin, 1f, SpriteEffects.None, 0f);
        }

        // Object overloaded methods

        public override bool Equals(object obj)
        {
            return  obj != null &&
                    (obj as Card) != null &&
                    (obj as Card).card == card;
        }

        public override int GetHashCode()
        {
            return card.GetHashCode();
        }

        public override string ToString()
        {
            return card;
        }
    }
}
