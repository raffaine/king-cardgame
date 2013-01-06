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

        Rectangle Area
        { get; }

        int CardCount();

        void PlayCard(string card);
        void SetHand(string[] cards);

        //Helper Drawing Functions
        void Resize(int w, int h);
        Vector2 getIncrement();
        Vector2 getStartPos();
        Vector2 getNamePos();
    }
}
