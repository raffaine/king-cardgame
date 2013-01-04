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

        void DefineArea(Vector2 size, float angle, int w, int h);

        int CardCount();

        void PlayCard(string card);
        void SetHand(string[] cards);

        //Helper Drawing Functions
        Vector2 getIncrement(int width, int height);
        Vector2 getStartPos(int width, int height);
    }
}
