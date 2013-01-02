using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using WinStoreKing;

namespace Win7King
{
    class Program
    {
        static void Main(string[] args)
        {
            using (KingGame game = new KingGame())
                game.Run();
        }
    }
}
