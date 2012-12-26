using System;
using RabbitMQ.Client;
using RabbitMQ.Client.Events;


namespace HumanConsoleClient
{

    class Program
    {
        static kingClient singleton_client = null;

        static void Main(string[] args)
        {
            Console.CancelKeyPress += new ConsoleCancelEventHandler(TerminatingHandler);

            try
            {
                singleton_client = new kingClient();
                singleton_client.GameLoop();
            }
            catch( Exception e )
            {
                System.Console.WriteLine(e.Message);
            }
        }

        protected static void TerminatingHandler(object sender, ConsoleCancelEventArgs args)
        {
            System.Console.WriteLine("Terminating Client");
            if( singleton_client != null )
                singleton_client.Terminate();
        }
    }
}
