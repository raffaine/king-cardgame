using System;
using System.IO;
using RabbitMQ.Client;
using RabbitMQ.Client.Events;

namespace HumanConsoleClient
{
    class kingClient
    {
        // RabbitMQ Parameters
        private IConnection connection;
        private IModel channel;
        private string queue_name;
        private QueueingBasicConsumer consumer;

        // Delegate for State Machine
        private delegate void ProcessDelegate(string msg);
        private ProcessDelegate process;

        // Game State and Variables
        private string playerName = null;
        private string tableName = null;
        
        public kingClient()
        {
            ConnectionFactory cf = new ConnectionFactory();
            cf.HostName = "localhost";

            connection = cf.CreateConnection();
            channel = connection.CreateModel();
            queue_name = channel.QueueDeclare(); //The default is an exclusive non-durable queue
            
            consumer = new QueueingBasicConsumer(channel);
            channel.BasicConsume(queue_name, false, consumer);

            process = InvalidProcess;
        }

        public void GameLoop()
        {
            while (playerName == null)
            {
                System.Console.WriteLine("Please provide a player's name: ");
                playerName = System.Console.ReadLine();
            }

            PublishHall("agenthall.listTable", "open");
            process = ListTables;

            while (true)
            {
                try
                {
                    BasicDeliverEventArgs e = (BasicDeliverEventArgs)consumer.Queue.Dequeue();
                   
                    // ... handle the delivery ...
                    process(System.Text.Encoding.UTF8.GetString(e.Body));

                    channel.BasicAck(e.DeliveryTag, false);
                }
                catch (EndOfStreamException ex)
                {
                    // The consumer was cancelled, the model closed, or the
                    // connection went away.
                    break;
                }
            }
        }

        public void Terminate()
        {
            if (tableName != null)
                PublishTable("quit", "empty");
        }

        private void PublishHall(string routingKey, string message)
        {
            byte[] messageBodyBytes = System.Text.Encoding.UTF8.GetBytes(message);
            IBasicProperties props = channel.CreateBasicProperties();
            props.ContentType = "text/plain";
            props.ReplyTo = queue_name;

            channel.BasicPublish("hall", routingKey, props, messageBodyBytes);
        }
        
        private void PublishTable(string command, string message)
        {
            string routingKey = String.Format("{0}.{1}", tableName, command);
            byte[] messageBodyBytes = System.Text.Encoding.UTF8.GetBytes(message);
            IBasicProperties props = channel.CreateBasicProperties();
            props.ContentType = "text/plain";
            props.ReplyTo = queue_name;

            channel.BasicPublish("king", routingKey, props, messageBodyBytes);
        }

        private void InvalidProcess(string msg)
        {
            System.Console.WriteLine("Invalid State, message: {0}", msg);
        }

        private void ListTables(string msg)
        {
            System.Console.WriteLine("Received: {0}", msg);

            tableName = null;

            char[] delim = {','};
            string[] tables = msg.Replace('[', ' ').Replace(']', ' ').Trim().Split(delim);

            if (msg != "[]")
            {
                while (tableName == null)
                {
                    System.Console.WriteLine("Choose Table (inform table position on list): {0}", msg);
                    uint pos = 0;
                    UInt32.TryParse(System.Console.ReadLine(), out pos);
                    if (pos < tables.Length)
                        tableName = tables[pos].Replace("'","");
                }
                PublishTable("join", playerName);
                process = JoinTable;
            }
            else
            {
                System.Console.WriteLine("No Table available, creating a new one.");
                PublishHall("agenthall.createTable", "empty");
                process = CreateTable;
            }
        }

        private void CreateTable(string msg)
        {
            System.Console.WriteLine("Received: {0}", msg);
        }

        private void JoinTable(string msg)
        {
            System.Console.WriteLine("Received: {0}", msg);
        }
    }
}
