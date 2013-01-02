#include "SimpleRpcClient.h"

using namespace AmqpClient;

int main()
{
    char* szBroker = getenv("AMQP_BROKER");
    Channel::ptr_t channel;
    if (szBroker != NULL)
        channel = Channel::Create(szBroker);
    else
        channel = Channel::Create();

	channel->DeclareQueue("alanqueue");
	channel->BindQueue("alanqueue", "amq.direct", "alankey");

	SimpleRpcClient::ptr_t r = SimpleRpcClient::Create(channel, "agenthall.listTable");
	
	std::cout << "Message text: " << r->Call("open") << std::endl;
	getchar();
}