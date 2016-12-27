King
====

As regras para o jogo podem ser lidas em
<https://en.wikipedia.org/wiki/King_(card_game)>.

Três alterações foram feitas com base na experiência de jogo:
 - Não existe ordem para as mãos, os jogadores tem liberdade para 
 escolher a melhor mão
	- Observando que cada jogador só pode escolher uma positiva (a 
	não ser que não exista outra opção)
 - Não existem Positivas decrescentes onde os jogadores começam com o 
 máximo e vão perdendo pontos
 - Ao final da Positiva onde um lance foi eleito vencedor, o valor do 
lance será efetivado mesmo que isso force o jogador a pontuar negativamente
  - (TODO) Servidor oferece uma opção para mesas onde lances são efetivados
apenas até o limite do jogador não pontuar, efetivamente um calote.
 - Na mão "No King of Hearts", traduzido como "King", o jogador não é 
 obrigado a jogar o Rei de Copas quando a rodada iniciou com Copas.
	- Caso o jogador tenha que descartar e este possui o Rei de 
	Copas, ele deve jogá-lo.

Os arquivos do jogo podem ser descritos como:

  + king.py :: Implementa as regras do jogo
  + server.py :: Representa um servidor de mesas de King
  + client.py :: Arquivo base para clientes humanos ou bots
  + random_bot.py :: Um cliente que realiza todas as ações de forma aleatória
  + human.py :: Cliente usado por jogadores humanos, baseado no client.py

Requerimentos Mínimos
---------------------

Estes requisitos representam o ambiente onde todos os testes foram bem
sucedidos.

  + Python :: v3.4.2
  + ZeroMQ (0mq) Python Library :: v4.0.5

Informações sobre os Clientes
-----------------------------

Estes requisitos representam os ambientes e linguagens atualmente suportadas, 
suporte este podendo variar de oferta de um cliente base ou apenas stubs exemplo
para comunicação efetiva com servidor.

* C# Console **(Não Testado)**
   + RabbitMQ .NET Client v3.0.1 (Adicionado como um submodule do repositorio)
   + Adicionar a referencia a DLL RabbitMQ.Client.dll na Solution

* C# Win 8 Store (Passivel de expansao para outras plataformas) **(Não Testado)**
   + RabbitMQ .NET Client, incluido como submodulo do repositorio, deve
   ser alterado o IContentHeader.cs para nao derivar de ICloneable, sendo
   necessario adicionar o metdodo "object Clone()" na interface.
   + MonoGame v3.0 (BETA), nao adicionei como submodulo, entao recomendo
   instalar e adicionar a referencia que estiver faltando.
   + **DISCLAIMER**: cards.png foi retirado do site: 
   http://math.hws.edu/javanotes/c13/s1.html
   + **DISCLAIMER**: Os logos da aplicacao estao usando a imagem retirada
   dos sites: http://gamebanana.com/sprays/38097
   e http://www.behance.net/gallery/King-of-Hearts/424990
   + **DISCLAIMER**: A imagem de fundo da mesa, foi retirada do site:
   http://graphics.ucsd.edu/courses/rendering/2008/tkim/
   + **DISCLAIMER**: Como nao tenho VS2010 para gerar minhas proprias
   fontes, utilizei uma fonte que encontrei em um dos Samples da
   MonoGame: https://github.com/CartBlanche/MonoGame-Samples

* C++ Console **(Não Testado)**
   + RabbitMQ C Client (Libs\DLLs do Windows incluidas no Repositorio),
   fontes disponiveis em: https://github.com/alanxz/rabbitmq-c
   + RabbitMQ C++ Wrapper (Libs\DLLs do Windows incluidas no
   Repositorio), fontes disponíveis em:
   https://github.com/alanxz/SimpleAmqpClient
   + Boost 1.47.0 or newer (DLLs incluidas no repositorio, da versao
   1.51.0, por comodidade) deve ser feito download e build da boost
   pelo site: http://www.boost.org/
