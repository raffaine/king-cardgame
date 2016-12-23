﻿Salão para Jogos de Cartas


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
 - Na mão "No King of Hearts", traduzido como "King", o jogador não é 
 obrigado a jogar o Rei de Copas quando a rodada iniciou com Copas.
	- Caso o jogador tenha que descartar e este possui o Rei de 
	Copas, ele deve jogá-lo.

Os arquivos do jogo podem ser descritos como:

  + king.py :: Implementa as regras do jogo
  + kingHallAgent.py :: Implementa o agente do Hall (este é o
    ServerSide no momento)
  + kingAgent.py :: Implementa o agente da Mesa, o HallAgent que
    inicia este processo
  + kingClient.py :: Arquivo base para clientes humanos ou bots
  + kingBoy.py :: Cliente não humano, baseado no kingClient.py
  + kingHuman.py :: Cliente usado por jogadores humanos, baseado no
    kingClient.py

Requerimentos Mínimos
---------------------

  + RabbitMQ :: v3.0
  + Pika :: Biblioteca Python, v0.9.8

Requerimentos para os Clients
-----------------------------

** C# Console
   + RabbitMQ .NET Client v3.0.1 (Adicionado como um submodule do repositorio)
   + Adicionar a referencia a DLL RabbitMQ.Client.dll na Solution

** C# Win 8 Store (Passivel de expansao para outras plataformas)
   + RabbitMQ .NET Client, incluido como submodulo do repositorio, deve
   ser alterado o IContentHeader.cs para nao derivar de ICloneable, sendo
   necessario adicionar o metdodo "object Clone()" na interface.
   + MonoGame v3.0 (BETA), nao adicionei como submodulo, entao recomendo
   instalar e adicionar a referencia que estiver faltando.
   + DISCLAIMER: cards.png foi retirado do site: 
   http://math.hws.edu/javanotes/c13/s1.html
   + DISCLAIMER: Os logos da aplicacao estao usando a imagem retirada
   dos sites: http://gamebanana.com/sprays/38097
   e http://www.behance.net/gallery/King-of-Hearts/424990
   + DISCLAIMER: A imagem de fundo da mesa, foi retirada do site:
   http://graphics.ucsd.edu/courses/rendering/2008/tkim/
   + DISCLAIMER: Como nao tenho VS2010 para gerar minhas proprias
   fontes, utilizei uma fonte que encontrei em um dos Samples da
   MonoGame: https://github.com/CartBlanche/MonoGame-Samples

** C++ Console
   + RabbitMQ C Client (Libs\DLLs do Windows incluidas no Repositorio),
   fontes disponiveis em: https://github.com/alanxz/rabbitmq-c
   + RabbitMQ C++ Wrapper (Libs\DLLs do Windows incluidas no
   Repositorio), fontes disponíveis em:
   https://github.com/alanxz/SimpleAmqpClient
   + Boost 1.47.0 or newer (DLLs incluidas no repositorio, da versao
   1.51.0, por comodidade) deve ser feito download e build da boost
   pelo site: http://www.boost.org/