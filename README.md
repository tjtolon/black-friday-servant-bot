# black-friday-servant-bot

A friend of mine told me about a friendly "hackathon" of sorts a company he
works for hold. I couldn't make into it, but my friend provided me a link to the
code base shared freely with other participants.

[The repository of the event](https://github.com/janneri/bots-of-black-friday/)
describes the rules of the system. In essence there is a game server providing a
square grid map for robots. The server serves UI in HTML viewable by web
browser, and a JSON API for robot AIs. Participant was tasked to implement this
bot that could interact with the server, and move on its turn.

This was my first touch with Aeson and Servant libraries. This is the reason I
have two extra files in the `src` directory, namely `Server.hs` and `Client.hs`. These
do not contribute to the black friday bot, they are purely rewrites of the code
found in
[official Servant tutorial](http://haskell-servant.readthedocs.io/en/stable/tutorial/index.html).

At the moment my bot makes the following:
* Joins into the server
* Parses everything it receives
* When it receives its turn to move, respond by moving towards right

Interesting observations so far:
* I think I understand why so many people are eager to have overlapping
  functions created by the record syntax. Would that been possible, boiler plate
  would be halved in the code.
* Most difficulty I had was when I tried to parse the map grid. It is not
  perfect, because during the parsing I do not check that the size of the grid
  is as it should.
* I am quite mixed with the idea of HTTP/restful/JSON based servers. They are so
  inefficient! On the other hand, this is now the most common architecture for
  M2M. In this thing the bot had a HTTP client open for sending `register` POST,
  and a HTTP server for receiving messages of "its this bots turn, the game is
  now in this state". Again, I was also surprised how every bit of data was
  passed on every turn of the robot, akin to restful design. On my todo I would
  have my memory allocated for things that do not change inside the game between
  turns, for example the grid map.
* In the beginning, both Servant and Aeson felt like magic. In the end, Servant
  continued to feel like magic, while Aeson started slowly to turn into
  something more understandable, "something I could someday come up with".
