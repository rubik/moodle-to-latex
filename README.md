### Installation

This project uses [Stack](https://github.com/commercialhaskell/stack). To
build, launch:

    $ stack install

### Running

At the moment the program expects an expression as a command line argument and
translates it to Latex. Example run:

    $ moodlelat "2 * pi() - sqrt(24, 3) + 4 * (2 - 3)"
    {{2\cdot\pi}-{\sqrt[3]{24}}}+{4\cdot{2}-{3}}
    $ moodlelat "2 * 4"
    2\cdot4
