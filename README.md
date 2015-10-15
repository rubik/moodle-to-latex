### Running

Attoparsec is the only required package. At the moment the only entry point is
in ``Main.hs``. The program loops forever asking for input and translating to
Latex. Example run:

    $ runhaskell Moodle/Main.hs
    2 * pi() - sqrt(24, 3) + 4 * (2 - 3)
    {{2\cdot\pi}-{\sqrt[3]{24}}}+{4\cdot{2}-{3}}
