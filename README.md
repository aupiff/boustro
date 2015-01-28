# boustro

a web app for reading texts in [boustrophedon](http://en.wikipedia.org/wiki/Boustrophedon)

the frontend is written in [Elm](http://elm-lang.org/), a language you should really check
out!

# todo

frontend

- hyphenation
    + can be preprocessed and cached. independent of font-size, etc.
      just need to create `Penalty` elements

- justification
    + display the letter widths of various alphabets nicely to test out the
      typogaphy library I started
    + write minimum adjustment ration `r_j` code

backend

- user tracking
- add markdown support for texts
