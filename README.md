# Elm Plot

Plot stuff in SVG with Elm!


## Overview

So, in the spirit of Elm and its goal of not only being a tool to express your code elegantly,
but also actively guiding you to do so, this library tries to guide you towards nicer plots. Of course, it's not just my
own preferences I push upon you, but yet another straight (?) white guy called [Edward Tufte](https://en.wikipedia.org/wiki/Edward_Tufte) who wrote
the book [The Visual Display of Quantitative Information](https://www.edwardtufte.com/tufte/books_vdqi) and had a
lot of great ideas of how to make plots more readable. However, if you find that these restrictions are keeping you
from doing something incredible vital, then lets talk about it and see if it makes sense to allow it.

### What does the api look like?

Something like this:

```elm
    main =
      viewSeries
        [ area (List.map (\{ x, y } -> circle x y)) ]
        [ { x = 0, y = 1 }
        , { x = 2, y = 2 }
        , { x = 3, y = 3 }
        , { x = 4, y = 5 }
        , { x = 5, y = 8 }
        ]
```

You're welcome to take a look at the docs folder for more [examples](https://github.com/terezka/elm-plot/tree/master/docs/src)!

### Missing something?

Let me know! Open an issue (or PR) or write in [slack](https://elmlang.slack.com/messages/elm-plot). Please don't hesitate, I'm happy to answer any questions or receive feedback!

## Development

### Setup

```elm
elm package install
elm-reactor
```

and open [docs](http://localhost:8000/docs/Docs.elm) (The docs contain a bunch of examples convenient for developing).

### Compile the Docs

```
elm-live docs/src/Docs.elm --output=docs/docs.js
```

### Tests

Tests are written with [elm-test](https://github.com/elm-community/elm-test).
For further information on elm-test check the documentation.
All required dependencies are downloaded and installed when initially running the command.

```
elm-test
```
