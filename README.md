# Fractal Language

## Presentation
  Fractal is a Lisp Like, interpreted, programming language. Its purpose is to be a simple toy language which will be usable as a Discord Bot.

When used as a bot, you will be able to try some functions directly on Discord.

Don't forget that I make this project in order to learn Haskell. My code isn't an example.

## Syntax exemples

Basic expressions :
**All of these will change**

```lisp
-- Simple commentary

(print "hello" "world")
(+ (* 5 2) 3)
```

If expression :

```lisp
(If [(isEq 5 3)] {
  (print "True")
} {
  (print "False")
})
```

Define names and functions

```lisp
(Define functionName [arg1 arg2 arg3] {
  (print arg1 arg2 arg3)
})
```

```lisp
(Define name [] { '("some" "values") })
```