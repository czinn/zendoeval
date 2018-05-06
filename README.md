# Zendo Rule Evaluation in Lojban

I wrote a [blog post](http://charleszinn.ca/blog/evaluating-zendo-rules/) about this project.

`cabal run zendoeval` will build and run the game. The available commands
`build [koan]`, `guess [rule]`, and `end` (give up). Koans are printed and
input using an abbreviated format:

    r3          -- a large red pyramid
    r3b1        -- a large red with a small blue on top
    r3b1 b3g2y1 -- two adjacent and touching stacks
    g2> y1      -- a medium green pointing at and touching a small yellow
    g2> | y1    -- a medium green pointing at but not touching a small yellow

For information about how to write rules, see my [other blog
post](http://charleszinn.ca/blog/lojban-zendo/) about how to write Zendo rules
in Lojban.
