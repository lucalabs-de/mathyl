# Unnamed SSG

Sigh... It has come to this again. I have to build another thing myself because all existing solutions are lacking in some dimension. This time it's a static site generator. Apparently it's already a meme that everyone and their grandma is building their own SSG, which makes it all the more surprising that none of the hundreds of frameworks I tried to day fit my needs. 

I want to write a mathematics-heavy blog, so I need extensive LaTeX support -- including the occasional tikZ drawing. Moreover, I want the actual writing to be as easy as possible, so Markdown based blog posts are a must. I would like to avoid any JavaScript in the output and don't want to touch Python with a 10-foot pole. 

This is my attempt to satisfy these demands ...written in Haskell of course :)

## Implementation TODOs

* [ ] Parse Markdown files, extract tikz drawings 
* [ ] Handle math & markdown compilation with pandoc
* [ ] Handle tikZ drawings with locally installed version of latex and `standalone` package

