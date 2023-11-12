# mathyl

Sigh... It has come to this again. I have to build another thing myself because all existing solutions are lacking in some dimension. This time it's a static site generator. Apparently it's already a meme that everyone and their grandma is building their own SSG, which makes it all the more surprising that none of the hundreds of frameworks I tried to day fit my needs. 

I want to write a mathematics-heavy blog, so I need extensive LaTeX support &ndash; including the occasional tikZ drawing. Moreover, I want the actual writing to be as easy as possible, so Markdown based blog posts are a must. I would like to avoid any JavaScript in the output and don't want to touch Python with a 10-foot pole. 

This is my attempt to satisfy these demands ...written in Haskell of course :)

## Features

## Usage

```
mathyl build <in path> <out path>
```
Copies the directory at <in path> to <out path>, filling templates and rendering LaTeX and tikZ elements.

## Implementation TODOs

* [ ] Parse Markdown files, extract tikz drawings and metadata
* [ ] Handle math & markdown compilation with pandoc
* [ ] Handle tikZ drawings with locally installed version of latex and `standalone` package
* [ ] Implement template filling with stache
