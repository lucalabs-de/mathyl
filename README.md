<h3 align="center">mathyl</h3>
Yeah, yeah, I know &ndash; it's another static site generator. Even though it seems that everyone has to write one, surprisingly none of them fit my requirements.
I want to write a mathematics-heavy blog, so I need extensive LaTeX support &ndash; including the occasional tikZ drawing. Moreover, I want the actual writing to be as easy as possible, so Markdown based blog posts are a must. I would like to avoid any JavaScript in the output and don't want to touch Python with a 10-foot pole. 


This is my attempt to satisfy these demands ...written in Haskell of course :)

### Features
- HTML generation from Markdown files 
- Rendering of LaTeX formulas using KaTeX
- Server-side rendering of tikZ pictures 

I mainly wrote mathyl for my personal use, but PRs are always welcome.

### Usage

```
mathyl build <in path> <out path>
```
Compiles your blog.

```
mathyl preview <in path> [out path]
```
Starts a local preview at at 127.0.0.1:8080. Optionally stores your blog files at [out path]. 

### Implementation TODOs

* [ ] Implement options 
    - [x] --use-svgs
    - [ ] --server-side-rendering
    - [ ] --continue-on-errors
    - [ ] --nice-urls

### Future Plans
* [ ] Option to continue compilation on errors (--continue-on-errors)
* [ ] Optional server side rendering for KaTeX formulas (--server-side-rendering)
* [ ] Option to make URLs nicer (`blog.tld/posts/test-post` instead of `blog.tld/posts/test-post.html`)
