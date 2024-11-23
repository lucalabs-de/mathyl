<h3 align="center">mathyl</h3>

<p align="justify">
mathyl is a batteries-included static site generator. You define your site structure in terms of directories and write your content in markdown files. mathyl then simply mirrors the directory structure, compiling all markdown files to HTML along the way. You can define HTML templates for your content using mustache.
</p>

<p align="justify">
The eponymous features of mathyl are extensive LaTeX support based on KaTeX and automatic compilation of tikZ images (either to .png or vector graphics). Its output also won't contain any JavaScript, unless your templates contain it.
</p>

Although there are probably hundreds of static site generators around, I couldn't find any that support this exact feature set, so I wrote mathyl *...in Haskell of course :)*

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

### Future Plans
* [ ] Option to continue compilation on errors (--continue-on-errors)
* [x] Option to make URLs nicer (`blog.tld/posts/test-post` instead of `blog.tld/posts/test-post.html`, --nice-urls)
