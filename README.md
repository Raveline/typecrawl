# Typecrawl : a basic blog scrapper

If you are like me and you live in a city where there is almost no Internet
connexion in the subway, you know the frustration it can lead to when you're
reading your favourite blog.

This scrapper lets you get a local copy of blogs, should you need it. Note that
distributing scrapped content like this is most likely not legal, so please use
this script like the responsible adult you most likely are.

The current version mostly support typepad blogs, but there are experimental
scrappers for Blogger and some WordPress blogs hosted by a specific French
newspapers that share attributes allowing to scrape them. It's fairly easy
to add new scrappers though, should you need it.

## Usage

    typecrawl <url of the blog> <scrapper name> <output file> --depth <number of pages you want to scrap>

If you're running through stack exec, remember the syntax for args:

    stack exec -- typecrawl <url> <scrapper> <output> --depth <int>

Or the --depth parameter won't be properly parsed.

Blogs usually have a chronological presentation of posts. The logic behind
this scrapper is to visit each page, take all post links available, then
look for the link to the next page, and iterate as much time as the
depth parameter.

The result will be fused in one html page (the output file).

Note that you can make a PDF out of this page, using Pandoc and LaTeX, with
a simple:

    pandoc <the former output file> -o result.pdf

## Future improvements

This is obviously a toy project, that let me use two cool libraries: Scalpel and
Pipes. There are probably several instances where this script does not work.

But I'd love to improve it in several ways :

- Currently, images are not downloaded. This is probably the feature I'd like to
  add first.

- Be able to scrap the *whole* blog. (Pretty easy to do)

- It's fairly slow right now, mostly because my curl calls are sequential.
  Multithreading this would be cool. (I have no idea how to do this in Haskell,
  though).

- Integrating directly the PDF output would also be nice, but I need to learn
  how to use the Pandoc lib for this.
