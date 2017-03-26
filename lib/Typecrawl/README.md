# Typecrawl : a basic typepad blog scrapper

If you are like me and you live in a city where there is almost no Internet
connexion in the subway, you know the frustration it can lead to when you're
reading your favourite blog.

This scrapper lets you get a local copy of this very blog, should you need it.
Note that distributing scrapped content like this is most likely not legal, so
please use this script like the responsible adult you most likely are.

## Usage

    typecrawl <url of the blog> <output file> --depth <number of pages you want to scrap>

If you're running through stack exec, remember the syntax for args:

    stack exec -- typecrawl <url> <output> --depth <int>

Or the --depth parameter won't be properly parsed.

Typepad blogs tend to have pagination. This scrapper will iterate on as many
page as the --depth option. For each page, it will pick every post address, then
visit each post and get its content.

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

- Of course, the ability to scrap other platforms (Blogger, Worpdress...) would
  be nice, but it's much easier on standardized platforms.

- Be able to scrap the *whole* blog. (Pretty easy to do)

- It's fairly slow right now, mostly because my curl calls are sequential.
  Multithreading this would be cool. (I have no idea how to do this in Haskell,
  though).
