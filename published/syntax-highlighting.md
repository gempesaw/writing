Syntax highlighting on tumblr is a completely manual effort, as far as I'm aware. I don't know of any built-in highlighting or any certified methods that they advertise. I did some research and tried a couple different highlighters - some of them are out of date, some of them don't support lisp, and some of them are more complicated. I was looking for a decently recent one that had built-in lisp support and would be easiest to hook into a tumblr set up. I settled on [highlight.js](http://softwaremaniacs.org/soft/highlight/en/), which has helpful documentation on its own website and everything.

Because I needed somewhere to host the files, I got set up with Github Pages and put my versions of the highlight.pack.js file in there. That took a while, but once it was sorted, the rest was pretty straight forward. I just need to select and add a CSS stylesheet with the colors of my choosing, add one script tag to source the js file that does the work, and then call javascript command to do all the highlighting. So, the following lines have been added to my tumblr theme:

    <link rel="stylesheet" href="http://gempesaw.github.com/stylesheets/ir_black.css" />
    <script type="text/javascript" src="http://gempesaw.github.com/javascripts/highlight.pack.js"></script>
    <script type="text/javascript">hljs.initHighlightingOnLoad();</script>

Stylesheet, highlight.js, and activation. A fancy part of highlight.js is that it has language autodetection, so if it figures out I'm writing Lisp on its own, that's great! Sometimes, it misses though, and I have to manually wrap my source code in `<pre><code class="lisp">source-code</code></pre>` tags, which isn't that bad.
