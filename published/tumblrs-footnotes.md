# Stopping footnotes from opening in a new tab

Tumblr's markdown formatting mode somewhat secretly supports
footnotes. But, it seems like my settings or my theme or something
makes footnote links with the `target="_blank"` attribute set, which
is pretty odd. Who wants a footnote to pop them to a new tab ? And
furthermore, the return links in the footer also have the same
`target="_blank"`. Basically, the footnotes on this blog have been
nigh unusable, since they keep spawning new tabs all over the place.

So, some quick javascript to get things sorted:

    Array.prototype.slice.call(
        document.querySelectorAll( 'a[rel=footnote], a[rev=footnote]' )
    ).forEach( function (node) { node.target = ''; } );

- find the impacted nodes with `document.querySelectorAll`
- convert that NodeList to an Array
- clear the `target` on each node

Honestly, I'm not entirely sure why this happens on this blog -
[other][] [people][] [don't][] seem to have the issue. In case anyone else
is seeing this behavior on their tumblr footnotes, just add the code
above to a `<script></script>` tag at the bottom of the HTML for the
page :)

[other]: http://www.marco.org/tagged-bestof#fnref:pcclmCSDW1
[people]: http://onethingwell.org/post/1680780219/tumblr-markdown#fnref:p1680780219-1
[don't]: http://nancym.tumblr.com/post/59594358553/links-footnotes-and-abbreviations-in-markdown#fnref:p59594358553-markdown
