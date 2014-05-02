# Trimming and down-casing strings with AngularJS Filters: lowercase and limitTo

I've got a small `ng-repeat` over this array:

    $scope.tabs = [
        { label: "Features" },
        { label: "Phrases" },
        { label: "Sets"}
    ];

I need the labels to be capitalized in at least one place, as I'm using
them as tab headers via [UI-Bootstrap's Tab component][tab]. But, I'd
like to also use the same text lowercased and in their singular forms,
so I can have [placeholder text][] like "search for your feature
files". Enter the `lowercase` and `limitTo` filters!

`lowercase` does pretty much what you'd expect:

    {{ "Features" | lowercase }} => features

and on strings, `limitTo:integer` trims the string length to the
specified integer argument. I tried putting an expression in as the
argument to `limitTo` on a whim, and it worked wonderfully ([plnkr][]):

    {{ "Features" | lowercase | limitTo: "Features".length - 1 }} => feature

You can of course replace "Features" with a variable on your scope to
make that more versatile - my expression with the array above ended up
being:

    {{ tab.label | lowercase | limitTo: tab.label.length - 1 }}

N.B. - make sure you've got your scope
[inheritance sorted when you're using `ng-repeat`][scope] - bind to
objects, not primitives :)

[scope]: https://github.com/angular/angular.js/wiki/Understanding-Scopes#ng-repeat
[plnkr]: http://plnkr.co/edit/Lfg897uxorHno5QpU1VI?p=preview
[tab]: http://angular-ui.github.io/bootstrap/#/tabs
[placeholder text]: https://github.com/gempesaw/honeydew-ng/blob/4e81a41a602b606bfcefd80db07113e11ecd4c24/app/components/filetree/filetree.html#L9
