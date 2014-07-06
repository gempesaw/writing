# An AngularJS File Tree ( with a PHP backend )

Something I needed in my AngularJS app was a file tree that exposed a
few folders full of flat files to the user. At the beginning of 2014,
there were only a few google groups [posts][] about trees in angular -
apparently the recursive nature of the tree structure made it
complicated to express in the form of directives. Luckily, there are
now a few [competing][] [file][] [tree][] [projects][] available. The
one I ended up going with is [wix's][] [angular-tree-control][] for a
few reasons. I'll go over a few of those reasons, walk through the
frontend usage, and also detail the backend PHP code that generates
the tree in the right format that `angular-tree-control` can
appropriately consume.

[posts]: https://groups.google.com/forum/#!msg/angular/TbpjE-5XEM0/yUi8wqc7sWoJ
[competing]: https://github.com/eu81273/angular.treeview
[file]: https://github.com/jdewit/ez-file-tree
[tree]: http://nickperkinslondon.github.io/angular-bootstrap-nav-tree/test/bs2_ng115_test_page.html
[projects]: http://jsfiddle.net/brendanowen/uXbn6/8/
[Slim]: http://www.slimframework.com/
[wix's]: https://github.com/wix
[angular-tree-control]: http://github.com/wix/angular-tree-control/

[[MORE]]

### 1. reasons

I went with `angular-tree-control` primarily because its light theme
looked the best in that it closely matched the old filetree that this
one was replacing. Most of the other projects weren't themed
appropriately (or at all) for my project. Although I have a decent
grasp of CSS, I'd much rather leave the styling to some one else and
save myself the time. Additionally, it had the valuable built-in
option of clicking on the label to expand/collapse the children of a
node. This again was a constraint that came from the previous
implementation, and I was aiming to replicate the experience as much
as possible so as not to disrupt my users during the
transition. Finally, the documentation for this project was very good,
complete with a great [example page][] that mimics the official AngularJS
docs. And, as an added bonus, it's still under current development,
with the author merging PRs in a timely fashion and recently making a
few new commits last week.

[example page]: http://wix.github.io/angular-tree-control/

### 2. front-end usage

So. Getting the tree into place is [pretty straightforward][]:

[pretty straightforward]: https://github.com/gempesaw/honeydew-ng/blob/master/app/components/filetree/filetree.html#L28-L34

```html
<treecontrol class="tree-classic"
             tree-model="tab.data"
             options="treeOptions"
             on-selection="tree.show(node)">
  {{ node.label }}
</treecontrol>
```

`treecontrol` is the name of the directive that we can drop into our
html template. The `class` determines its theme; `tree-model`
indicates where on the scope our model. `options` is the object on our
scope that specifies the options, and `on-selection` is our on-click
handler. Finally, the content of the `<treecontrol>` element becomes
the label for each node.

Each node is of the format

```js
{
    label: "aBranch",
    children: [
        {
            label: "aLeaf",
            children: []
        }
    ]
}
```

If the `children` array is empty, then the node is displayed as a
leaf/file; otherwise it's a branch/folder.

Getting into our controller, we have a few things to set up:
