# The PHP backend!

The front end that we detailed previously for making an AngularJS file
tree wouldn't be of any use without a complementary backend. Ours
happens to be in PHP where we're using the
[Slim framework][slim]. Slim a small RESTful framework that enables us
to write endpoints with anonymous callbacks, like

```php
$app->get('/example', function () {
    echo 'example endpoint';
});
```

For our particular case, our AngularJS `$resource` has a `.get()`
method on it that expects an object, so we our PHP fn has to end up
echoing something like[^1].

```php
echo json\_encode(array("tree" => $tree\_structure));
```

To construct our tree, we use a recursive function that builds up the
tree bit by bit. It's available in all of its "glory"
[in the repo][repo], and we'll split it up and go bit by bit here.

We start by accepting a directory that we're trying to list,
`$start_dir`, which defaults to the current working directory `.`. We
open the directory and start looping through it, skipping the
uninteresting cases when the `$file` is `.` or `..`.

```php
function list\_files\_in\_dir( $start_dir = '.' ) {
    $files = array();

    if ($fh = opendir( $start_dir )) {
        while(($file = readdir( $fh )) !== false){
            /* loop through the files, skipping . and .. */
            if (strcmp($file, '.')==0 || strcmp($file, '..')==0) {
                continue;
            }
```

Otherwise, `$file` is something interesting, so let's suffix it to the
`$start_dir` to get its full filepath and get to work. We create a
potential candidate whose label is the file's relative name, and then
branch our logic depending on whether or not the file is a directory
or not.

```php
            $filepath = $start_dir . '/' . $file;

            $candidate = array(
                'label' => $file,
            );
```

If it's a directory, we need to use this very function to get the
branches and leaves that are inside of it, so we call ourself to do
that, assuming we've properly written this function. When we're
finished calling ourselves, we put a guard to check that the array of
children files returned isn't empty.

Since the filetree plugin displays nodes without children as files, an
empty directory is indistinguishable from an actual leaf; since we
have no use for an empty directory, there's no need to add it.

```php
            if (is_dir( $filepath )) {
                $children = list\_files\_in\_dir($filepath, $basedir);

                /* exclude empty folders */
                if (!empty($children)) {
                    $candidate['children'] = $children;
                    $files[] = $candidate;
                }
            }
```

Assuming the directory has children, we can put the `$children` on to
our `$candidate` from before, and then put our `$candidate` onto our
own `$files` array that we'll eventually return.

Going back a few paragraphs, we next have to process the case where
`$file` is actually a file, and not a directory. By definition, these
nodes have no children, so we can set that to an empty array. We also
want to add a `folder` property to the `$candidate` so the front-end
has enough metadata to act intelligently when a node is
clicked. Otherwise, the only information available when clicking on a
node would be its label/relative file name - not enough to figure out
what to do.

```php
            else {
                if (endswith($file, "feature")
                    || endswith($file, "phrase")
                    || endswith($file, "set")) {
                    $candidate['children'] = array();

                    $candidate['folder'] = $start_dir;
                    $files[] = $candidate;
                }
```

My logic also includes a guard to only add files with the `.feature`,
`.phrase`, or `.set` file extensions, but those are obviously
customizable.

Finish off by closing the filehandle on the open directory, and
handling the case where `$start_dir` isn't a directory at all.

```php
            }
        closedir($fh);
    }
    else {
        $files = false;
    }

```

The last thing to do is to optionally sort the $files by label before
we return them. This isn't absolutely necessary, but since the
previous implementation sorted the files as 0-9A-Za-z, we'll do the
same thing by sorting on each file's `label` property:

```php
    usort($files, function ($a, $b) {
        return strcmp($a['label'], $b['label']);
    });

    return $files;
```

PHP's `usort` mutates the existing array, so we don't return the
output of `usort`; we just return `$files` after invoking it. It feels
a bit gross, but what can ya do.

All in all, a pretty straightforward recursive function. For the Slim
side, we do:

```
$app->get('/tree/:folder+', function ($folder) use ($app) {
    try {
        $folder = implode("/", $folder);
        $tree = list\_files\_in\_dir($folder);
        echo successMessage(array("tree" => $tree));
    }
    catch (Exception $e) {
        $app->halt(418, errorMessage($e->getMessage()));
    };
});
```

This sets up an endpoint at /tree/:folder and lists the desired
directory - the end point accepts different directories, or a path
with multiple folders in it, and it returns the listing for any of
them. In actual usage, we're prefixing a base directory to all
requests so users can't request directories under `/` or anything like
that.

Note that you probably shouldn't halt with 418 in production, as it
might be nonstandard, and it's definitely not helpful. Also,
`successMessage()` and `errorMessage()` are just helper functions that
wrap up the json_encode call and some other items for us so we don't
have to repeat that all over the place.

[slim]: http://www.slimframework.com
[repo]: http://github.com/gempesaw/honeydew-ng/
[^1]: We could just as easily use $resource's `.query()` method and
just echo the json encoded `$tree_structure`.
