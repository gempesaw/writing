<small>(This post is direct language translation of
[ep 39](http://elementalselenium.com/tips/39-drag-and-drop) of
[Elemental Selenium](http://elementalselenium.com/), Dave Haeffner's
Ruby series about Webdriver. Go check 'em out, even if you don't use
Ruby, as Webdriver concepts are the same across bindings. We're even
reusing his fixture code, as it's quite useful!)</small>

Although the webdriver protocol offers a few endpoints that allow you
to accomplish drag and drop ([button_down][],
[mouse_move_to_location][], and [button_up][]), there's an
[outstanding bug in webdriver for those endpoints on HTML5 pages.](https://code.google.com/p/selenium/issues/detail?id=6315)
So, there's nothing we can do from the side of the Perl bindings, as
invoking the endpoints won't actually do the drag and drop. Luckily,
there's a javascript workaround that allows us to
[simulate drag and drop events][] with jQuery (or Zepto), courtesy of
Rob Correia's gist.

[button_up]: https://metacpan.org/pod/Selenium::Remote::Driver#button_up
[button_down]: https://metacpan.org/pod/Selenium::Remote::Driver#button_down
[mouse_move_to_location]: https://metacpan.org/pod/Selenium::Remote::Driver#mouse_move_to_location
[simulate drag and drop events]: https://gist.github.com/rcorreia/2362544

[[MORE]]

First things first, you'll need the helper javascript downloaded
somewhere - you can use `__FILE__` or
[`FindBin`](https://metacpan.org/pod/FindBin) to locate it relative to
your perl script. Assuming you save the helper js as `drag.js` in the
same directory as your test script, your test might look like:

    #! /usr/bin/perl

    use strict;
    use warnings;
    use FindBin;
    use Selenium::Remote::Driver;
    use Test::More;

    my $d = Selenium::Remote::Driver->new;

    $d->get('http://the-internet.herokuapp.com/drag_and_drop');

    my $grab = $d->find_element('column-a', 'id');
    my $target = $d->find_element('column-b', 'id');

    my $drag_file = $FindBin::Bin . '/drag.js';
    open (my $fh, "<", $drag_file);
    my $drag_js = join('', <$fh>);
    close ($fh);

    my $simulate_js = '$(arguments[0]).simulateDragDrop({ dropTarget: arguments[1] })';
    $d->execute_script( $drag_js . $simulate_js, $grab, $target);

    is($grab->get_text, 'B', 'text in column-a has changed!');
    is($target->get_text, 'A', 'text in column-b has changed!');

    $d->quit;

    done_testing;

This is accomplishing exactly the same thing as the Ruby version of
the script. We load up our driver, get to our test page, and then
we're deviating slightly. I prefer to find the elements in webdriver
separately and pass them into the `execute_script`, as my framework
allows users to locate elements in various with multiple `by`
strategies, so we'd need account for that deviation. Luckily,
`execute_script` puts any additional arguments it receives into the
special `arguments` array in javascript, transforming them to and from
actual DOM elements if necessary. So our `$grab` and `$target` are
available in their DOM element form as `arguments[0]` and
`arguments[1]`, respectively.

The final step is to concatenate the tiny drag library with the
javascript to simulate the drag and drop. After that, we just verify
that the text in the columns changed, and we're good to go!

I'm wondering if I should include the `$drag_js` in
Selenium::Remote::Driver somewhere. We recently found a deprecated
`drag` method on our WebElement class - since the `drag` endpoint no
longer exists in the JSONWireProtocol, we've begun the process to
deprecate it in our module. However, the aforementioned workaround
means that we could restore our binding's `drag` method with this hack
until an official fix comes in. But, the official fix might lead to a
change in API that would confuse our users.

On the one hand, it always feels a bit icky to include javascript in a
perl module. On the other hand, drag and drop is pretty commonly
requested and it could be useful to many users. I think I'll leave it
out of our library until there's an explicit need for it, and point
people to this method until then.

<div class="drag">
  <h4>Drag and Drop Test Fixture</h4>
  <div id="columns">
    <div class="column" id="column-a" draggable="true"><header>A</header></div>
    <div class="column" id="column-b" draggable="true"><header>B</header></div>
  </div>
</div>
<br>

<style>
.drag .columns {
  width: 400px;
}
.drag .column {
  height: 150px;
  width: 150px;
  float: left;
  border: 2px solid #666666;
  background-color: #ccc;
  margin-right: 5px;
  text-align: center;
  cursor: move;
}
.drag .column.over {
  border: 2px dashed #000;
}

</style>

<script>
var dragSrcEl = null;

function handleDragStart(e) {
  this.style.opacity = '0.4';

  dragSrcEl = this;

  e.dataTransfer.effectAllowed = 'move';
  e.dataTransfer.setData('text/html', this.innerHTML);
}

function handleDragOver(e) {
  if (e.preventDefault) {
    e.preventDefault();
  }

  e.dataTransfer.dropEffect = 'move';

  return false;
}

function handleDragEnter(e) {
  this.classList.add('over');
}

function handleDragLeave(e) {
  this.classList.remove('over');
}

function handleDrop(e) {
  if (e.stopPropagation) {
    e.stopPropagation();
  }

  if (dragSrcEl != this) {
    dragSrcEl.innerHTML = this.innerHTML;
    this.innerHTML = e.dataTransfer.getData('text/html');
  }

  return false;
}

function handleDragEnd(e) {
  [].forEach.call(cols, function (col) {
    col.classList.remove('over');
  });
  this.style.opacity = '1';
}

var cols = document.querySelectorAll('#columns .column');
[].forEach.call(cols, function(col) {
  col.addEventListener('dragstart', handleDragStart, false);
  col.addEventListener('dragenter', handleDragEnter, false);
  col.addEventListener('dragover', handleDragOver, false);
  col.addEventListener('dragleave', handleDragLeave, false);
  col.addEventListener('drop', handleDrop, false);
  col.addEventListener('dragend', handleDragEnd, false);
});
</script>
