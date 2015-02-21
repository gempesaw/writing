# Waiting for a page to load in Selenium::Remote::Driver

One of the first problems we come up against when trying to use
Webdriver to automate a web application is handling the asynchronicity
of loading a web page. For the test to be useful at all, it needs to
be as fast and reliable as possible, or else there's a very real risk
that devs and QA engineers will give up running the tests. The problem
is that making the test run faster can sacrifice reliability if the
async javascript isn't properly handled.

Instead of trying to figure out when a page loads, we should take a
hint from what an actual user does. Users don't care when a page is
done loading - they just wait for the exact element they want to
interact with, and then start clicking/inputting right away. We can
mimic that behavior with `wait_until`, a utility function exported by
[Selenium::Waiter][].

[[MORE]]

The general case of determining when a page is done loading is a
variation of the [halting problem][]. When you ask Webdriver to load a
page, it does block briefly while it runs through its own algorithms
to figure out when the page is done loading. But, it being the halting
problem and all, they obviously can't solve it for all the cases.

```perl
use Selenium::Firefox;
my $d = Selenium::Firefox->new;
$d->get('http://tricky.slow-loading.page');

# this will throw when the element isn't present
my $text = $d->find_element_by_css('div')->get_text
```

The problem with putting in a sleep before attempting to find the
element is that it will usually work, but maybe one time in twenty it
will fail, and it won't be immediately apparent why it failed,
especially months down the line.

```perl
my $elem = wait_until { $d->find_element_by_css('div') };
if ($elem) {
    say 'Text: ' . $elem->get_text;
}
else {
    say 'We waited thirty seconds without finding css=div';
}
```

`wait_until` takes a block and an optional hashref of arguments. It
wraps the block execution in a `try`/`catch` from [Try::Tiny][]. By
[default][], it will run for thirty seconds, sleeping one second
between iterations. If at any point the block returns something true,
it immediately returns that value as its result. Note that
`wait_until` expects a block that is generally NON-blocking, so you'll
want to set the appropriate timeout to a second or less.

You can also use wait_until to do have your test block until the
element is visible, or some other boolean property:

```perl
my $visible_elem = wait_until {
    $d->find_element_by_id('eventually-visible')->is_displayed
};
```

[halting problem]: https://groups.google.com/forum/#!msg/webdriver/7K2QWGVNCYo/PngL9YDXDLgJ
[Selenium::Waiter]: https://metacpan.org/pod/Selenium::Waiter
[default]: https://metacpan.org/pod/Selenium::Waiter#Timeouts-and-Intervals
[Try::Tiny]: https://metacpan.org/pod/Try::Tiny
