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

    my $d = Selenium::Firefox->new;
    $d->get($tricky_slow_loading_page);

    # this will throw when the element isn't present
    my $text = $d->find_element_by_css('div')->get_text

The problem with putting in a sleep before attempting to find the
element is that it will usually work, but maybe one time in twenty it
will fail, and it won't be immediately apparent why it failed,
especially months down the line. What we want is a method that
reliably and consistently waits exactly until the element in question
is ready. We don't want to wait any longer than necessary, so a long
explicit sleep is out, but we don't want to go early, or else we'll
get exceptions all over the place. `wait_until` lets us get pretty
close to this ideal behavior:

    # wait_until will also catch dies and croaks
    my $elem = wait_until { $d->find_element_by_css('div') };
    if ($elem) {
        say 'Text: ' . $elem->get_text;
    }
    else {
        say 'We waited thirty seconds without finding css=div';
    }

`wait_until` takes a block and an optional hashref of arguments. It
wraps the block execution in a `try`/`catch` from [Try::Tiny][]. By
[default][], it will run for thirty seconds, sleeping one second
between iterations. If at any point the block returns something true,
it immediately returns that value as its result. Note that
`wait_until` expects a block that is generally NON-blocking, so if
webdriver has an associated timeout, like the implicit wait timeout
for finding elements, you'll want to set it to a second or less if
you've increased it. The exact number of iterations will depend on how
long the block takes to execute.

To be clear, if your `implicit_wait_time` is 31 seconds, and you put a
`find_element` inside a `wait_until`, we'll run it once, Webdriver
itself will block for 31 seconds, and by the time we get control back
in our `wait_until` block, the timeout will have expired, and we'll
return control to your script after executing exactly one
`find_element`. (This may be the behavior you desire - but just make
sure you're doing it on purpose!)

    my $d = Selenium::Firefox->new;
    $d->set_implicit_wait_timeout(30000);
    my $one_iteration = wait_until { $d->find_element('this is blocking', 'css') };

You can also use `wait_until` to have your test block until the
element is visible, or some other boolean property:

    my $visible_elem = wait_until {
        $d->find_element_by_id('eventually-visible')->is_displayed
    };

Finally, as mentioned, `wait_until` wraps everything in a `try`, so if
the BLOCK you passed in does die, it'll get demoted to a warn. This
means you MUST check the return value of `wait_until`. Normally,
Selenium::Remote::Driver will croak when we run into something we
don't understand [^1]. Since we're only warning, it's possible you may
get into some weird territory if you make assumptions about the return
value. Honestly, I'm still not sure about this behavior - perhaps it
makes sense for `wait_until` to die if the expected value never
returns true.

[^1]: Although it can be frustrating, it's helpful for the
program to die as close as possible to the source of the crash. Also,
from the test's point of view, we have no idea what to do if we get an
unexpected exception.

[halting problem]: https://groups.google.com/forum/#!msg/webdriver/7K2QWGVNCYo/PngL9YDXDLgJ
[Selenium::Waiter]: https://metacpan.org/pod/Selenium::Waiter
[default]: https://metacpan.org/pod/Selenium::Waiter#Timeouts-and-Intervals
[Try::Tiny]: https://metacpan.org/pod/Try::Tiny
