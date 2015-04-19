We've just released version 0.25 of Selenium::Remote::Driver to the
various CPANs. The big push this time around was to get around our
hard dependency on the JRE. Previously, the Perl bindings demanded a
standalone selenium server be operating on the browser's machine. So,
if you wanted to run tests on your own box, you'd need the Java
Runtime Environment installed, as the selenium-standalone-server is a
`.jar` and needs the JRE to execute. However, as [akafred][] pointed
out, this is a prohibitive constraint (perhaps especially so for Perl
programmers?).

When I first started out working with Webdriver years ago, I only used
the standalone server and for a while I thought there was no other way
to run the tests. Eventually, when various webdrivers began replacing
Selenium RC, I found out that it was possible to "talk" directly to
them with the same exact API as the standalone server, but I never
connected that with the ability to avoid needing the standalone server
and the JRE!

Anyway, thanks to some long-awaited prodding, Perl can now run
[Firefox][], [Chrome][], and [PhantomJS][] without the JRE! What follows are
some implementation details, and simple usage examples. :)

[akafred]: https://github.com/gempesaw/Selenium-Remote-Driver/pull/189
[Firefox]: https://metacpan.org/pod/Selenium::Firefox
[Chrome]: https://metacpan.org/pod/Selenium::Chrome
[PhantomJS]: https://metacpan.org/pod/Selenium::PhantomJS

[[MORE]]

### usage

Hopefully the usage is pretty straightforward - instead of
constructing a `Selenium::Remote::Driver` instance, use the
constructor for the browser of your choice instead. Just like the
Selenium standalone server, Firefox will work out of the box, as long
as you have Firefox installed locally on your machine in the default
location:

    my $firefox = Selenium::Firefox->new;
    $firefox->get('http://www.mozilla.org');

    # "We’re building a better Internet — Mozilla"
    print $firefox->get_title;

The usage is exactly analogous for the ::Chrome and ::PhantomJS
classes, except you need to have the browser installed along with its
associated webdriver. For PhantomJS, GhostDriver is automatically
bundled for all recent versions, but for Chrome, you'll need to
[download the Chromedriver separately][].

[download the Chromedriver separately]: https://sites.google.com/a/chromium.org/chromedriver/downloads

If your driver executables are not in your $PATH, or you'd like to
specify a certain one, the constructors take a `binary` option that
lets you tell us what executable to start. You can also specify a
`binary_port` if there's a specific port that you'd like the webdriver
server to bind to.

    my $firefox = Selenium::Firefox->new(
        binary => '/custom/path/to/firefox-bin',
        binary_port => 65432
    );

As expected, usage is analogous for the other two classes.

As a last note, we skipped creation of Selenium::InternetExplorer
hoping that [YAGNI][], but if that's the browser that floats your boat,
we can definitely throw a class together for the next release. Let us
know in our [Google group][], or in the [Github issues][]!

[YAGNI]: https://www.google.com/search?q=yagni
[Google group]: https://groups.google.com/forum/#!forum/selenium-remote-driver
[Github issues]: https://github.com/gempesaw/Selenium-Remote-Driver/issues

### implementation

The implementation is ideally pretty straightforward:

1. Find the binary webdriver
2. Figure out what arguments to pass it to make it mimic a standalone
server
3. Start it on the right port
4. Afterwards, clean it up so we don't orphan processes

The first step is easy - just check the `$PATH`/`%PATH%` - exceeept
for Firefox, where it's apparenty quite complicated. Different
operating systems install it in multiple distinctly different places,
and if different versions of Firefox are named differently - basically
it's seems like a big headache, and I mostly threw my hands up if the
Firefox binary wasn't in the first specific default location. There's
also a bit of extra complication involved to let the user choose their
own path, and validating it for them afterwards.

Passing the arguments is more or less straightforward as well -
exceeept for Firefox, which doesn't take arguments, as the webdriver
for Firefox is actually just a Firefox extension. Luckily, we
previously implemented a Firefox::Profile class, so we just use the
newly renamed [Selenium::Firefox::Profile][] to create a profile with
the extension loaded. We actually now bundle the `webdriver.xpi`
extension from the 2.45.0 version of Selenium in our release, and each
time a new Selenium is released, we'll have to do a mirror release of
our bindings with the updated extension. The other webdriver language
bindings also start up Firefox with a few pre-compiled `.so` files for
solving focus errors that I was also too lazy to do.

Finding an open port is simple enough, and we got to re-use
Selenium::Waiter's [wait_until][] in a few places.  And, starting the
webdriver up is usually easy, although at first I was a bit unfamiliar
using `system` to start up an asynchronous process across different
platforms (namely Windows...).

Finally, cleaning up the process we started is straightforward on OS X
and Linux, as I _think_ it just cleans itself up when the Perl script
ends. At least, that's what some superficial tests seemed to prove - I
didn't see anything in `ps aux` after the test was over, and I
couldn't open sockets to the server port afterwards. But, on Windows,
the task stays open and we need to kill it by string-matching the
title of the process. It ended up being a bit of a mess!

We went through a bunch of attempts & refactors at organizing the
functionality and ended up at something that's decently organized -
the [Selenium::CanStartBinary][] role implements most of the common work
between the different classes. Since Firefox is special, it gets a few
extra classes of its own. Meanwhile, each class holds its own
arguments, default binary name, and default binary port.

[wait_until]: https://metacpan.org/pod/Selenium::Waiter#wait_until
[Selenium::Firefox::Profile]: https://metacpan.org/pod/Selenium::Firefox::Profile
[Selenium::CanStartBinary]: https://metacpan.org/pod/Selenium::CanStartBinary

### conclusion

For a first pass, I think the functionality works pretty decently - as
usual, I've been using it locally for a month or two now on OS X with
no major issues, but I'm sure I missed a few bugs. If you run across
any bugs, definitely [let us know][] or even fix them for us, since I
bet you can do it better than me! :D Cheers...

[let us know]: https://github.com/gempesaw/Selenium-Remote-Driver/issues
