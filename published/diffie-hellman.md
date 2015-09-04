# Chromedriver and the Weak Ephemeral Diffie-Hellman Public Key

As of Chrome 45, there's a new error message about a
[weak ephemeral Diffie-Hellman public key][] that started showing up
in our webdriver & chromedriver proxy tests. The intent of the block
was to secure users from the [LogJam vulnerability][].

    ERR_SSL_WEAK_SERVER_EPHEMERAL_DH_KEY

In our testing set up at $WORK, we use [Browsermob Proxy][] to MitM
our E2E test traffic so that we can analyze the network traffic. Using
a proxy allows us to test things like Omniture & Google analytics, and
also enables us to simulate XSS attacks against our website.

Our E2E test suite depends heavily on the proxy being allowed to MitM
the traffic, and Chrome started noticing that the DH key that our
proxy presented was insecure. This is pretty valid for Chrome to want
to block, since we are after all attacking ourselves[^1]. Luckily,
Chrome allows us to blacklist certain ciphers as an argument during
startup and after some wild googling, I arrived upon the following CLI
argument for Chrome:

    --cipher-suite-blacklist=0x0039,0x0033

[[MORE]]

Basically, we are blacklisting the cipher suites that we don't want to
run, and this allows Browsermob::Proxy to go on its merry
man-in-the-middle-attack way. Using the [perl webdriver bindings][] to
start up a proxy and then chromedriver, this ends up looking like:

    use strict;
    use warnings;
    use feature qw/say/;
    use Selenium::Remote::Driver;
    use Browsermob::Proxy;

    my $proxy = Browsermob::Proxy->new(
        server_addr => $server,
        server_port => 8080
    );

    my $d = Selenium::Remote::Driver->new(
        desired_capabilities => {
            browserName => 'chrome',
            proxy => $proxy->selenium_proxy,
            chromeOptions => {
                args  => [
                    'cipher-suite-blacklist=0x0039,0x0033'
                ],
            }
        }
    );

    $d->get('https://www.google.com');
    say $d->get_body;

Probably the most interesting part[^2] is the particular data
structure that `chromeOptions` requires. The same idea will work with
any of the other webdriver language bindings as well - if you can
figure out how to pass arguments to your chromedriver via your
bindings of choice, just append the `cipher-suite-blacklist` argment
to the list of args.

Also of note - a lot of sources recommended blacklisting a longer list
of ciphers:

    --cipher-suite-blacklist=0x0088,0x0087,0x0039,0x0038,0x0044,0x0045,0x0066,0x0032,0x0033,0x0016,0x0013

After little trial and error I was able to determine which particular
ciphers that I needed to blacklist, so I only chose `0x0039,0x0033`,
but others may be necessary; YMMV! For further reading, there were a
couple very helpful links I found:

- Frank Ehlis already figured out how to
  [disable SSL ciphers in Dec 2013][].

- There's a chromium issue [listing all the ciphers][].

- This superuser also helpfully had the answer about
  [disabling the SSL suites][]

- A [diagnostic page][] for determining which SSL cipher suites your
  browser supports!

[^1]: Coincidentally, the security block also to inadvertently caused
issues for people with intranet websites that aren't properly secure,
amongst other things. This has somewhat understadably ended up
[angering plenty of internet users](https://productforums.google.com/forum/#!topic/chrome/o3vZD-Mg2Ic)
who feel very entitled to their free browsers.

[^2]: The other _gotcha_ that I ran into while troubleshooting this
issue is that if the Browsermob proxy server was on the same machine
as the browser in question, the issue didn't manifest itself at
all. We only experienced the issue when the proxy server and the
browser were operating on different machines.

[weak ephemeral Diffie-Hellman public key]: https://www.chromium.org/administrators/err_ssl_weak_server_ephemeral_dh_key
[LogJam vulnerability]: https://weakdh.org/
[Browsermob Proxy]: https://metacpan.org/pod/Browsermob::Proxy
[perl webdriver bindings]: https://metacpan.org/pod/Selenium::Remote::Driver
[disable SSL ciphers in Dec 2013]: http://fehlis.blogspot.com/2013/12/how-to-disable-ssl-ciphers-in-google.html
[listing all the ciphers]: https://code.google.com/p/chromium/issues/detail?id=58833
[disabling the SSL suites]: http://superuser.com/a/966879/493387
[diagnostic page]: https://cc.dcsec.uni-hannover.de/
