[Browsermob Proxy][bmp] is an open source project that
[Patrick Lightbody][pl] split off from the main Selenium project. It
works really well in tandem with Selenium Webdriver, but it was
missing Perl bindings. BMP exposes a RESTish interface for interacting
with the proxies, so I've strung together a Perl module to take care
of it: [Browsermob::Proxy][bp]. Amongst other things, you can use BMP
to throttle net traffic during tests, analyze request/response pairs
for things like Omniture and Google Analytics, and even alter the
requests on the fly with custom headers. Here's a short run down and
some basic get-started scripts for using it!

[bmp]: http://bmp.lightbody.net/
[pl]: https://github.com/lightbody
[spore]: https://metacpan.org/pod/Net::HTTP::Spore

[[MORE]]

The bindings are up on CPAN with pretty limited functionality at the
moment - definitely not the full BMP API. It can create proxies and
HARs, and spit out the HARs back out when requested, but it doesn't do
much else besides that as of v0.04. To get up and running with BMP,
you'll need to download the binary from the [BMP website][bmp] and
then execute the appropriate file in its `bin/` folder to start the
BMP server.

[bp]: https://metacpan.org/pod/Browsermob::Proxy

Here's an example of using [Browsermob::Proxy][bp] with
[Selenium::Remote::Driver][srd] to capture all of the network traffic
during a single request to google:

[srd]: https://metacpan.org/pod/Selenium::Remote::Driver

    #! /usr/bin/perl

    use strict;
    use warnings;
    use DDP;
    use Browsermob::Proxy;
    use Selenium::Remote::Driver;

    my $proxy = Browsermob::Proxy->new(
        server_port => 8080,
    );

    my $driver = Selenium::Remote::Driver->new(
        browser_name => 'chrome',
        proxy        => $proxy->selenium_proxy
    );

    $driver->get('https://www.google.com');

    p $proxy->har;

This is assuming your BMP server is already downloaded and currently
running on port 8080. But, that's all we need to create a new proxy
for the Chrome browser that Selenium::Remote::Driver is about to
make. The `$proxy` comes with a convenience method `selenium_proxy()`
that returns the appropriate desired capabilities to configure the
proxy for use with Chrome. (If you want to use a proxy with Firefox,
you'll need to use a [custom Firefox profile][ff].)

[ff]: https://metacpan.org/pod/Selenium::Remote::Driver::Firefox::Profile

Once we've instantiated our driver, having told it to use the proxy,
all that's left is to create some traffic and spit out the HAR.

If you want to try out the module without having to install
Selenium::Remote::Driver, you can just `curl` some traffic across the
proxy:

    #! /usr/bin/perl

    use strict;
    use warnings;
    use DDP;
    use Browsermob::Proxy;

    my $proxy = Browsermob::Proxy->new(
        server_port => 8080,
    );

    $proxy->new_har;
    my $generate_traffic = 'curl -x http://127.0.0.1:' . $proxy->port
        . ' http://www.google.com > /dev/null 2>&1';

    `$generate_traffic`;
    p $proxy->har;

This creates a proxy in exactly the same fashion and then uses `curl`
to generate the traffic.
