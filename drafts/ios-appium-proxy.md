# Proxying Safari traffic on a real iOS device with Appium

At $WORK, we need to analyze our network traffic during our mobile web
tests. In particular, we want to double check our analytics calls,
since they're of utmost import. For a desktop browser, this is baked
right in to Webdriver: using the `proxy` desiredCapability during
browser instantiation takes care of all the hard work on the
browser-setup side. Besides that desiredCapability, we need to stand
up a proxy that captures HARs, configure the browser, use webdriver to
generate the traffic, and then analyze the HARs appropriately.

For a mobile test through Appium, things are a bit more complicated,
and doubly so for a real iOS device. Unfortunately, using a proxy is
not possible while using Appium with a native app - this method is
only for inspecting Safari traffic[^1].

So, you'll need a couple things to get this set up:

- a real iOS device with which to test
- an Apple Developer license, or whatever it's called - it costs
  $99/year, unfortunately
- Appium
- A proxy capable of on the fly SSL MITM. I use [Browsermob Proxy]
  with its [perl bindings], but you can of course choose your
  own. [mitmproxy] is a popular Python proxy that would also work.

[[MORE]]

## Preparing the real iOS device

#### Bribe apple to let you install your software on your hardware

There are pretty good instructions on how to do this in the
[Appium hybrid app testing docs]. Basically, you create a certificate
signing request, upload it to the dev center, and then create and
download a wildcard provisioning profile. Opening that profile in
XCode should install it, and then you tell Appium your credentials so
it will let you install applications on the actual device.

The idea here is to indicate to Apple that we are an iOS developer, so
that they let us install native applications on our real device,
ostensibly for testing our native app. The provisioning profile is
actually used to install SafariLauncher on the real device, which is a
tiny app that launches Safari for us[^3].

At some point before trying to run your test, you'll need to connect
the iOS device to the computer via the thunderbolt cable.

#### Turn on Web Inspector for your iOS's Safari

This one is pretty straightforward - go into Settings -> Safari ->
Advanced -> Web Inspector and make sure it's turned on[^4].

#### Trick your device into trusting your proxy

As expected, when you try to MITM your own SSL traffic, your iOS
device will sense the funny business and refuse to let you do so since
the SSL connection gets terminated at the proxy before your device,
and when the network traffic hits your iOS device, it's been resigned
with the proxy's cert (or something, I don't understand this business
very much at all, obviously).

You need to install and trust the cert offered by your proxy. For
Browsermob, this means you should go to the
[browsermob cert on github], and then click the `Raw` button
there. This will open up the .cer file in Safari, and Safari figures
out that it should install it as a profile. You'll need to click
Verify or Trust a few times during this process, and afterwards you
can check what invalid certs your device trusts in Settings -> TODO
TODO TODO.

## Starting servers

#### Proxy Server

As mentioned, in my case I use Browsermob Proxy to capture the network
traffic into a HAR. You are free to use any proxy setup you want,
noting that being able to programatically create and delete proxies is
very useful. So, I'd need to start the Browsermob server - after
downloading the Browsermob binaries, that should simply be

    # defaults to running a server on 8080
    $ bin/browsermob-proxy

At this point, you can set up the proxy settings on the real iOS
device. Open up Settings -> General -> TODO TODO TODO -> Wireless, and
then tap the connection you're using for the internet. This should
open up the advanced settings for that connection, and at the bottom
of that view you can set a Manual proxy.

The HOST will be the address of your proxy server, in my case wherever
I'm running Browsermob Proxy. You can use ifconfig/ipconfig to get the
IP of the machine on the network. Note that you shouldn't use
localhost/127.0.0.1 for this, since the iOS device will be searching
the network for the address, so it needs to be an IP that the iOS
device can see.

The PORT needs to be selected at this point and manually put into the
iOS device. Later, when running a test, we'll start our proxy on this
port before starting the Appium test. So, put the port in the iOS
device, and then press back in the top left to save the settings[^2].

Putting in the proxy settings should end up breaking the internet on
your iOS device, since we haven't created the proxy yet.

#### ios_webkit_debug_proxy

You'll also need to download and run another process: the
`ios_webkit_debug_proxy`. This is _not_ the same as the above proxy
you'll use for analyzing traffic - it's for communicating with the
Safari webviews through Appium. When starting IWDP, you'll need to
specify the UDID of your device.

The ios webkit debug proxy must also be configured to run on port
27753, Appium expects that specific port and iwdp doesn't default to
it, so it must be set during iwdp startup.

For me, this ends up looking like

    TODO TODO TODO TODO

#### Appium

As per the [Appium hybrid app testing docs], one way to start Appium
is to either have cloned the Github repo, or to go into your
node_modules folder and work from in there. During Appium startup,
you should also pass the UDID of the device, which looks like:

    TODO TODO TODO TODO

## Running a Test

At this point, this ridiculously fragile set up should be ready for
you to run some code. You have a proxy server running for capturing
HARs, IWDP running to comms to/from the webviews, and Appium running
so you can use the handy JSONWireProtocol to drive the
website. Meanwhile, your iOS device is connected via USB, its network
connection is configured to use the manual proxy of your choosing, AND
you convinced Apple with bribes of money and PII to let you install
your own apps on your own hardware. Excellent!

Your code only has to do a few things to get you going:

- For BMP, start a proxy on the proper port. If you're not using BMP,
  this may be look different[^5]
- (optional) Check your appium server and murder any existing Appium
  sessions[^6]
- Pass the proper desiredCaps to tell Appium you want to run Safari
- After the test is done, take down the proxy you created (so we can
  make it new again for the next test)

In perl, this can be done like

    use strict;
    use warnings;
    use Browsermob::Proxy;
    use Appium;

    # must match the manual proxy settings on the iOS device
    my $hardcoded_proxy_port = 9090;

    # assumes BMP server is running on localhost:8080
    my $proxy = Browsermob::Proxy->new(
        port => $hardcoded_proxy_port
    );

    # assumes Appium server is running on localhost:4723
    my $ios_appium = Appium->new(caps => {
        browserName => 'Safari',
        platformName => 'iOS',
        deviceName => 'iPhone 6', # must match your device's name
        platformVersion => '9.2'  # must match your device's iOS version
    });

    $proxy->new_har;
    $ios_appium->get('https://www.google.com');
    my $google_har = $proxy->har;

    use Data::Dumper; use DDP;
    p $google_har;

    $ios_appium->quit;

After all the set up, the code is pretty straightforward: Start the
proxy on the proper port, boot up Appium, start the HAR, use Appium to
make Safari do some network traffic, and then analyze the HAR as you
please.

## Caveats

First off, this has no chance of scaling well. Each iOS device needs a
dedicated OS X to instrument it, and nothing can be run in
parallel. If you run two jobs at once (and you're doing the optional
auto-delete of existing sessions), you get nonsense results. We're
working around this by having a dedicated queue in Resque with only a
single worker, and all iOS jobs for a particular iOS device/OS X box
pair go to the same queue, regardless of the job's source.

Also, as mentioned, the server set up is a little precarious. Three
apps need to be available, and with appropriately matching versions -
across iOS upgrades, I have a big enough headache just getting Appium
back into shape. Some of this can be alleviated with a docker set up
or something like that, where the configuration different server apps
can be locked down, but I haven't gotten that far.

Additionally, it's a bit of a hassle to have to interact with the
physical device to set the port and accept the fake SSL cert. For
server configs, you can do something like schedule Puppet to regularly
reset the configs to the proper state to alleviate silly humans trying
to change things. But, for an actual physical iOS device, I can't
prevent people from fooling with it as they please.

Another point is that the proxy server you use needs to be able to do
on the fly SSL MITM. There are definitely proxies that do this -
LittleProxy (which backs Browsermob Proxy) and MITM Proxy are two open
source options that both have successful implementations (CharlesProxy
also does, but it's closed source and a paid product), so it's not
impossible to find. But, you may find that the proxy setup you're
using for your webdriver proxy tests doesn't yet have SSL MITM.

Finally, if you're not already developing an iOS mobile app (giving
you another reason to pay $99 a year for the Apple Developer License
thing), it's going to cost you $99 a year that you wouldn't have
spent otherwise.

## Conclusion

In summary, this is quite the shaky house of cards. But, after having
gotten everything set up and just trying not to touch anything, it's
already started being pretty useful - I've used it a couple times when
I needed access to a physical device for a quick test. Composing tests
for Appium's Safari is often very similar to writing Webdriver tests,
and we've got plenty of experience doing that.

As a bonus, you can set all of this up on a remote machine combined
with a cool feature of Quicktime Player. Set up an unused OS X box on
your network to accept Screen Sharing requests, connect an iOS device
to it, start all the requisite servers on the box, and then open up
Quicktime and start Recording a video via one of its File
menus. Choose your iOS device from the dropdown list near the record
button, and then you can see the screen of the device from a remote
machine. Leave that quicktime window open, and then you don't need to
be physically near your iOS device to observe the tests - it will be
visible when you use Screen Sharing to connect to the OS X box!

[^1]: For whatever reason (probably a very good reason), native apps
do not respect the proxy settings on the device, and I'm not aware of
any other way to route a native app's HTTP traffic through a proxy

[^2]: If, like me, you don't believe the proxy settings are saved,
since you don't press a "Save" or "Confirm" button, and there's no UI
change to indicate that the proxy settings were saved, you can open
the connection again and voila, the proxy settings are still there :P

[^3]: I think Appium's usual flow is to install an app on the iOS
simulator or device, and then use instruments to interact with
it. However, (I think) we can't instrument Safari because we can't
build the Safari app. Since IWDP lets us talk to Safari's WebViews,
SafariLauncher bridges the gap: it's not built-in, so Appium can
install and instrument it, and then SafariLauncher has a button that
launches Safari, at which point the Safari WebViews are available for
communication through IWDP.

[^4]: Coincidentally, this also lets you use your
OS X Safari's devTools on your iOS device (or simulator)'s instance of
Safari, so you can see the console & network tabs and run javascript
on your device (or simulator).

[^5]: I chose to create & delete the BMP proxy around each Appium
session because this more closely mimics my existing BMP server
behavior for desktop Webdriver proxy tests. The only difference is
that the port is hardcoded for the Appium tests. If you want to leave
the proxy around all the time instead of re-creating/deleting it,
that's fine as well, and it gives you the added advantage that your
iOS device will have a working network connection outside of your
Appium tests, since the proxy is available. With my set up, the iOS
device's network connection is useless without its proxy.

[^6]: I don't think there's any technical limitation on Appium's side
that limits you to a single session at a time. But, I think iOS and OS
X only let you instrument one app at a time. As a result, for iOS
runs, Appium is set up to run only one session at a time. If there's
an existing iOS session on the Appium server, it will get mad and
refuse to start up. This limitation dovetails somewhat well with the
fact that we're hardcoding the proxy port. If we were able to run two
tests at once across the same proxy port, there'd be no way to
separate what traffic came from which test.

[Browsermob Proxy]: TODO TODO TODO TODO
[perl bindings]: TODO TODO TODO TODO
[mitmproxy]: TODO TODO TODO TODO
[Appium hybrid app testing docs]: TODO TODO TODO TODO
[browsermob cert on github]: TODO TODO TODO TODO
