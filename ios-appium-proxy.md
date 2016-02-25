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
  $99/year, unfortunately.
- Appium,
- A proxy capable of on the fly SSL MITM. I use [Browsermob Proxy]()
  with its [perl bindings](), but you can of course choose your
  own. [mitmproxy]() is a popular Python proxy that would also work.

### Set up your iOS device

There are pretty good instructions on how to do this in the
[Appium docs](). Basically, you create a certificate signing request,
upload it to the dev center, and then create and download a wildcard
provisioning profile. Opening that profile in XCode should install it,
and then you can tell Appium your code signature so it will let you
install applications on the actual device.

The idea here is to indicate to Apple that we are an iOS developer, so
that they let us install native applications on our real device,
ostensibly for testing our native app. The provisioning profile is
actually used to install SafariLauncher on the real device.

### Starting servers

In addition to running the Appium server, you'll also need to download
and run another server: the `ios_webkit_debug_proxy`. This is not the
same as the proxy you'll use for analyzing traffic. Both this process
and the Appium process should be started up with the UDID of your real
iOS device; I put the following in a shell script to be able to start
both servers at once.

The ios webkit debug proxy also must run on port 27753.

### Preparing the iOS device

[^1]: For whatever reason (probably a very good reason), native apps
do not respect the proxy settings on the device, and I'm not aware of
any other way to route a native app's HTTP traffic through a proxy
