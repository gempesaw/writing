[Appium][] is an automation framework for controlling apps on mobile
devices. Like [Webdriver][], the system architecture consists of the
Appium server that bridges between your code and the mobile
device. You can use any language to speak to the Appium server through
its RESTful interface. The [ruby bindings][] have a pretty cool tool
called [arc][] - Appium Ruby Console - it lets you interact with
appium REPL-style, interactively via [pry][]. One of the commands in
arc is especially useful\: [`page`][] lists all the elements on the
current view of the mobile app that you are able to interact with. So,
anything you can tap, swipe, or input into, `page` will tell you
about, along with some identifying information, if any is
available. Since we're using the Perl bindings at work, we don't
immediately have access to the `page` command, but the beautiful thing
about Appium's set up is that its just a matter of translating between
languages to port new functionality over!

[[MORE]]

I started with the iOS version of `page`, as the implementation is
actually pretty different between iOS and Android. In order to get
information about the current view of the iOS app, we have to use
UIAutomation. UIAutomation is iOS's way of automating iOS apps - it
looks a lot like javascript, and it lets you interact with the apps in
question. I believe this is what Appium uses to control iOS: it
translates between its REST interface and UIAutomationOS under the
hood. We use a plain UIAutomation command to get information about the
current view from iOS instruments, and then spend some time formatting
the results to show to the user.

First off, we need to determine which "source window" has any elements
of interest. I'm not entirely clear on what these are, but often the
first source window doesn't have any elements, so we end up wanting to
skip it. We recurse through the list of source windows before finally
returning the first one that has any interesting children elements.

Next, we ask the iOS app for data about the page and get to work. The
JSON structure that the Appium server hands back to us has some
information about the element we're looking at, if available: name,
label, value, visibility, etc. If the element is visible, we'll print
all defined details to the user, and double check if any of the
element details match any of the ID strings we got from
[`app_strings`][]. Note that we're using Perl's [`state`][] to load
`$app_strings` so we don't flood the Appium server with repetitive
requests. Using `state` instead of `my` means we just instantiate
`$app_strings` once, the first time we come across that line of code,
instead of every time we run the function.

Finally, we look at the children of our element and run `page` on our
children, and in a depth-first we'll eventually list all of our
visible elements.

It's not a 1-to-1 translation of the ruby lib's page command, as they
take filtering arguments, and also do some work to ensure that
duplicated details about the element are contained on the same line,
and also we have no Android support whatsoever, but it's already
useful to me! This will probably be coming in a future release of
Perl's [Appium bindings][].

[Appium]: https://github.com/appium/appium/
[Webdriver]: https://github.com/SeleniumHQ/selenium
[ruby bindings]: https://github.com/appium/ruby_lib
[arc]: https://github.com/appium/ruby_console
[pry]: https://github.com/pry/pry
[`page`]: https://github.com/appium/ruby_lib/blob/ad10640c58ba1435b32a4d3a8268f66ae4c4b74e/lib/appium_lib/ios/helper.rb#L125-L152
[`app_strings`]: https://metacpan.org/pod/Appium#app_strings-language
[`state`]: http://perldoc.perl.org/functions/state.html
[Appium bindings]: http://metacpan.org/pod/Appium
