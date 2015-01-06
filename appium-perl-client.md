Getting Started with the Appium Perl Bindings

I've started work on a set of Perl bindings for the Appium webdriver
client. We're using Appium to test out a few different ideas for
mobile apps, and since our existing browser automation framework is
written in Perl, it makes sense to use Perl for our mobile automation
as well. I recently received an email from probably the first person
other than myself interested in such a pairing (Perl & Appium) for
some help getting started, so I'm putting together a collection of
steps to get going with the Appium Perl bindings.

1. Install Appium and the Perl bindings

You can install Appium via npm:

    $ npm install -g appium

It comes with a helpful binary `appium-doctor` that will give you the
status of your setup. For specific information on installing Appium,
I'll defer to the Appium project's [installation docs][docs]. I made a
[previous post that goes a bit more in depth][mydocs] into the steps
on this blog a few months ago that may be helpful as well.

[docs]: http://appium.io/slate/en/master/#setting-up-appium
[mydocs]: http://blog.danielgempesaw.com/post/83809119400/getting-started-with-ionic-and-appium-for-a-new

For running Android tests specifically, you'll need to download the
Android SDK Tools, and then you'll need to have `$ANDROID_HOME`,
`$JAVA_HOME`, and you'll need to append the sdk to your `$PATH`. If
you're on OS X, Homebrew is super useful for the last step (`brew
install android-sdk`); the first two you may need to do on your
own. See stackoverflow for help on persisting and modifying
environment variables on your platform.

Anyway, follow the [instructions][adt] on the Android Developer
website for downloading at least the SDK Tools. After the download,
you'll need to open the SDK Manager and get the appropriate tools that
correspond to the version of Android you're interested in; the Android
developer website has a [short guide on how to do so][adt-guide].

[adt]: http://developer.android.com/sdk/installing/index.html?pkg=tools
[adt-guide]: http://developer.android.com/sdk/installing/adding-packages.html#GetTools

To install the Perl bindings, you can use `CPAN` or `cpanm`:

    $ cpanm Appium
