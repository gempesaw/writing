[Ionic][] enables you to author mobile apps using the powerful
[AngularJS][] javascript framework. [Appium][] is a cross-platform
mobile testing tool that lets you run your app on emulators _and_ real
devices. Both of these open source projects have full support for iOS
and Android, which is just great. This morning, I worked through
taking a generated Ionic app all the way to my device for testing with
Appium.

[Ionic]: http://ionicframework.com
[AngularJS]: https://www.angularjs.org
[Appium]: http://appium.io

[[MORE]]
### 0. prereqs

You'll need `ionic` (which depends heavily on `cordova`), and
`appium`. Ionic needs `ant` to build the android app; if you're using
Homebrew on OS X, they've gotcha covered.

    $ npm install -g cordova ionic appium
    $ brew install ant

You'll also need the [Android SDK][sdk], which has the Android
Developer Tools in it, I think. I don't know of a better way to
install them other than downloading the zip and unzipping it to a
known folder. After unzipping the sdk somewhere, you'll want to add
`sdk/tools` and `sdk/platform-tools` to your path, and you'll also
want to export `ANDROID_HOME`. Here's the relevant section of my
`~/.bash_profile`; you'll need to edit `ANDROID_HOME` accordingly.

    export ANDROID_HOME=/opt/adt-bundle-mac-x86_64-20140321/sdk
    ANDROID_TOOLS=$ANDROID_HOME/tools/:$ANDROID_HOME/platform-tools
    export PATH=$ANDROID_TOOLS:$PATH

Appium will also want you to export `$JAVA_HOME`; stackoverflow says
you can [do it like this][so] on OS X Mavericks:

    export JAVA_HOME=$(/usr/libexec/java_home)

[so]: http://stackoverflow.com/a/1348940/1156644
[sdk]: http://developer.android.com/sdk/index.html?hl=sk

### 1. the ionic half

Seeing as I'm not _too_ concerned with the app itself right now, just
working on getting all the pieces in place, we'll just use
`generator-ionic` as our template.

    $ npm install -g generator-ionic
    $ yo ionic

That'll lead you through a few set up steps and then generate a
boilerplate app that should be enough to make a baby app. Next thing
to do is to specify that you want android as a platform target:

    $ ionic platform add android

At this point, you'll need to have `ant` and `android` installed and
in your `$PATH`, or else the command will fail; see the prereqs part
above if you're missing anything. Pay a little bit of attention to the
output of this command - in particular, you'll want to hold on to the
`Package` and `Activity` lines for firing up Appium via desired
capabilities. I called my toy app `Breakside`, so the relevant lines
for me said:

    Creating Cordova project for the Android platform:
        Path: platforms/android
        Package: com.example.Breakside
    [...snip...]
    No project name specified, using Activity name 'Breakside'.

After that, you just have to build the `.apk` for appium to use:

    $ ionic build android

That should create a couple `.apk`s in `platforms/android/ant-build`,
including one called suffixed `-debug.apk`. That's what I used for
with Appium in the next step.

### 2. the appium half

I ran Appium with my Galaxy Nexus hooked up to my laptop via USB, with
the [USB debugging mode on][usb]; you'll need to do the same if you
want to run on a device. [^1]

[usb]: http://developer.android.com/tools/device.html

Appium comes with a useful binary called `appium-doctor` which should
be in your path if you've installed it with npm. You can run it to
double check whether your system is ready for testing Android devices.

    $ appium-doctor
    Running iOS Checks
    [...snip...]

    Running Android Checks
    ✔ ANDROID_HOME is set to "/opt/adt-bundle-mac-x86_64-20140321/sdk"
    ✔ JAVA_HOME is set to "/System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home."
    ✔ ADB exists at /opt/adt-bundle-mac-x86_64-20140321/sdk/platform-tools/adb
    ✔ Android exists at /opt/adt-bundle-mac-x86_64-20140321/sdk/tools/android
    ✔ Emulator exists at /opt/adt-bundle-mac-x86_64-20140321/sdk/tools/emulator
    ✔ Android Checks were successful.

    ✔ All Checks were successful

If you already exported `$ANDROID_HOME` and `$JAVA_HOME`, everything
should be good to go! Run the `appium` command after making sure that
the shell knows about `$ANDROID_HOME` and `$JAVA_HOME`:

    $ appium &

The next thing is figuring out how to tell Appium what you want to
do. I'm using the [Perl Webdriver bindings][srd], but luckily desired
capabilities are the same in any language. Here's my short perl script
script:

[srd]: https://github.com/gempesaw/Selenium-Remote-Driver

    #! /usr/bin/perl

    use strict;
    use warnings;
    use Selenium::Remote::Driver 0.1951;

    my $caps = {
        'app' => '/opt/breakside/platforms/android/ant-build/Breakside-debug.apk',
        'app-package' => 'com.example.Breakside',
        'app-activity' => 'Breakside',
        'browserName' => '',
        'platformName' => 'android',
    };

    my $android = Selenium::Remote::Driver->new_from_caps(
        port => 4723,
        desired_capabilities => $caps
    );

    use DDP;
    p $android;
    sleep(60); # optional, if you want to see it open on your device

The `$caps` object needs to know where your `apk` is, as well
as the `app-package` and `app-activity` that we saved from earlier's
`ionic platform add android`. The `browserName` key is a holdover from
some old Appium dependecies, and `platformName` is straightforward.
There's more information about the appium caps in
[their documentation][docs].

Remember to point your bindings at the correct port - by default,
`appium` starts up on `4723`. Pass in your capabilities object, and
you should be good to go - running that script with my Galaxy Nexus
plugged in on USB debugging mode opened the app with the default
Cordova splash screen. Without that last sleep, the app pops open and
closed too quickly to notice, but of course you can remove it when
writing real test scripts.

![ionic and appium](https://24.media.tumblr.com/86e48dd9a4d562ada08f0e1f6a93d7e6/tumblr_n4jfqePOip1qahaiko1_1280.png
 =250x)

[docs]: https://github.com/appium/appium/blob/master/docs/en/caps.md
[^1]: To emulate the Android app, there's some useful information
about how to get it working if you run `ionic emulate android` in the
toy project directory.
