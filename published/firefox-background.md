# Prevent Selenium Webdriver's Firefox from stealing focus on OS X

When I'm composing automated functional browser tests, I usually run
the tests against a local instance of Webdriver on my own computer to
keep the feedback loop tight. Once authoring is done, I'll kick it off
to Saucelabs for cross-browser testing and additional validation.

On OS X, if I instantiate a new Chrome webdriver browser, it politely
stays in the background unless I go looking for it, so I can watch and
debug my test output on the CLI. Unfortunately, when Webdriver starts
a test in Firefox, it steals cursor focus, shows up on top of whatever
windows I already have, and interrupts my workflow.

There's apparently a plist option that solves this very problem:

    <key>LSBackgroundOnly</key>
    <string>True</string>

By adding that to the top of my
`/Applications/Firefox.app/Contents/Info.plist` file in the `<dict>`
tag, Firefox no longer steals focus. (You may or may not have to
`sudo` to edit that file...). Previously:

    <plist version="1.0">
    <dict>
        <key>CFBundleDevelopmentRegion</key>
        ...

and now,

    <plist version="1.0">
    <dict>
        <key>LSBackgroundOnly</key>
        <string>True</string>

        <key>CFBundleDevelopmentRegion</key>
        ...

In fact, with that key in the plist, Firefox doesn't show up in the
Alt-Tab menu, the dock, or in the `Force Quit Applications` window
either! When I try to focus on it, it hides behind all other
windows. I ended up having to kill it via command line, which was nice
since I'm trying to get more comfortable with awk.

    $ ps aux | grep [f]irefox | awk '{print $2 " "}' | xargs kill -9

Seeing as it's a pretty generic plist solution, it probably works for
other apps too. But, it will affect all instances of Firefox, not just
the Webdriver one, so it's possible this may not be the right solution
if Firefox is your daily browser.
