Unknown command 'WaitForAllTabsToStopLoading'

I woke up this morning and went to run some Webdriver jobs in Chrome, but for some reason they weren't working! The browser would start up, and then it would die, complaining about an unknown command called 'WaitForAllTabsToStopLoading'

    Dying: Error while executing command: An unknown server-side error
    occurred while processing the command.: Unknown command
    \'WaitForAllTabsToStopLoading\'.

It seems like there's a problem with older versions of Chromedriver and the newest version of Chrome. While my day to day Chrome hasn't updated since I never close it, the Chrome that Webdriver uses gets restarted every time a test is run, so it updated itself to Version 29.0.1546.57. As a result, my old Chromedriver doesn't seem to know how to drive the new Chrome. Updating the chromedriver binary to the [one released on August 6][1] fixes the problem for me.

For a few minutes, this was a really confusing bug. I'm constantly tweaking our automation framework, so I wasn't too surprised when the tests were failing - I figured I must've broken something on my dev branch of the framework. However, when a couple of my coworkers said they had the exact same issue, despite not changing anything from the day before, I had no idea what might be going wrong. I didn't think about Chrome updating itself on all of our computers at the same time, leading to duplicate bugs in systems that were supposedly unchanged. Fun :)

[1]: https://code.google.com/p/chromedriver/downloads/list
