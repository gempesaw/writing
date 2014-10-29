We're gearing up for the release of v0.22 of Selenium-Remote-Driver, a set of Perl bindings for the Webdriver project. My primary motivation to get a new release out is to tweak some of the classes to be more easily extended, so that the perl bindings for [Appium](https://github.com/appium/perl-client) can be written more cleanly. But, there's also some cleanup for deprecated functions and a few bugfixes included, along with plans for another release in the near future.

* Before we get in to the changes, here's a heads up about the new mailing list for the perl bindings: https://groups.google.com/forum/#!forum/selenium-remote-driver :D

[[MORE]]

* A new subroutine `get_user_agent` will be making it into the API for Driver.pm as a convenience method to, well, get the user agent!

* The main [changes](https://github.com/gempesaw/Selenium-Remote-Driver/commit/63e78e3) for Appium were allowing for more dependency injection of classes like `::RemoteConnection` and `::ErrorHandler`. This lets us tweak things in the Appium module to address discrepancies between Appium and Webdriver. This is also the case for the `FINDERS` constant defined in the `::Driver` class - previously, the constant was privately scoped to Driver.pm, preventing modification from Appium.pm. But Appium has a different list of acceptable finders. So, [ea68e0][] makes it so the `Appium` class can declare its own `FINDERS` constant via the `use constant` pragma.

[ea68e0]: https://github.com/gempesaw/Selenium-Remote-Driver/commit/ea68e064fcf442cef1e1f9d0f9f1e01953c30dee

* Thanks to a few updates to [Dist::Zilla::Plugin::TravisYML](https://metacpan.org/pod/Dist::Zilla::Plugin::TravisYML), our Travis tests are passing again on the `master` branch instead of just on `cpan` - there used to be test failures just getting Dist::Zilla to install, even though our own tests were passing. The most recent version of ::TravisYML fixed those issues!

* [peroumal1](https://github.com/peroumal1) caught a couple deprecated functions that will now be throwing warnings - `get_speed`, and `set_speed` are already no-ops and now will be warning appropriately. The same goes for the `drag` subroutine in `::WebElement`, which no longer seems to be in the (not actually official) [JSONWireProtocol](https://code.google.com/p/selenium/wiki/JsonWireProtocol).

* A few bugfixes made it in, including more helpful error messaging when failing to initiate a session on Saucelabs, or when chromedriver is incorrectly configured. There was also a casting issue reported that I still can't reproduce for setting window size where the JSON conversion was using strings instead of integers ("1280" x "1024" vs 1280 x 1024). I still can't reproduce the issue, but I can't imagine the fix to cast the variables into integers causing any other problems, so I'll probably include it anyway.

* On the horizon, we've got a rewrite of our recording/mocking functionality on the way, courtesy of [peroumal1](https://github.com/peroumal1). Looking over the code in their [mock-driver-experiment](https://github.com/peroumal1/Selenium-Remote-Driver/tree/mock-driver-experiment) branch, things seem to be shaping up quite nicely. This is a welcome change because we've been dealing with a complicated process and an explicit dependency on LWP::Protocol::PSGI v0.04 to generate our recordings, effectively preventing anyone else from contributing recordings.

* Speaking of release versions, this is the numbering scheme I'm trying to use. The impression I've gotten is that CPAN version numbering is a bit of a grab bag, so perhaps explanation is in order.

 - A point increase for deliberate releases with new/important functionality - ie, 0.21 to 0.22.
 - A minor version increase for bugfixes - 0.21 to 0.2101 to 0.2102, etc.
 - Dev releases to prepare for the next point release go out on the 0.XX50 namespace and increment - so, 0.2150, 0.2151, etc.
