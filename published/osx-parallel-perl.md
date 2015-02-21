# Running perl's Test::More tests in parallel on OS X

Integration tests and e2e tests are very useful when developing apps,
but unlike unit tests it's harder to keep them fast and snappy. In my
case, I have to spin up multiple webdriver instances to test my
webdriver automation framework as part of the e2e process. Although
webdriver is now significantly faster than it used to be, running
twenty tests in parallel is an order of magnitude quicker than running
them all in series. I've started using [Test::ParallelSubtest][] to
achieve this speedup, due to trouble installing Test::Parallel on OS
X.

[[MORE]]

Previously, [Test::Parallel][] was my perl package of choice for
dropping into a normal [Test::More][] file and getting an immediate
speed up. But at some point a few OS X versions ago, I tried
reinstalling Test::Parallel to no
avail. [Sys::Statistics::Linux::MemStats][] is failing its
configuration because OS X/darwin isn't the intended platform.

    bash-3.2$ cpanm -nf Test::Parallel
    --> Working on Test::Parallel
    Fetching http://www.cpan.org/authors/id/A/AT/ATOOMIC/Test-Parallel-0.20.tar.gz ... OK
    Configuring Test-Parallel-0.20 ... OK
    ==> Found dependencies: Sys::Statistics::Linux::MemStats
    --> Working on Sys::Statistics::Linux::MemStats
    Fetching http://www.cpan.org/authors/id/B/BL/BLOONIX/Sys-Statistics-Linux-0.66.tar.gz ... OK
    Configuring Sys-Statistics-Linux-0.66 ... N/A
    ! Configure failed for Sys-Statistics-Linux-0.66. See /Users/dgempesaw/.cpanm/work/1424475457.49669/build.log for details.
    ! Installing the dependencies failed: Module 'Sys::Statistics::Linux::MemStats' is not installed
    ! Bailing out the installation for Test-Parallel-0.20.

and in the build log

    Configuring Sys-Statistics-Linux-0.66
    Running Build.PL
    OS unsupported! Sorry, but this system seems not to be a linux system! at Build.PL line 5.
    Running Makefile.PL
    OS unsupported! Sorry, but this system seems not to be a linux system! at Makefile.PL line 3.

But, like previously mentioned, Test::ParallelSubtest installs just
fine. It does take a little change to the test code itself, as it
requires you to organize the tests in subtests.

One small issue I ran into was keeping my tiny webserver alive for the
entire duration of the test. I have to start up the webserver, wait
for it to be ready to accept connections, and then I can kick off the
tests that hit the server. Since the server I was using automatically
cleans itself up during destruction, I got bit when the main process
was finishing rather quickly - all it needed to do was put the tests
together, and then quit. Meanwhile, the background subtests had barely
gotten started.

Luckily, `bg_subtest_wait()` solves this problem for me. The docs do
say that it gets called implicitly along with `done_testing`, but it
wasn't working for me. Manually invoking it at the end of the script
kept the parent webserver process alive while all of its kids played
in its server sandbox.

[Test::ParallelSubtest]: https://metacpan.org/pod/Test::ParallelSubtest
[Test::Parallel]: https://metacpan.org/pod/Test::Parallel
[Test::More]: https://metacpan.org/pod/Test::More
[Sys::Statistics::Linux::MemStats]: https://metacpan.org/pod/Sys::Statistics::Linux::MemStats
