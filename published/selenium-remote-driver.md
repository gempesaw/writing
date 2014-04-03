I recently became the maintainer of [Selenium::Remote::Driver][srd],
which is the Perl bindings to the Selenium Webdriver API. I just
[released version 0.18 to the CPAN][cpan] and hopefully got the tests
passing again :D.

I'm still trying to figure out the Best Way to test a module that is
basically an interface to a ReST API. As far as I'm aware, integration
testing these kinds of modules basically require you to mock or fake
out network responses, but there were a couple articles on
blogs.perl.org that [discouraged the use of mocks][chromatic], so I'm
a bit unsure about best practices.

[Test::LWP::UserAgent][tua] is one way to mock/fake out network
traffic in Perl, and it's similar to [AngularJS's $httpBackend][ng]:
you instantiate a fake network object, and then tell it what requests
it should expect. You inject the fake network object into your
module's constructor, and when it tries to make a request, the mock
catches it and lets you examine the request for tests and the
like. You can also specify responses to test out the different
branches of the module, like for error handling tests.

For a big module, having to set up tests for every single request can
get a bit cumbersome, so `S::R::D` went the way of simply recording
all of the requests/responses that its test suite makes, and including
the recordings with the module. When running the tests, it loads up
the recording and matches incoming requests to its list of recorded
REQ/RES pairs, and returns the appropriate response.

Unfortunately, the recording process is different across platforms, so
every change to the module or the tests require new recordings for
each supported platform. Additionally complicating the issue was that
there was a bug somewhere in the recording code, so it would only work
with an LWP-Protocol-PSGI-0.04, which is the module used to record the
requests and responses.

[srd]: https://github.com/gempesaw/Selenium-Remote-Driver
[chromatic]: http://modernperlbooks.com/mt/2012/04/mock-objects-despoil-your-tests.html
[cpan]: https://metacpan.org/pod/Selenium::Remote::Driver
[tua]: http://search.cpan.org/~ether/Test-LWP-UserAgent-0.023/lib/Test/LWP/UserAgent.pm
[ng]: http://docs.angularjs.org/api/ngMock/service/$httpBackend
