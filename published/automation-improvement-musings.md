re: http://blog.fogcreek.com/the-abuse-and-misuse-of-test-automation-interview-with-alan-page/

I've been thinking recently that one of the big problems we're having
with automation at work is caused by previous-me of years past
encouraging the idea of "oh doing automation is easy, look, gherkin is
close to readable English, that makes it simple!" However, we're a
couple years in to our (perl!) Gherkin & Selenium based automation
framework and we're running into a lot of pain points.

What I've realized, perhaps unfortunately a few years too late, is
that E2E automation is very complicated to do well & usefully. It
takes a deep knowledge of multiple working & changing systems (app
under test, the automation framework, and everything in between) to do
it effectively. Bringing in people who aren't familiar with either
(through no fault of their own) and neglecting to train them properly
is a good way to get a brittle automation suite.

I think that's one of the big problems I should try to solve this
year. In the linked podcast episode, Alan Page espouses some pretty
similar ideas about how to do automated testing well, pointing out
that having junior developers write (e2e) tests often leads to
failure, where as more senior developers who are comfortable with the
complexity of the entire system are actually able to successfully
write useful tests.

> Another reason, and I’m sorry to say this, is often we have is this,
> really scary when you think about it, to me at least… We often have
> our most junior developers, our brand new testers who are learning
> to code or maybe some company that’s where you start people. You get
> these very junior people without a lot of design knowledge for
> software and a lot of experience. You tell them write this GUI
> automation. You have junior people doing something that’s much, much
> harder than most people admit and it just makes it more prone for
> failure.

> I think when you get success is you have developers who know what
> they’re doing, more senior developers. It doesn’t have to be your
> top developer but you have experienced people who know what they’re
> doing. They understand design and they’re very careful about writing
> tests that they can trust to show product status. That’s the case
> that GUI automation can actually work.

He's also pretty strict about flaky tests, which I unfortunately don't
have the luxury to get too hung about. Since I'm the main (aka only)
developer on our automation framework, supporting a growing group of
QA engineers, I don't have time to dig into every flaky issue. Hm,
but, putting it on paper like that, it actually becomes pretty
apparent to me that I do need to spend some time digging into flaky
issues - otherwise I'll get drowned in flaky bugs that negatively
impact our view of the framework. That's already happening, and we do
need to actively combat it.

Another question that we're always curious about is what to
automate. Although they're not wrong with their "automate all the
tests that should be automated," and "automate when you're bored,"
heuristics, I find it a bit lacking to translate that into specific
situations that I can tell our team about.

( also I like this trend of transcribing podcasts! I can read much
faster than I can listen :) )
