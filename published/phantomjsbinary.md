

tl; dr - `-Dphantomjs.binary.path=/path/to/your/phantomjs`

[Selenium WebDriver](https://code.google.com/p/selenium/) is an
browser automation framework that uses "drivers" to drive
browsers. So, there is a "chromedriver" that you send instructions
like "click on the Sign In link" or "tell me what color that element
is," and it will carry out the instructions in an actual Chrome
browser. [PhantomJS](http://phantomjs.org/), by
[Ariya Hidayat](https://github.com/ariya), is a quick, lightweight
headless browser that parses JS (unlike HTMLUnit! !). Since it doesn't
have to render anything on the screen, it can perform these tests much
faster, but the tradeoff is accuracy - you're no longer testing in the
_exact_ same environment as your users.

Its driver is cleverly (?) named GhostDriver, written by
[Ivan De Marino](https://github.com/detro). GhostDriver is built in to
PhantomJS, so if you want to use PhantomJS in your WebDriver tests,
the only thing you need to do is
[install PhantomJS](http://phantomjs.org/download.html), which Ariya
makes very easy (for example, `brew install phantomjs`).

I'm using the Perl WebDriver bindings, and I start the selenium server
with a shell command that looks something like

    $ java -jar selenium-server-standalone-2.35.0.jar -Dwebdriver.chrome.driver=C:\chromedriver.exe -Dwebdriver.ie.driver=C:\InternetExplorerDriver.exe

That `-Dwebdriver.chrome.driver` option tells the Selenium server
where to go find my Chromedriver, and likewise for IE. Now, PhantomJS
makes it really easy to connect to an existing standalone server -
from their Github page, once PhantomJS is in your path, you just need
to

    $ phantomjs --webdriver=<PORT_TO_WEBDRIVER> # default is 4444

So, those two commands will start up the Selenium standalone server
and hook up PhantomJS to it, so you can request a PhantomJS browser of
the server and run your tests. But, it's a pain to have to set up two
processes for PhantomJS when I can already add the Chrome and IE
drivers as options. I couldn't find this documented anywhere, but you
can also add the PhantomJS binary as a command line argument to the
standalone server:

    $ java -jar selenium-server-standalone-2.35.0.jar -Dphantomjs.binary.path=/path/to/your/phantomjs

If you use homebrew, it would be

    $ java -jar selenium-server-standalone-2.35.0.jar -Dphantomjs.binary.path=/usr/local/bin/phantomjs

Interestingly, it doesn't seem to show the PhantomJS log information
on OS X when invoking PhantomJS this way, but on Windows, all of the
normal request and response logging does show up in the command window.
