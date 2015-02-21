I like using Emacs as often as possible. Our work server was
previously jabber, so I was using the excellent `jabber.el` to chat
with my coworkers. We're switching over to Microsft Lync, so it
appears my best option is to use Bitlbee and an Emacs IRC client (like
[Circe][]!). Bitlbee is a server that you can run on your computer
that acts as a gateway to different chat protocols. It handily exposes
your IM interactions to you through an IRC server. One of the
protocols it supports is SIPE, which is what Microsoft Lync uses. It
was slightly involved to get all the pieces working together; here are
the steps I followed.

[Circe]: https://github.com/jorgenschaefer/circe
[libpurple]: https://developer.pidgin.im/wiki/WhatIsLibpurple

[[MORE]]

### 1. get pidgin and bitlbee

    $ brew update
    $ brew install bitlbee --with-pidgin

`--with-pidgin` specifies the `pidgin` package as a dependency, which
I believe we'll need to manually compile `pidgin-sipe`, since it's not
available on homebrew.

### 2. get pidgin-sipe

`pidgin-sipe` will teach bitlbee how to speak SIPE! Download the
latest tarball from the sipe project [on SourceForge][sipe]. At time
of writing, the direct download link to 1.18.4 is [here][]. Compiling
it requires `gettext`, so get that from homebrew real quick:

[sipe]: http://sourceforge.net/projects/sipe/files/sipe/
[here]:
http://sourceforge.net/projects/sipe/files/sipe/pidgin-sipe-1.18.4/pidgin-sipe-1.18.4.tar.gz/download

    $ brew install gettext
    $ brew link --force gettext

Unzip the tarball, get in that folder, and get to work [compiling][c]
it:

[c]: http://sourceforge.net/p/sipe/discussion/688534/thread/9b11e2b4

    $ ./configure -disable-quality-check -prefix=/usr/local/
    $ make
    $ sudo make install

The `-prefix` option just puts stuff where Homebrew does, which may or
may not be a good idea; I've no idea.

### 3. tell bitlbee what's up!

Once you've got `pidgin`, `pidgin-sipe`, and `bitlbee` all installed,
you should be able to get started playing around with bitlbee
(finally!). You'll want to run it in ForkDaemon mode via `-F`, as it
suggests as a consequence of compiling `--with-pidgin`.

    $ bitlbee -F

Fire up your IRC client of choice and connect to `localhost:6667` and
join the control channel "&bitlbee"[^1]. Say `help purple` in the
&bitlbee channel to see your potential protocols, and hopefully `sipe
(Office Communicator)` will show up in the list! If it's missing,
you'll need to go back and verify your dependencies. Forging bravely
on, add an account with the sipe protocol - although the
[Bitlbee wiki][w] says to include DOMAIN\LOGIN for the `account add
sipe` command, I left it blank and had no troubles logging in. YMMV.

[w]: http://wiki.bitlbee.org/HowtoSIPE

    account add sipe EMAIL@ADDRESS.COM PASSWORD
    account sipe set authentication tls-dsk
    account sipe set useragent "UCCAPI/15.0.4481.1000 OC/15.0.4481.1000 (Microsoft Lync)"
    account 0 on

There were a final few things to tweak: we needed to set the
authentication scheme (TLS-DSK) and masquerade as an actual lync
client by setting our user agent[^2], but after that, we get the golden
output:

    <root> sipe - Logging in: Connecting
    <root> sipe - Logging in: Logged in

Use `blist all` to see your verbose buddy list, and chat away! Ta-da!

[^1]: <small>Honestly, I'm not very good at IRC so it might bring up that
channel automatically, or something - I'm autojoined to it in Circe,
at least. You can optionally register an admin account to persist
your settings via bitlbee's `register`, and subsequently `identify`,
commands.</small>

[^2]: <small>And perhaps we need to set an environment variable? I
didn't need to, but obviously some people did:
http://blog.mattwoodward.com/2012/08/pidgin-sipe-and-read-error-on-ubutnu.html</small>
