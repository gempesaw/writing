Bitlbee is a server that you can run on your computer that acts as a
gateway to different chat protocols. You tell it about your accounts
and it transparently presents your IM interactions to you through an
IRC server, at which point you can use any IRC client of your choosing
to chat with the clients that it supports. One of the protocols it
supports is SIPE, used by Microsoft Lync (ehhh?). Thanks to
[libpurple][] and [pidgin-sipe][sipe], bitlbee theoretically is able
to connect to SIPE accounts (as is pidgin and adium), but I'm still
working through all the steps.

[libpurple]: https://developer.pidgin.im/wiki/WhatIsLibpurple

```
$ brew update
$ brew install bitlbee --with-pidgin
```

`--with-pidgin` will installs `pidgin` for us quite usefully, which I
believe we'll need to compile manually `pidgin-sipe`, since it's not
available on homebrew. If you use macports, I think there is a
`pidgin-sipe` port available, but unfortunately I do not use
macports. No big deal either way. You need to get some deps ready for
pidgin-sipe, and you'll see a spooky warning for linking `gettext`
which may mean things to people much smarter than me:

```
$ brew install gettext
$ brew link --force gettext
```

Next, download the latest sipe tarball from the sipe project
[on SourceForge][sipe]. At time of writing, the direct download link
to 1.18.4 is [here][].

[sipe]: http://sourceforge.net/projects/sipe/files/sipe/
[here]:
http://sourceforge.net/projects/sipe/files/sipe/pidgin-sipe-1.18.4/pidgin-sipe-1.18.4.tar.gz/download

Unzip the tarball, get in that folder, and get to work compiling it:

```
$ ./configure -disable-quality-check -prefix=/usr/local/
$ make
$ sudo make install
```

The -prefix option just puts stuff where Homebrew does, which may or
may not be a good idea; I've no idea.

Once you've got `pidgin`, `pidgin-sipe`, and `bitlbee` all installed,
you should be able to get started playing around with bitlbee
(finally!). You'll want to run it in ForkDaemon mode via `-F`, as it
suggests as a consequence of compiling `--with-pidgin`.

```
$ bitlbee -F
```

Fire up your IRC client of choice and connect to `localhost:6667` and
join the control channel "&bitlbee". (Honestly, I'm not very good at
IRC so it might bring up that channel automatically). You can
optionally register an admin account to persist your settings via
bitlbee's `register`, and subsequently `identify`, commands.

Say `help purple` in the &bitlbee channel to see your potential
protocols, and hopefully `sipe (Office Communicator)` will show up in
the list! If it's missing, you'll need to go back and verify your
dependencies. Forging bravely on, add an account with the sipe
protocol:

```
account add sipe EMAIL@ADDRESS.COM PASSWORD
account sipe set authentication tls-dsk
account sipe set useragent "UCCAPI/15.0.4481.1000 OC/15.0.4481.1000 (Microsoft Lync)"
account 0 on
```

We needed to set the authentication scheme (TLS-DSK) and masquerade as
an actual lync client by setting our user agent, but after that, we
get the golden output:

```
<root> sipe - Logging in: Connecting
<root> sipe - Logging in: Logged in
```

Use `blist all` to see your verbose buddy list, and chat away! Ta-da!
