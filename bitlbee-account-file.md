This is primarily for me to find next time I run into this issue!
Having previously installed bitlbee on my OS X machine via
[homebrew & some elbow grease], I made a one-off password for the
local bitlbee server I run to store my account credentials. Since I
run the bitlbee server through Emacs and like a well behaved Emacs
user, I hardly ever restart Emacs, I'm sure to have forgotten my
bitlbee credentials between server restarts. At those times it's
pretty useful to be able to find the bitlbee XML file that houses my
username and encoded password. This is also useful in case I need to
change other account setting things.

So, for bitlbee installed on OS X via homebrew, the account xml file
is in the following folder on my machine:

    /usr/local/var/bitlbee/lib/<username>.xml

and the bitlbee server wants the credentials like

    > identify <password>

[homebrew & some elbow grease]: http://blog.danielgempesaw.com/post/104839206054/using-sipe-through-bitlbee-in-emacs-on-os-x
