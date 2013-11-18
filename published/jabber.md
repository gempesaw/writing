I recently added [`jabber.el`](http://emacs-jabber.sourceforge.net/) to my workflow via `package-install jabber`. `Jabber.el` seems to be in a really good place in terms of usage and polished development. The only hiccup I had was that it was complaining about starttls issues. That was easy enough to google, and it seemed like I should specify the connection type as starttls. We're apparently using an expired cert or something, so I had to make starttls insecure as well. I got the impression I should be more worried about that, but oh well.

<pre><code class="lisp">(setq jabber-account-list '((work-email-address
                             (:connection-type . starttls))))
(setq starttls-extra-arguments '("--insecure"))
(setq starttls-use-gnutls t)
</code></pre>

So that got me connected. As always, there were some customizations that were necessary. First, I don't care about seeing presence, and I don't care about who is offline:

<pre><code class="lisp">(setq jabber-alert-presence-hooks nil
      jabber-show-offline-contacts nil)
</code></pre>

Next, I don't want to hear about the avatars, I don't even want to download them, and I don't want them showing up in the roster:

<pre><code class="lisp">(setq jabber-avatar-verbose nil
      jabber-vcard-avatars-retrieve nil
      jabber-roster-line-format " %c %-25n %u %-8s (%r)")
</code></pre>

I do want the history, as well as alerts in my mode line, and we can shorten the buffer names for funsies.

<pre><code class="lisp">(setq jabber-history-enabled t
      jabber-mode-line-mode t
      jabber-chat-buffer-format "*-jabber-%n-*"
      jabber-roster-buffer "*-jabber-*")
</code></pre>

Finally, I want to autojoin some chatrooms every time I connect:

<pre><code class="lisp">(setq jabber-muc-autojoin '("qa@conference.sharecare.com"))</code></pre>

All together, I'm quite happy with my jabber setup. Pretty low amount of customization necessary for being able to do away with Adium entirely.

Oh, and keybindings - I only know and use a few of them

* `C-x C-j C-c` - connect! start out with this one obviously
* `C-x C-j C-l` - if you have a pending message, switch to that buffer! if you've already switched to a pending message buffer, switch back to the one you came from if there are no more new messages! if there's no pending messages, do nothing! I really like this one.

My only complaint is that I wish I started using `jabber.el` earlier, as it's quite nice to stay in Emacs for chats; quite nice, indeed.
