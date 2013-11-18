![auth-source-forget-all-cached](http://d.pr/i/1SjL+)

I recently had to change my email password, as properly dictated by
work's IT policies. I use `offlineimap` to get my mail, `mu4e` to read
it, and `smtpmail-send-it` to send it.


    (setq message-send-mail-function 'message-send-mail-with-sendmail
          message-send-mail-function 'smtpmail-send-it
          smtpmail-stream-type 'starttls
          smtpmail-default-smtp-server "pod51019.outlook.com"
          smtpmail-smtp-server "pod51019.outlook.com"
          smtpmail-smtp-user "dgempesaw@sharecare.com"
          smtpmail-smtp-service 587)


`Offlineimap` manages its password stuff from a config file at
`~/.offlineimaprc`. I updated that file and I was able to continue
retrieving my mail with no hiccoughs. I then updated my `~/.authinfo`
file as well with the new password, but I was continued to get the
authentication failed error.

    Sending failed: 535 5.7.3 Authentication unsuccessful in response to base64-encoded-password

That was pretty odd, considering that I changed the source file and
all. I tried digging through `smtpmail.el` a little bit, looking for
occurences of password or auth or something. I happened to see a defun
called `auth-source-search` which got me into `auth-source.el`. Doing an
`apropos-command` for "auth" led me to
`auth-source-forget-all-cached`.

Apparently the auth sources like
`~/.authinfo` are cached for a customizable length of time, so I just
had to clear the cache and everything is back in running order!
