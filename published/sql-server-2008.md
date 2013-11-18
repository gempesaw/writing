I don't know anything about SQL Server 2008 R2

One of my work assignments depended on an install of SQL Server 2008 R2. I needed to use the `sa` account to log in and do some database-y things during latter parts of the install, but I didn't know the password for the `sa` account, and I didn't know how to find it or change it. According to Google, [SQL Server Management Studio](http://www.microsoft.com/en-us/download/details.aspx?id=7593) is what I need to do admin things. At this point, I found out that the `sa` account was actually disabled by default in new installations of 08 R2 as a security precaution, so my task was to get admin access, enable the `sa` account, and change its password.

There are apparently two methods of login for SQL Server 2008: Windows Authentication mode, wherein you use the same credentials that you do for logging in to the machine itself, and SQL Server and Windows Authentication mode. The latter allows us to use SQL logins alongside of the Windows credentials. Now, enabling the `sa` account wouldn't have done anything on its own if the server authentication mode was Windows Authentication only, which would have prevented me from logging into the `sa` account. So.

First, in order to do admin things to other accounts, I apparently needed to temporarily enable single user mode for the server, described in a [Microsoft article](http://msdn.microsoft.com/en-us/library/dd207004.aspx) I had been lucky enough to find off [stackoverflow](http://stackoverflow.com/questions/4188193/how-do-you-reset-the-sa-password). After changing the startup command and restarting the server, I was able to use SSMS with no restrictions and full freedom to muck about with all sorts of stuff.

[!http://d.pr/i/KFS2+]
[!http://d.pr/i/Byp7+]
Next, I had to change the server authentication to "Mixed" so that enabling the `sa` account was worth anything at all. Docs for that courtesy of [another Microsoft article](http://msdn.microsoft.com/en-us/library/ms188670.aspx). Finally, I had to open the properties of the "sa" acconut up, "Enable" it, and change its password to something I knew.

Hmm, I've learned that I don't like to use GUIs to manage my databases, and that VM snapshots are quite useful after you've done some crazy things to the db.
