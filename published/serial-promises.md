# Executing promises in serial with [].reduce

Recently at $WORK, we were writing a data migration script in node
that needed to make a couple hundred requests. The first attempt was
just to wrap everything up in a Promise.all:

    const rp = require('request-promise-native');
    const urls = [
        'url1',
        'url2',
        // ...,
        'url300'
    ];

    Promise.all(urls.map((url) => rp.get(url)
                         .then(sendRelatedRequests)));


However, the internal server we were talking to wasn't able to handle
all of the requests concurrently, and since the subsequent logic would
also send a few more requests of its own, we ended up taking down the
server because we were spawning all the promises at the same time,
and since Promises execute once they're made, that means all the
requests were starting off at roughly the same time.

So, for our second pass, we decided we wanted to only send one request
at a time, lining up all of our requests in serial, since we know that
when the server finishes responding to our nth request, it should be
ready to handle the (n+1)th request. One way to accomplish this is
with a big long chain of `.then`s, as by the time we're in the
`.then`, we're guaranteed that its preceding promise is completed. And
one way we can construct that chain is with a reduce:

    urls.reduce(
        (acc, url) => acc.then(() => rp.get(url).then(sendRelatedRequests)),
        Promise.resolve()
    );

`[].reduce` takes two arguments: the reducing function, and the
initial value. We need to start with a Promise, because our reduce
function assumes that the accumulator `acc` has a `.then` on it.

For the reducing function, we have an accumular, and a url. Each time,
`acc` is the existing serial chain of promises, and we add another
`.then` on to it. The important part is that the function in the
`.then` handler is _not_ immediately creating the promise, because
that would mean the request is immediately sent. Instead, passing the
function expression means the Promise isn't created until the `.then`
handler is invoked, and since the `.then` handler is invoked until its
preceding Promise is complete, we get our serial behavior.

Also, since the requests don't care about each other, we don't need to
use the arguments that are coming from the previous promise, so the
function expression doesn't use its arguments.

The one last catch (hoho) is that if any of the `rp.get(url)` promises
fail, then all of the subsequent `.then`s are skipped, as the promise
flow dictates that it should jump to a `.catch` handler, if one
exists. So, to guarantee that we do make all the requests that we
wanted to, we need to add a catch handler to each of the promises in
the chain.


    urls.reduce(
        (acc, url) => acc.then(() => rp.get(url)
                               .then(sendRelatedRequests)
                               .catch(console.error)),
        Promise.resolve()
    );
