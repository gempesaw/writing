# Connecting to Mongo with a self signed CA on a JVM in Kubernetes

At $WORK, we're creating an internal platform on top of Kubernetes for
developers to deploy their apps. Our Ops people have graciously
provided us with Mongo clusters that all use certificates signed by a
self-signed certificate authority. So, all our clients need to know
about the self-signed CA in order to connect to Mongo. For Node or
Python, it's possible to pass the self-signed CA file in the code
running in the application.

But, things are a little more complicated for Java or Scala apps,
because configuration of certificate authorities is done at the JVM
level, not at the code level. And for an extra level of fun, we want
to do it in Kubernetes, transparently to our developers, so they don't
have to worry about it on their own.

### err, wha? telling the JVM about our CA

First off, we had to figure out how to tell the JVM to use our CA. And
luckily since all the JVM languages use the same JVM, it's the same
steps for Scala, or Clojure, or whatever other JVM language you
prefer. The [native MongoDB Java driver docs][docs] tell us exactly
what we need to do: use `keytool` to import the cert into a keystore
that the JVM wants, and then use system properties to tell the JVM to
use that keystore. The `keytool` command in the docs is:

```
$ keytool -importcert -trustcacerts -file <path to certificate authority file> -keystore <path to trust store> -storepass <password>
```

The path to the existing keystore that the JVM uses by default is
`$JAVA_HOME/jre/lib/security/cacerts`, and its default password is
`changeit`. So if you wanted to add your self signed CA to the
existing keystore, it'd be something like

```
$ keytool -importcert -trustcacerts -file ssca.cer -keystore $JAVA_HOME/jre/lib/security/cacerts -storepass changeit
```

(Even this very first step had complications. Our self signed CA was a
Version 1 cert with v3 extensions, and while no other language cared,
`keytool` refused to create a keystore with it. We ended up having to
create a new self-signed CA with the appropriate version. Some lucky
googling led us to that conclusion, but of particular use was using
`openssl` to examine the CA and check its versions and extensions:)

```
$ openssl x509 -in ssca.cer -text -noout
// Certificate:
//     Data:
//         Version: 3 (0x2)
//         Serial Number: ...
//         ...
//         X509v3 extensions:
//             X509v3 Subject Key Identifier: ...
//             X509v3 Key Usage: ...
//             X509v3 Basic Constraints: ...
```

Another useful command was examining the keystore before and after we
imported our self signed CA:

```
$ keytool -list -keystore /path/to/keystore/file
```

as you can look for your self-signed CA in there to see if you ran the
command correctly.

Anyway, once you've created a keystore for the JVM, the next step is
to set the appropriate system properties, again as out lined in the [docs][]:

```
$ java \
  -Djavax.net.ssl.trustStore=/path/to/cacerts \
  -Djavax.net.ssl.trustStorePassword=changeit \
  -jar whatever.jar
```

Since the default password is `changeit`, you may want to change
it... but if you don't change it, you wouldn't have to specify the
trustStorePassword system property.

### handling this in kubernetes

The above steps aren't too complicated on their own. We just need to
make sure we add our CA to the existing ones, and point the JVM
towards our new and improved file. But, since we'll eventually need to
rotate the self-signed CA, we can't just run `keytool` once and copy
it everywhere. So, an `initContainer` it is! `keytool` is a java
utility, and it's handily available on the `openjdk:8u121-alpine`
image, which means we can make a initContainer that runs `keytool` for
us dynamically, as part of our Deployment.

Since seeing the entire manifest at once doesn't necessarily make it
easy to see what's going on, I'm going to show the key bits piece by
piece. All of the following chunks of yaml belong to in the
`spec.template.spec` object of a Deployment or Statefulset.

```
spec:
  template:
    spec:
      volumes:
      - name: truststore
        emptyDir: {}
      - name: self-signed-ca
        secret:
          secretName: self-signed-ca
```

So, first things first, volumes: an empty volume called `truststore`
which we'll put our new and improved keystore-with-our-ssca. Also,
we'll need a volume for the self-signed CA itself. Our Ops provided it
for us in a secret with a key `ca.crt`, but you can get it into your
containers any way you want.

```
$ kubectl get secret self-signed-ca -o yaml --export
apiVersion: v1
data:
  ca.crt: ...
kind: Secret
metadata:
  name: self-signed-ca
type: Opaque
```

With the volumes in place, we need to set up init containers to do our
keytool work. I assume (not actually sure) that we need to add our
self-signed CA to the existing CAs, so we use one initContainer to
copy the existing default `cacerts` file into our `truststore` volume,
and another initContainer to run the `keytool` command. It's totally
fine to combine these into one container, but I didn't feel like
making a custom docker image with a shell script or having a super
long command line. So:

```
spec:
  template:
    spec:
      initContainers:
      - name: copy
        image: openjdk:8u121-alpine
        command: [ cp, /usr/lib/jvm/java-1.8-openjdk/jre/lib/security/cacerts, /ssca/truststore/cacerts ]
        volumeMounts:
        - name: truststore
          mountPath: /ssca/truststore

      - name: import
        image: openjdk:8u121-alpine
        command: [ keytool, -importcert, -v, -noprompt, -trustcacerts,
                   -file, /ssca/ca/ca.crt, -keystore, /ssca/truststore/cacerts,
                   -storepass, changeit ]
        volumeMounts:
        - name: truststore
          mountPath: /ssca/truststore
        - name: self-signed-ca
          mountPath: /ssca/ca
```

Mount the `truststore` volume in the `copy` initContainer, grab the
file cacerts file, and put it in our `truststore` volume. Note that
while we'd like to use $JAVA_HOME in the `copy` initContainer, I
couldn't figure out how to use environment variables in the
command. Also, since we're using a tagged docker image, there is a
pretty good guarantee that the filepath shouldn't change underneath
us, even though it's hardcoded.

Next, the import step! We need to mount the self-signed CA into this
container as well. Run the `keytool` command as described above,
referencing our copied `cacerts` file in our `truststore` volume and
passing in our ssCA.

Two things to note here: the `-noprompt` argument to `keytool` is
mandatory, or else `keytool` will prompt for interaction, but of
course the initContainer isn't running in a shell for someone to hit
`yes` in. Also, the mountPaths for these volumes should be separate
folders! I know Kubernetes is happy to overwrite existing directories
when a volume mountPath clashes with a directory on the image, and
since we have different data in our volumes, they can't be in the same
directory. (...probably, I didn't actually check)

The final step is telling the JVM where our new and improved trust
store is. My first idea was just to add `args` to the manifest and set
the system property in there, but if the Dockerfile `ENTRYPOINT` is
something like

```
java -jar whatever.jar
```

then we'd get a command like

```
java -jar whatever.jar -Djavax.net.ssl.trustStore=...
```

which would pass the option to the jar instead of setting a system
property. Plus, that wouldn't work at all if the `ENTRYPOINT` was a
shell script or something that wasn't expecting arguments.

After some searching, StackOverflow taught us about the `_JAVA_OPTIONS`
and `JAVA_TOOL_OPTIONS` [environment variables][javaOpts]. We can
append our trustStore to the existing value of these env vars, and
we'd be good to go!

```
spec:
  template:
    spec:
      containers:
      - image: your-app-image
        env:
          # make sure not to overwrite this when composing the yaml
        - name: _JAVA_OPTIONS
          value: -Djavax.net.ssl.trustStore=/ssca/truststore/cacerts
        volumeMounts:
        - name: truststore
          mountPath: /ssca/truststore

```

In our app that we use to construct the manifests, we check if the
developer is already trying to set _JAVA_OPTIONS to something, and make
sure that we append to the existing value instead of overwriting it.

### a conclusion of sorts

Uh, so that got kind of long, but the overall idea is more or less
straightforward. Add our self-signed CA to the existing cacerts file,
and tell the JVM to use it as the truststore. (Note that it's the
trustStore option you want, not the keyStore!). The entire Deployment
manifest all together [is also available][manifest], if that sounds
useful...

[docs]: http://mongodb.github.io/mongo-java-driver/3.6/driver/tutorials/ssl/#jvm-system-properties-for-tls-ssl
[manifest]: https://github.com/gempesaw/writing/blob/master/published/kubernetes-mongo-ssca-jvm.yaml
[javaOpts]: https://stackoverflow.com/questions/28327620/difference-between-java-options-java-tool-options-and-java-opts
