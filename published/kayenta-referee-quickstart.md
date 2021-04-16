# Getting started with standalone Kayenta + Referee for Automated Canary Analysis

Automated canary analysis is a deploy strategy wherein you direct a
subset of your (production?) traffic to a canary version of your
application. The idea is that your baseline application and your
canary application will then be shipping metrics off to your metrics
provider, and Kayenta + Referee help you determine whether your new
code fails any metrics! This replaces developers manually looking at
dashboards & logs during deploys.

[Kayenta][] is an open source Java microservice for performing
automated canary analysis - it's just one microservice of many that
make up Netflix's [Spinnaker][] platform. [Referee][] is an
open-source React frontend that sits in front of Kayenta and allows
for rapid iteration of your canary configurations. This blog post will
outline the steps I took to get this set up at $WORK recently!

You can read through the details below in this blog post, or go
straight to the [kayenta-referee-quickstart repo][] to get started
right away.

## Configuring Kayenta

Starting up Kayenta consists mainly of configuring its three data
stores in the `kayenta.yml` config: `METRICS_STORE`,
`CONFIGURATION_STORE`, and `OBJECT_STORE`.

For the `METRICS_STORE`, choose the one that your applications is
shipping its metrics to. Kayenta supports Atlas, Google, Datadog,
graphite, New Relic, Prometheus, SignalFX, and Wavefront. You'll need
to obtain the appropriate API keys to allow Kayenta to query the
metrics API, which will depend on the datastore you're using. For
datadog, the config excerpt looks like:

```
  datadog:
    enabled: false
    metadataCachingIntervalMS: 900000
    accounts:
      - name: my-datadog-account
        apiKey: xxxx
        applicationKey: xxxx
        supportedTypes:
          - METRICS_STORE
        endpoint.baseUrl: https://app.datadoghq.com
```

For `CONFIGURATION_STORE` and `OBJECT_STORE`, you can start out with
the built-in `in-memory-store`, and this is already configured in the
quickstart repo. Later, you can switch that to S3/GCP/Azure if you need to
persist your configs/objects.

```
  memory:
    enabled: true
    accounts:
    - name: in-memory-store
      supportedTypes:
      - OBJECT_STORE
      - CONFIGURATION_STORE
```

## running kayenta + referee

Over in the kayenta-referee-quickstart Github repo, there are
dockerfiles and a `docker-compose.yml` that you can use to run both of
the microservices locally. Fill out your metrics store API keys in the
`kayenta.yml` config at the root of the repo, and then you can start
it up:

```
docker-compose up -d
```

The Kayenta API documentation will be available at
http://localhost:3001/swagger-ui.html, and Referee will be running at
http://localhost:8090/dashboard. If the containers don't come up,
check their logs for any telling messages:

```
docker-compose logs kayenta
docker-compose logs referee
```

If you decide you want to deploy Kayenta & Referee into your internal
infrastructure, you'll need to provide a Redis instance for Kayenta to
talk to. This will depend on your internal infrastructure, but in a
pinch, you could use a Docker container running Redis even without a
persistent data volume backing it, if you don't need to store the
Canary configurations inside Kayenta.

## integrating kayenta + referee into your production deploys

If you're embarking on the canary <--> production deploy integration
road, one of the most useful tips I came across was to start out with
an asynchronous canary pipeline separate from your existing production
pipeline. Have the canary pipeline run in parallel to your existing
production deploy, and don't let the canary pipeline block or even
fail deploys. Instead, the idea is to set up the canary pipeline with
a few metrics and have it gather data on every production deploy until
you can see whether the metrics you chose are appropriate and would
catch real production issues.

## canary configurations

Configuring a canary consists of constructing the POST body for the
standalone canary endpoint; there's probably enough material there for
its own blog post, so we'll defer that to a potential part two!


[Kayenta]: https://github.com/spinnaker/kayenta
[Spinnaker]: https://github.com/spinnaker
[Referee]: https://github.com/nikeinc/referee
[quickstart repo]: https://github.com/gempesaw/kayenta-referee-quickstart
