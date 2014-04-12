Facebook Listener
==========================

Facebook Listener represents a good way to consume [`Facebook Realtime Updates`](https://developers.facebook.com/docs/graph-api/real-time-updates/ "Facebook Realtime Updates").

Realtime updates are a good solution to be notified when your Facebook application users do some activities on Facebook. So far only `likes` and `feed` are supported by Facebook Listener.

Whenever a Realtime Update is received from Facebook, the application will retrieve the original content using the `user token` stored in the db specified in your implementation of the Facebook Listener (default is `repo_mem.erl` which has an hardcoded value). Once the original content is retrieved, Facebook Listener will push the content to a destination queue (default is `queue_mem.erl`).

### Setup a minimal installation:

It is really easy to setup Facebook Listener in order to receive real time updates, you just need to customize `prod.config` with your Facebook Application Secret and build the application in order to get a working Erlang release.

To build the application as an Erlang release, run the following command:

``` bash
$ make
```

This will generate a release that works with an the aforesaid implementation of users repository db in memory (`repo_mem.erl`) and destination queue (`queue_mem.erl`). You can implement your own versions of both and ovveride them using the `prod.config` file.

### Run the realease:

In order to run the release in foreground you can do:

``` bash
$ ./start.sh
```

You can also start/stop the release using:

``` bash
$ ./_rel/bin/facebook_listener [start|stop]
```

### Run the tests:


To run the tests:

``` bash
$ make tests
```
