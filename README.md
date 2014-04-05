REFERENCE APPLICATION
==========================

To build the application, run the following command:

``` bash
$ make
```

Notice that you may want to set the application to use your own Facebook application secret.
In this case, before running make command, you have to copy prod.config to yourfile.config, customize it and then change the file relx.config accordingly.

To run the tests:

``` bash
$ make tests
```

To start the release in the foreground:

``` bash
$ ./start.sh
```
