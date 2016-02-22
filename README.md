# Scout Probes Launcher

Scout Probe is a service writen with Erlang, based on:

  - EVE CREST API
  - EVE Static export
  - EVE Online Image server
  - Zkillboard API

Not fuly OTP, have no idea how to do OTP tracker without rewrting gen_server loop. 
No https. It's prety easy to enable https at cowboy by yourself.
Created for [The EVE Online API Challenge].

### Version
0.0.1 - result of 2 days full of coding.
### Demo
[0.0.1v].

### Usage

get dependencies, compile, run shell
```sh
$ ./rebar get-deps
$ ERL_LIBS=$ERL_LIBS:deps/cowboy make
$ make run
```
For noshel startup edit makefile.


### Modules

Like any app based on erlang, Scout Probe has several modules:

* tracker - used to track user througth new eden
* router - spawned by router_sup for routing all events
* Google Drive
* OneDrive

### Documentation
Due to time limit, there is no documentation yet. You can read sources. 

### CREST Usage

Service is using "new" crest endpoints. 

 - **characterLocationRead** - nice working endpoint with one anoying bug|feature. If pilot route looks like System A->System B->System A endpoint result going to be "System B" for a while.
 - **characterNavigationWrite**

### Public CREST Usage

 - **alliances**
 - **solarsystems**
 - **sovereignty/campaigns**

### Todos

 - Delete rudiment code
 - More coments to code
 - Rework web template
 - Rework ws timeout.

License
----

MIT

[0.0.1v]: <http://46.101.130.93/>
[The EVE Online API Challenge]:<http://community.eveonline.com/news/dev-blogs/the-eve-online-api-challenge-1/>


