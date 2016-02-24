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
0.0.2 - public CREST cached by ets. Less list-processing.
### Demo
[0.0.2v].

### Usage
get [sqlite SDE], unpack

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
* *_handler - cowboy request handlers
* crest - pure api for crest
* pub_crest - pure api for public crest
* sov - store and give information about soverenity

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
 - Swich to maps based router state
 - More coments to code
 - Rework web template (probably, not before 4th March)
 - Rework ws timeout.

License
----

MIT

[sqlite SDE]: <https://www.fuzzwork.co.uk/dump/sqlite-latest.sqlite.bz2>
[0.0.2v]: <http://46.101.130.93/>
[The EVE Online API Challenge]:<http://community.eveonline.com/news/dev-blogs/the-eve-online-api-challenge-1/>


