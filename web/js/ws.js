/*
%%%-------------------------------------------------------------------
%%% Copyright © 2016 Redpate
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%-------------------------------------------------------------------
*/
var websocket;
var events={upcoming:[], staging:[], active:[]};
var probeid="";
function init() {
        if(!("WebSocket" in window)){
            $('#status').append('<p><span style="color: red;">websockets are not supported </span></p>');
        } else {
            connect();
        };
};
function connect()
  {
        wsHost = "ws://" + window.location.host + "/tracker";
        websocket = new WebSocket(wsHost);
        websocket.onopen = function(evt) { onOpen(evt) };
        websocket.onclose = function(evt) { onClose(evt) };
        websocket.onmessage = function(evt) { onMessage(evt) };
    };

    function disconnect() {
        websocket.close();
    };
    function toggle_connection(){
        if(websocket.readyState == websocket.OPEN){
            disconnect();
        } else {
            connect();
        };
    };
    function sendTxt() {
        if(websocket.readyState == websocket.OPEN){
            txt = "and";
            websocket.send(txt);
            showScreen('sending: ' + txt);
        } else {
             showScreen('websocket is not connected');
        };
    };
    function onOpen(evt) {
        timeoutLoop();
    };
    function onClose(evt) {
        //showScreen('DISCONNECTED');
    };
    function onMessage(evt) {

      var o = jQuery.parseJSON( evt.data );
      if (o.location) {
        $('#location').text(o.location.name);
        $('#location').attr("href", "http://evemaps.dotlan.net/system/"+o.location.name);
      }
      if (o.jumped_in) {
        $('#start_point').text(o.jumped_in.name);
        $('#start_point').attr("href", "http://evemaps.dotlan.net/system/"+o.jumped_in.name);
        $('#start_point_w').html("<a onclick=\"websocket.send('D:"+o.jumped_in.id_str+"');\" >Set Destination</a> | <a onclick=\"websocket.send('W:"+o.jumped_in.id_str+"');\" >Set Waypoint</a>");
      }
      if (o.msg_type==0) {
        if ($('#events > tbody>tr').length >=15) {
            $('#events > tbody>tr').last().remove();
          }
        $("#events > tbody").prepend("<tr><td>"+o.system.name+"</td><td>"+o.text+"(<a target=\"_blank\" href=\"https://zkillboard.com/kill/"+o.id+"/\">killmail</a>)</td><td>"+o.time+"</td><td><a onclick=\"websocket.send('D:"+o.system.id_str+"');\" >Set Destination</a> | <a onclick=\"websocket.send('W:"+o.system.id_str+"');\" >Set Waypoint</a></td></tr>");
      } else if (o.msg_type==1){
        if (o.capital_id) { //defined staging system
           render ("staging", o);
        }
        if (o.upcoming){
          for (var i =1;i<5; i++){
            if (o.upcoming[i]){
            render('upcoming', o.upcoming[i])}
            };
          }
        if (o.active){
            for (var i =1;i<5; i++){
              if (o.active[i]){
              render('active', o.active[i])}
            };
        }
      }
    };

$(document).ready(function() {
  init();
});

function timeoutLoop () {
  websocket.send("and");
  setTimeout(function () {
    timeoutLoop();
  }, 120000)
}

function render (type, event){

  if (type=="staging"){
    if (-1!==$.inArray(event.capital_id, events[type])){
      $("#staging>div.small-6#"+event.capital_id).remove();
    } else {
      $("#staging>div.small-6").last().remove();
      events[type].push(event.capital_id);
    }
    $("#staging>div.small-6").first().before("<div id='"+event.capital_id+"' class='large-3 small-6 columns'><img title='"+event.capital_alliance_name+"' src='https://image.eveonline.com/Alliance/"+ event.capital_alliance + "_128.png'><div class='panel'><p>"+ event.capital_name + " Set (<a onclick='websocket.send(\"D:"+event.capital_id+"\")''>destination</a> or <a onclick='websocket.send(\"W:"+event.capital_id+"\")''>waypoint</a>)</div></div></p></div></div>");
  } else {
    var src = "https://placeholdit.imgix.net/~text?txtsize=30&txt=Undefined&w=200&h=200&fm=png";
    if (event.defender && event.defender.defender){
      src = "https://image.eveonline.com/Alliance/"+ event.defender.defender.id + "_128.png"
    }
    if (-1!==$.inArray(event.campaignID, events[type])){
      $("#"+type+">div.small-6#"+event.campaignID).remove();
    } else {
      $("#"+type+">div.small-6").last().remove();
      events[type].push(event.campaignID);
    }
    $("#"+type+">div.small-6").first().before("<div id='"+event.campaignID+"' class='large-3 small-6 columns'><img src='"+ src +
        "'><div class='panel'><p><a  target='_blank'  href='https://timerboard.net/"+event.sourceSolarsystem.name+"'>"+ event.sourceSolarsystem.name + "</a></p>"+
        " Set (<a onclick='websocket.send(\"D:"+event.sourceSolarsystem.id+"\")'>destination</a> or <a onclick='websocket.send(\"W:"+event.sourceSolarsystem.id+"\")''>waypoint</a>)</div></div>");
  }
};

//tracker:send(94219353, {sov,<<"30000503">>}).
