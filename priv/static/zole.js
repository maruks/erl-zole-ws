var websocket;
$(document).ready(init);

String.prototype.isEmpty = function() {
    return (this.length === 0 || !this.trim());
};

function init() {
    $('#server').val("ws://" + window.location.host + "/websocket");
    if(!("WebSocket" in window)){
        $('#status').append('<p><span style="color: red;">websockets are not supported </span></p>');
    } else {
        $('#status').append('<p><span style="color: green;">websockets are supported </span></p>');
        connect();
    };
    $("#connected").hide();
    $("#content").hide();
};

function connect()
{
    wsHost = $("#server").val()
    websocket = new WebSocket(wsHost);
    showScreen('<b>Connecting to: ' +  wsHost + '</b>');
    websocket.onopen = function(evt) { onOpen(evt) };
    websocket.onclose = function(evt) { onClose(evt) };
    websocket.onmessage = function(evt) { onMessage(evt) };
    websocket.onerror = function(evt) { onError(evt) };
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

function sendTxt(txt) {
    if(websocket.readyState == websocket.OPEN){
        websocket.send(txt);
        showScreen('sending: ' + txt);
    } else {
        showScreen('websocket is not connected');
    };
};

function onOpen(evt) {
    showScreen('<span style="color: green;">CONNECTED </span>');
    $("#connected").fadeIn('slow');
    $("#content").fadeIn('slow');
};

function onClose(evt) {
    showScreen('<span style="color: red;">DISCONNECTED </span>');
};

function onMessage(evt) {
    showScreen('<span style="color: blue;">RESPONSE: ' + evt.data+ '</span>');
    console.log(JSON.parse(evt.data));
};

function onError(evt) {
    showScreen('<span style="color: red;">ERROR: ' + evt.data+ '</span>');
};

function showScreen(txt) {
    $('#output').prepend('<p>' + txt + '</p>');
};

function clearScreen()
{
    $('#output').html("");
};

function login() {
    txt = $("#username").val();
    if (!txt.isEmpty()) {
	sendTxt(JSON.stringify(["login", txt]));
    }
};

function join() {
    txt = $("#tablename").val();
    if (!txt.isEmpty()) {
	sendTxt(JSON.stringify(["join", txt]));
    }
};

function play() {
    txt = $("#card").val();
    if (!txt.isEmpty()) {
	sendTxt(JSON.stringify(["play", JSON.parse(txt)]));
    }
};

function save() {
    txt = $("#two_cards").val();
    if (!txt.isEmpty()) {
	sendTxt(JSON.stringify(["save", JSON.parse(txt)]));
    }
};

function send(cmd) {
    if (!cmd.isEmpty()) {
	sendTxt(JSON.stringify([cmd]));
    }
};
