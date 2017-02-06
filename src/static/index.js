require('./styles/main.scss');
require('../../node_modules/bootstrap-sass/assets/javascripts/bootstrap');

var Scriabin = require('../elm/Main');

window.ScriabinApp = Scriabin.Main.embed(document.getElementById('app'));

var DURATION_TRACKING_FREQUENCY = 500;
var PRELOAD_THRESHOLD = 5;

window.ScriabinApp.ports.webAudioControl.subscribe(function (object) {
  console.log('Nature!!!', object);
  if (object.action == 'play') {
    window.player.play(object.url, object.time, object.id);
    window.player.next = object.next;
  } else if (object.action == 'pause') {
    window.player.pause(object.url, object.id);
  } else if (object.action == 'stop') {
    window.player.stop();
  } else {
    throw 'u mad bro';
  }
});

AudioContext = window.AudioContext || window.webkitAudioContext;
audioContext = new AudioContext();

window.player = {
  audioSource: null,
  buffers: {},
  startedAt: null,
  willEndAt: null,
  offset: 0,
  durationWorker: null,
  next: null,

  sync: function (state) {
    window.ScriabinApp.ports.webAudio.send(state);
  },

  trackDuration: function () {
    var currentTime = audioContext.currentTime;

    if (this.startedAt && this.willEndAt > currentTime) {
      window.ScriabinApp.ports.webAudio.send({
        state: 'playing',
        offset: currentTime - this.startedAt,
      });

      if (this.next && this.willEndAt - currentTime <= PRELOAD_THRESHOLD) {
        this.load(this.next.url, this.next.id);
        this.next = null;
      }

      var _this = this;

      this.durationWorker = setTimeout(function () {
        _this.trackDuration();
      }, DURATION_TRACKING_FREQUENCY);
    }
  },

  connectAndStart: function (id, time) {
    this.audioSource = audioContext.createBufferSource();
    this.audioSource.connect(audioContext.destination);
    this.audioSource.buffer = this.buffers[id];
    this.audioSource.start(0, time);

    var _this = this;
    this.audioSource.onended = function () {
      _this.sync({
        state: 'finished',
      });
    };

    this.startedAt = audioContext.currentTime - time;
    this.willEndAt = this.startedAt + this.buffers[id].duration;

    this.trackDuration();
  },

  destroySource: function () {
    if (this.audioSource) {
      this.audioSource.disconnect();
      this.audioSource.stop();
      this.audioSource = null;
    }

    if (this.durationWorker) {
      clearTimeout(this.durationWorker);
      this.durationWorker = null;
    }

    this.startedAt = null;
    this.willEndAt = null;
  },

  load: function (url, id, then) {
    then = then || function () {};

    if (this.buffers[id]) {
      then();
    } else {
      var request = new XMLHttpRequest();
      var _this = this;

      request.open('GET', url, true);
      request.responseType = 'arraybuffer';
      request.onload = function () {
        audioContext.decodeAudioData(request.response, function (buffer) {
          _this.buffers[id] = buffer;
          then();
        });
      };

      request.send();
    }
  },

  stop: function () {
    this.offset = 0;
    this.destroySource();
  },

  pause: function (url, id) {
    this.offset = audioContext.currentTime - this.startedAt;
    this.destroySource();
    this.load(url, id);

    this.sync({
      state: 'paused',
      offset: this.offset,
    });
  },

  play: function (url, time, id) {
    var _this = this;
    this.destroySource();
    this.load(url, id, function () { _this.connectAndStart(id, time); });
  },
};
