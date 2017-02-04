require('./styles/main.scss');
require('../../node_modules/bootstrap-sass/assets/javascripts/bootstrap');

var Scriabin = require('../elm/Main');

window.ScriabinApp = Scriabin.Main.embed(document.getElementById('app'));

window.ScriabinApp.ports.webAudioControl.subscribe(function (object) {
  if (object.action == 'play') {
    window.player.play(object.url, object.time, object.id);
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
  offset: 0,
  durationWorker: null,

  sync: function (state) {
    window.ScriabinApp.ports.webAudio.send(state);
  },

  startDurationWorker: function () {
    var _this = this;
    this.durationWorker = setTimeout(function () {
      if (_this.startedAt) {
        window.ScriabinApp.ports.webAudio.send({
          state: 'playing',
          offset: audioContext.currentTime - _this.startedAt,
        });
        _this.startDurationWorker();
      }
    }, 500);
  },

  connectAndStart: function (id, time) {
    this.audioSource = audioContext.createBufferSource();
    this.audioSource.connect(audioContext.destination);
    this.audioSource.buffer = this.buffers[id];
    this.audioSource.start(0, time);
    this.startedAt = audioContext.currentTime - time;

    this.startDurationWorker();
  },

  destroySource: function () {
    if (this.audioSource) {
      this.audioSource.disconnect();
      this.audioSource.stop();
      this.audioSource = null;
    }
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
    this.destroySource();
    this.offset = 0;
    this.startedAt = null;
  },

  pause: function (url, id) {
    this.destroySource();
    this.load(url, id);

    this.offset = audioContext.currentTime - this.startedAt;
    this.startedAt = null;
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
