require('./styles/main.scss');
require('../../node_modules/bootstrap-sass/assets/javascripts/bootstrap');

var Scriabin = require('../elm/Main');

window.Howl = require('howler').Howl;

window.ScriabinApp = Scriabin.Main.embed(document.getElementById('app'), {
  token: window.localStorage.getItem('token'),
});

var FPS = 5;
var DURATION_TRACKING_FREQUENCY = 1000 / FPS;
var PRELOAD_THRESHOLD = 10;

// LocalStorage

window.ScriabinApp.ports.localStorage.subscribe(function (object) {
  if (object.action == 'set') {
    window.localStorage.setItem(object.key, object.value);
  } else if (object.action == 'get') {
    window.ScriabinApp.ports.localStorageFeedback.send({
      key: object.key,
      value: window.localStorage.getItem(object.key),
    });
  } else if (object.action == 'remove') {
    window.localStorage.removeItem(object.key);
  } else {
    throw 'u mad bro';
  }
});

// Audio

window.player = {
  howls: {},
  currentHowl: null,
  next: null,
  durationWorker: null,

  sync: function (state) {
    window.ScriabinApp.ports.webAudioFeedback.send(state);
  },

  trackDuration: function () {
    var h = this.currentHowl;

    if (!this.durationWorker && h) {
      var seek = h.seek();
      var duration = h.duration();
      var remaining = duration - seek;

      if (h.playing() && remaining > 0) {
        this.sync({
          state: 'playing',
          offset: seek,
          progress: duration,
        });

        if (this.next && remaining <= PRELOAD_THRESHOLD) {
          player.buildHowl(this.next.url, this.next.id);
          this.next = null;
        }

        var _this = this;

        this.durationWorker = setTimeout(function () {
          _this.durationWorker = null;
          _this.trackDuration();
        }, DURATION_TRACKING_FREQUENCY);
      }
    }
  },

  buildHowl: function (url, id) {
    var h = this.howls[id];

    if (!h) {
      h = new Howl({
        src: [url],
        format: ['mp3'],
        html5: true,
      });

      this.howls[id] = h;
    }

    return h;
  },

  play: function (url, id, time) {
    this.stop();

    var h = this.buildHowl(url, id);

    this.currentHowl = h;
    h.seek(time);
    h.play();

    var _this = this;

    h.once('play', function () {
      _this.trackDuration();
    });

    h.once('end', function () {
      _this.sync({
        state: 'finished',
      });
    });
  },

  pause: function (url, id) {
    var h = this.currentHowl;

    h.pause();

    this.sync({
      state: 'paused',
      offset: h.seek(),
      progress: h.duration(),
    });
  },

  stop: function () {
    var h = this.currentHowl;

    if (h) h.stop();

    this.currentHowl = null;
  },
};

window.ScriabinApp.ports.webAudioControl.subscribe(function (object) {
  if (object.action == 'play') {
    player.play(object.url, object.id, object.time);
    player.next = object.next;
  } else if (object.action == 'pause') {
    window.player.pause(object.url, object.token, object.id);
  } else if (object.action == 'stop') {
    window.player.stop();
  } else {
    throw 'u mad bro';
  }
});
