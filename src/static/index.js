require('./styles/main.scss');
require('../../node_modules/bootstrap-sass/assets/javascripts/bootstrap');

var Scriabin = require('../elm/Main');

window.ScriabinApp = Scriabin.Main.embed(document.getElementById('app'), {
  endpoint: window.localStorage.getItem('endpoint'),
  token: window.localStorage.getItem('token'),
});

window.Howl = require('howler').Howl;

var FPS = 5;
var DURATION_TRACKING_FREQUENCY = 1000 / FPS;

// PageTitle

window.ScriabinApp.ports.title.subscribe(function (string) {
  document.title = string;
});

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

        var _this = this;

        this.durationWorker = setTimeout(function () {
          _this.durationWorker = null;
          _this.trackDuration();
        }, DURATION_TRACKING_FREQUENCY);
      }
    }
  },

  setCurrentHowl: function (url, id) {
    h = new Howl({
      src: [url],
      format: ['mp3'],
      html5: true,
    });

    this.currentHowl = h;
  },

  play: function (url, id, time) {
    this.stop();

    this.setCurrentHowl(url, id);

    var h = this.currentHowl;

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
      setTimeout(function () { h.unload(); }, DURATION_TRACKING_FREQUENCY * 2);
    });
  },

  pause: function (url) {
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
    window.player.play(object.url, object.id, object.time);
  } else if (object.action == 'pause') {
    window.player.pause(object.url);
  } else if (object.action == 'stop') {
    window.player.stop();
  } else {
    throw 'u mad bro';
  }
});
