var Scriabin = require('../elm/Main');

window.ScriabinApp = Scriabin.Main.embed(document.getElementById('app'), {
  endpoint: window.localStorage.getItem('endpoint'),
  token: window.localStorage.getItem('token'),
});

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

window.Howl = require('howler').Howl;

var FPS = 5;
var DURATION_TRACKING_FREQUENCY = 1000 / FPS;

window.player = {
  currentHowl: null,
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

  setNewCurrentHowl: function (url, id) {
    h = new Howl({
      src: [url],
      format: ['mp3'],
      html5: true,
    });

    this.currentHowl = h;
  },

  play: function (url, id, time) {
    this.unloadCurrentHowl();

    this.setNewCurrentHowl(url, id);

    var h = this.currentHowl;

    if (time > 0) {
      h.seek(time);
    }

    h.play();

    var _this = this;

    h.once('play', function () {
      _this.trackDuration();
    });

    h.once('end', function () {
      _this.sync({
        state: 'finished',
      });
      _this.unloadHowl(h);
    });
  },

  pause: function (url, time) {
    var h = this.currentHowl;

    h.pause();

    h.seek(time);

    this.sync({
      state: 'paused',
      offset: h.seek(),
      progress: h.duration(),
    });
  },

  stop: function () {
    this.unloadCurrentHowl();
  },

  unloadCurrentHowl: function () {
    var h = this.currentHowl;

    if (h) {
      h.stop();
      this.unloadHowl(h);
    }

    this.currentHowl = null;
  },

  unloadHowl: function (h) {
    setTimeout(function () {
      h.unload();
    }, DURATION_TRACKING_FREQUENCY * 2);
  },
};

window.ScriabinApp.ports.webAudioControl.subscribe(function (object) {
  console.log('webAudioControl received', object);
  if (object.action == 'play') {
    window.player.play(object.url, object.id, object.time);
  } else if (object.action == 'pause') {
    window.player.pause(object.url, object.time);
  } else if (object.action == 'stop') {
    window.player.stop();
  } else {
    throw 'u mad bro';
  }
});
