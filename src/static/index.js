require('./styles/main.scss');
require('../../node_modules/bootstrap-sass/assets/javascripts/bootstrap');

var Scriabin = require('../elm/Main');

window.ScriabinApp = Scriabin.Main.embed(document.getElementById('app'));

window.ScriabinApp.ports.webAudioControl.subscribe(function (object) {
  if (object.action == 'play') {
    loadSound(object.url, object.time);
  }
});

AudioContext = window.AudioContext || window.webkitAudioContext;

window.audioContext = new AudioContext();

loadSound = function (url, time) {
  if (window.audioSource) {
    window.audioSource.stop();
  }

  var request = new XMLHttpRequest();

  request.open('GET', url, true);
  request.responseType = 'arraybuffer';
  request.onload = function () {
    window.audioContext.decodeAudioData(request.response, function (buffer) {
      window.audioSource = window.audioContext.createBufferSource();
      window.audioSource.buffer = buffer;
      window.audioSource.connect(window.audioContext.destination);
      window.audioSource.start(time);
    });
  };

  request.send();
};
