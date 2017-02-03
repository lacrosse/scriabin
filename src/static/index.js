require('./styles/main.scss');
require('../../node_modules/bootstrap-sass/assets/javascripts/bootstrap');

var Scriabin = require('../elm/Main');

window.scriabin = Scriabin.Main.embed(document.getElementById('app'));

window.scriabin.ports.webAudioControl.subscribe(function (object) {
  if (object.action == 'play') {
    console.log('Playing ' + object.url);
  }
});

AudioContext = window.AudioContext || window.webkitAudioContext;
var audioContext = new AudioContext();

window.loadSound = function (url) {
  var request = new XMLHttpRequest();
  request.open('GET', url, true);
  request.responseType = 'arraybuffer';

  var buffer;

  request.onload = function () {
    audioContext.decodeAudioData(request.response, function (buffer) {
      window.audioSource = audioContext.createBufferSource();
      window.audioSource.buffer = buffer;
      window.audioSource.connect(audioContext.destination);
      window.audioSource.start(0);
    });
  };

  request.send();
};
