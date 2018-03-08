var endpoint = localStorage.getItem('endpoint');

if (endpoint) {
  console.log('Migrating endpoint to host:port');

  localStorage.removeItem('endpoint');
  var match = endpoint.match(/^https?\:\/\/([^\/:?#]+)(?::(\d+))?(?:[\/:?#]|$)/i);
  localStorage.setItem('host', match[1]);
  localStorage.setItem('port', match[2]);
}
