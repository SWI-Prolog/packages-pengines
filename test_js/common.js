exports.getPenginePort = function() {
  var port = parseInt(process.argv[2], 10);
  if (isNaN(port)) {
    process.error('Pengine server port is not set.');
    process.exit(1);
  }
  return port;
};
